import pandas as pd
from pandas.tseries.offsets import YearBegin, MonthBegin
from dateutil.relativedelta import *
import cx_Oracle as cxo
import matplotlib.pyplot as plt
import matplotlib.dates as mdate
import matplotlib.ticker as ticker
import seaborn as sns
import locale
import re

## Encoding cp949로 저장

## Class Data Loading
## Class Chart
## Class Table
b
locale.setlocale(locale.LC_ALL, 'korean')

plt.rcParams['font.family'] = 'KopubDotum'
plt.rcParams['font.size'] = 14
plt.rcParams['axes.unicode_minus'] = False

locale.setlocale(locale.LC_ALL, 'korean')

ed_date='20180630'

class sql_load:

    def __init__(self, date=ed_date):

        ## Date - ed_date로 지정 및 데이터 베이스 연결
        self.date = date
        self.conn = cxo.connect(user='******', password='******', dsn='******')

## SQL Date Changer

    def st_date_Creator(self):

        ## SQL WHERE 절 내 시작일 만들어주기 위한 함수
        st_date = pd.to_datetime(self.date) + pd.DateOffset(years=-1) + MonthBegin()
        adj_st_date = st_date + pd.DateOffset(days=- 10)

        ans = {'st_date': st_date.strftime('%Y%m%d'),
               'adj_st_date': adj_st_date.strftime('%Y%m%d')}

        return ans

    def sql_date_qry(self):

        ## SQL WEHRE TRD_DT BETWEEN ~ 절 만들어주는 함수
        date_dict = self.st_date_Creator()
        st_date = date_dict['st_date']
        adj_st_date = date_dict['adj_st_date']

        st_dt_qry = 'between ' + st_date + ' and ' + self.date
        ed_dt_qry = 'between ' + adj_st_date + ' and ' + self.date

        ans = {'st_dt_qry': st_dt_qry,
               'adj_dt_qry': ed_dt_qry}

        return ans

    def sql_date_change(self, query):

        ## SQL WEHRE TRD_DT BETWEEN ~ 절 Date 바꿔주기 위한 함수
        st_dt_qry = self.sql_date_qry()['st_dt_qry']
        ed_dt_qry = self.sql_date_qry()['adj_dt_qry']

        chg_1 = re.sub("BETWEEN '20170430' AND '20180430'", st_dt_qry, string=query)
        chg_2 = re.sub("BETWEEN '20170321' AND '20180430'", ed_dt_qry, string=chg_1)

        return chg_2

    def sql_reader(self, query, cond=True):

        ## SQL을 읽어들이기 위한 함수. INDEX-TRD_DT 맞춰줌
        ## Cond Is True then sql_date_change로 변경
        ## Cond Is Not True then query 사용
        if cond == True:
            qry = self.sql_date_change(query)
        elif cond == False:
            qry = query

        data = pd.read_sql(qry, self.conn)
        data['TRD_DT'] = pd.to_datetime(data['TRD_DT'])

        return data.set_index('TRD_DT')

    def bday_date_Creator(self):

        ## bday_finder를 위한 Helper 함수

        str_1 = "BETWEEN "
        dt_1 = (pd.to_datetime(self.date) - relativedelta(days=20)).strftime('%Y%m%d')
        str_2 = " AND "
        dt_2 = self.date

        return str_1 + dt_1 + str_2 + dt_2


    def bday_finder(self):

        ## 위탁펀드 쿼리를 위해 영업일을 찾아주는 갓함수
        dt_qry = """
                select TRD_DT
                from fnc_calendar@D_FNDB2_UFNGDBA
                where 1=1 
                      AND TRD_DT BETWEEN '20180410' AND '20180430'
                      AND HOLIDAY_YN = 'N'
                ORDER BY TRD_DT
                    """

        sql_qr = re.sub("BETWEEN '20180410' AND '20180430'",
                        self.bday_date_Creator(),
                        dt_qry)

        return (pd.read_sql(sql_qr, self.conn)).iloc[-1]['TRD_DT']

    def fnd_sql_qry(self, query):
        sql_qry = re.sub('20180430', self.bday_finder(), query)
        ans = pd.read_sql(sql_qry, self.conn)

        return ans

## SQL 쿼리 가져올 때, GDP Query / 1000 , Amount P_Quarter / 100

class plot:

    def __init__(self, data):

        self.fig, self.ax = plt.subplots()
        self.data = data
        self.index = self.data.index
        self.columns = self.data.columns

    def size_cent_to_inches(self, size):

        ## fig.set_size~함수는 Inch로 표기. 이를 센티미터로 바꿔줌

        row = size[0] / 2.58
        col = size[1] / 2.58

        return row, col

    def fig_set_size_centimeter(self, size):

        ## fig.set_size~함수는 Inch로 표기. 이를 센티미터로 바꿔줌

        self.fig.set_size_inches(self.size_cent_to_inches(size))

        return self.fig, self.ax

    def twin_x(self):

        ## 새로운 ax를 생성함

        return self.ax.twinx()

    def esc_option(self):

        ## 모든 Plot에 적용되는 공통옵션

        self.ax.minorticks_off() # Minor tick 제거
        self.ax.set_xlabel('') # xlabel 제거
        self.ax.set_ylabel('') # ylabel 제거
        self.ax.legend(loc='upper center', ncol= len(self.columns),
                       bbox_to_anchor = (0.5, 1.18), frameon=False) # legend(범주) 변경
        self.ax.margins(x=0) # margin 0으로 지정

## Bar Graph tick

    def gdp_tick(self):

        # 본격 GDP xtick 만드는 함수

        lt_tick = [i for i in range(0, 8)]
        ticks = [str(y)[2:] + "." + str(int(m / 3)) + "Q" for y, m in zip(self.index.year, self.index.month)]

        self.ax.set_xticks(lt_tick) # xtick 들어갈 자리 만들어 놓긔
        self.ax.set_xticklabels(ticks) # 각 xtick 이름

        return self.fig, self.ax

    def bar_tick(self):

        # Bar grpah Tick 생성함수

        tick_n = self.data.shape[0]
        lt_tick = [i for i in range(1, tick_n, 2)]
        lt_label = self.index.strftime('%y.%B')[lt_tick]

        self.ax.set_xticks(lt_tick)
        self.ax.set_xticklabels(lt_label, rotation=0)
        # plt.xticks(rotation=0)

        return self.fig, self.ax

    ## Bar Graph tick
    def date_tick_creator(self, frq):

        items = pd.date_range(self.index[0], self.index[-1], freq = frq)
        tick_lbs = [item.strftime('%y.%B') for item in items]

        return tick_lbs

    def date_tick(self, frq):

        ## 시계열 그래프 Date Tick

        tick_l = self.date_tick_creator(frq)
        self.ax.xaxis.set_major_locator(mdate.MonthLocator())
        self.ax.xaxis.set_major_formatter(ticker.FixedFormatter(tick_l))
        plt.xticks(rotation=0)

        return self.ax

    def ax_legend(self, ax1):

        # ax가 두개일 때(yaxis 가 양면 두개 일 때 Legend 만들어주는 함수)

        plots, labels = self.ax.get_legend_handles_labels()
        plots2, labels2 = ax1.get_legend_handles_labels()

        self.ax.legend().set_visible(False)
        ax1.legend(plots + plots2, labels + labels2,
                                    loc='upper center', ncol= len(self.columns)+1
                                    , bbox_to_anchor = (0.5, 1.18), frameon=False)

    def ax_one_legend(self):

        # ax가 하나일 때 legend 만들어주는 함수

        l = self.ax.legend(loc='upper center', ncol = len(self.columns) + 1, bbox_to_anchor=(0.5, 1.18), frameon=False)
        l.set_title('')

        return self.ax

    def gdp_plot(self, size):


        self.fig_set_size_centimeter(size)

        data = self.data.reset_index()
        data['POS'] = data['전기대비'] > 0 ## 양 / 음일때 색깔 만들어주기 위한 준비
        ax1 = self.twin_x()

        an_x = 7
        an_y = round(data['GDP'].iloc[-1]) / 1000

        (data['GDP'].div(1000)).plot(ax=self.ax, color='red', lw=3, legend=True)
        self.ax.annotate(str(int(an_y)), xy=(an_x, an_y), xytext=(an_x, (an_y + 0.5)))

        (data['전기대비']).plot(ax=ax1, kind='bar', alpha=0.5, legend=True, rot=0,
                              color=data['POS'].map({True: 'g', False: 'r'}))

        # ax1.set_ylim(bottom=data['전기대비'].min() - 0.02, top=data['전기대비'].max() + 0.1)
        self.y_aestatic('$')
        self.right_y_aestatic(ax1, '%')
        self.esc_option()
        self.ax_legend(ax1=ax1)
        self.gdp_tick()

        return self.fig, self.ax, ax1

    def y_aestatic(self, num_type):

        ## 본격 yaxis 표기 바꿔주는 함수

        if num_type == '%':
            return self.ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:.2%}'.format(y)))
        elif num_type == '$':
            return self.ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:,}'.format(int(y))))
        elif num_type == 'f':
            return self.ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:.2f}'.format(y)))
        else:
            print("Type among %, $, f")

    def two_y_aestatic(self, ax_one, num_type):

        ## 본격 yaxis 2개 동이랗게

        if num_type == '%':

            for ax in [self.ax, ax_one]:
                ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:.2%}'.format(y)))
            return self.ax, ax_one

        elif num_type == '$':
            for ax in [self.ax, ax_one]:
                ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:,}'.format(int(y))))
            return self.ax, ax_one

        elif num_type == 'f':
            for ax in [self.ax, ax_one]:
                ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:.2f}'.format(y)))
            return self.ax, ax_one

        else:
            print("Type among %, $, f")

    def right_y_aestatic(self, ax_one, num_type):

        if num_type == '%':
            return ax_one.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:.2%}'.format(y)))
        elif num_type == '$':
            return ax_one.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:,}'.format(int(y))))
        elif num_type == 'f':
            return ax_one.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:.2f}'.format(y)))
        else:
            print("Type among %, $, f")

    def mmf_chg_plt(self, size):

        dt = self.data.reset_index()

        dt['POS'] = dt['MMF'] > 0

        self.fig_set_size_centimeter(size)

        dt['MMF'].plot(kind='bar', ax=self.ax, rot=0,color=dt['POS'].map({True: 'g', False: 'r'}), width=0.8)
        self.ax.set_xticklabels(dt['TRD_DT'].dt.strftime('%y.%B'))
        self.ax.get_yaxis().set_major_formatter(ticker.FuncFormatter(lambda x, p: format(int(x), ',')))

        self.esc_option()
        self.ax.legend().set_visible(False)
        self.bar_tick()

        return self.fig, self.ax


    def cpi_plot(self, size):

        self.fig_set_size_centimeter(size)

        data = self.data.reset_index()
        ax1 = self.twin_x()

        an_x = (data.iloc[-1]).name
        an_y = data['소비자물가'].iloc[-1]

        data['소비자물가'].plot(ax=self.ax, color='red', lw=3, legend=True)

        self.ax.annotate(str(an_y), xy=(an_x, an_y), xytext=(an_x - 0.5, (an_y + 0.05)))

        data['전년동월'].plot(ax=ax1, kind='bar', color='green', alpha=0.5, legend=True, rot=0)

        ax1.set_ylim([data['전년동월'].min() - 1, data['전년동월'].max() + 2])

        self.esc_option()
        self.ax_legend(ax1=ax1)
        self.bar_tick()

        return self.fig, self.ax, ax1

    def exp_plot(self, size):

        self.fig_set_size_centimeter(size)

        data = self.data[['국가별수출액', '국가별수입액', '무역수지']]\
                   .sort_index()\
                   .div(1000)\
                   .reset_index()

        data['무역수지'].plot(ax=self.ax, legend=True, kind='bar', color='green', alpha=0.5)
        data['국가별수출액'].plot(ax=self.ax, lw=3, legend=True, ls ='-.')
        data['국가별수입액'].plot(ax=self.ax, lw=3, legend=True, ls ='-')

        self.ax.set_xlim(left=-0.5, right=11.5)
        self.ax.set_ylim(bottom=data['무역수지'].min()-10, top=data['국가별수출액'].max() + 1)

        self.esc_option()
        self.bar_tick()
        # self.ax.legend()

        return self.fig, self.ax

    def dol_graph(self, size, frq):

        self.fig_set_size_centimeter(size)
        ax1 = self.twin_x()

        self.data['원달러'].plot(ax=self.ax, color = 'coral', legend=True, lw=3)
        self.data['달러인덱스'].plot(ax=ax1, color = 'green', legend=True, lw=3, ls='-.')

        self.date_tick(frq = frq)
        self.ax.get_yaxis().set_major_formatter(ticker.FuncFormatter(lambda x, p: format(int(x), ',')))
        self.esc_option()
        self.ax_legend(ax1=ax1)


        return self.fig, self.ax, ax1

    def stk_plot(self, size, cols, frq, date=ed_date):

        std_dt = pd.to_datetime(date) + YearBegin(-1)
        line_style = ['-', '-.', '--']

        for col, ls in zip(cols, line_style):

            plt_dt = (self.data[col]\
                      .div(self.data[col].loc[std_dt]))\
                      .div(1 / 100)

            plt_dt.plot(ax=self.ax, legend=True, lw=3, ls = ls)

        # plt_dt = (self.data[cols]\
        #           .div(self.data[cols].loc[std_dt], axis=1))\
        #           .div(1 / 100)

        self.fig_set_size_centimeter(size)

        # self.ax_one_legend()

        self.ax.axvline(x=std_dt, color='red', lw=2)
        self.esc_option()
        self.date_tick(frq)

        return self.fig, self.ax

    def net_buy_plot(self, size):

        data = self.data.reset_index()
        data['순매수'] = data['순매수'] / 10 ** 6

        self.fig.set_size_inches(size)
        sns.barplot(data=data, x='TRD_DT', y='순매수',
                    hue='CD_NM', ax=self.ax)

        self.esc_option()
        self.ax.set_xticklabels(data['TRD_DT'].drop_duplicates().dt.strftime('%y.%B'),
                                rotation=0)
        self.ax.legend(loc='upper center', ncol=3, bbox_to_anchor=(0.5, 1.18), frameon=False)

        return self.fig, self.ax

    def kr_bd_st_plt(self, size, frq):

        self.fig_set_size_centimeter(size)
        dt = pd.to_datetime(ed_date) - relativedelta(years=1)

        # self.data.loc[dt:].plot(ax=self.ax, legend=True, lw=3)

        cols = self.data.columns
        ncols = len(cols)
        lin_style = ['-', '--', '-.', '--', '-.', '-','-.', '--', '-'][:ncols]

        for col, ls in zip(cols, lin_style):

            self.data.loc[dt:][col].plot(ax=self.ax, legend=True, lw=3, ls=ls)

        self.ax_one_legend()
        self.esc_option()
        self.PercFormatter()
        self.date_tick(frq)

        return self.fig, self.ax

    def yld_data(self, date, lt = ['국고_3Y', '국고_5Y', '국고_10Y', '국고_20Y', '국고_30Y']):

        lt_dt = pd.to_datetime(date)  ## 현재일

        thrm_dt = lt_dt - relativedelta(months=3) + MonthBegin(0)  ## 3개월전
        sxm_dt = lt_dt - relativedelta(months=6) + MonthBegin(0)  ## 6개월전
        ony_dt = lt_dt - relativedelta(months=12) + MonthBegin(0)  ## 1년전

        ## Data 추출 및 사전작업
        data_c = self.data[lt]

        ## Columns
        lt_1 = []

        for yd in lt:
            yd = re.sub('_', '', yd)
            yd = re.sub('Y', '년', yd)

            lt_1.append(yd)

        data_c.columns = lt_1


        data_c = data_c.loc[[lt_dt, thrm_dt, sxm_dt, ony_dt]] \
                       .reset_index() \
                       .melt(id_vars='TRD_DT')

        ## Date -> String 으로 변경
        dt_cg = {lt_dt: lt_dt.strftime('%y.%m.%d'),thrm_dt: '3개월전',
                        sxm_dt: '6개월전', ony_dt: '1년전'}
        data_c['TRD_DT'] = data_c['TRD_DT'].map(dt_cg)

        return data_c

    def yld_curve(self, size, lt, date = ed_date):

        yld_data = self.yld_data(lt=lt, date=date)

        ## 플랏플랏 ~
        self.fig_set_size_centimeter(size)
        self.esc_option()

        sns.pointplot(data=yld_data, x='variable', y='value', hue='TRD_DT', ax=self.ax)
        self.ax_one_legend()
        self.ax.set_xlabel('')
        self.ax.set_ylabel('')
        self.PercFormatter()

        return self.fig, self.ax

    def yld_chg_plot(self, size, frq):

        self.fig_set_size_centimeter(size)

        self.data.plot(ax=self.ax, legend=True, lw=3)
        self.ax_one_legend()
        self.esc_option()
        self.date_tick(frq)
        self.PercFormatter()

        return self.fig, self.ax

    def kr_sprd_grp(self, size, frq):

        self.fig_set_size_centimeter(size)
        ax1 = self.twin_x()

        data_c = self.data[['국고_3Y', '회사_3Y']].copy()
        data_c['회사AA-스프래드'] = (data_c['회사_3Y'] - data_c['국고_3Y']).div(1 / 100)
        data_c.columns = ['국고3년', '회사AA- 3년', '회사AA-스프래드']

        lin_cols =  ['국고3년', '회사AA- 3년']
        lin_style = ['-.', '-']

        for col, ls in zip(lin_cols, lin_style):

            data_c[col].plot(ax=self.ax, legend=True, lw=3, ls=ls)

        # data_c[['국고3년', '회사AA- 3년']].plot(ax=self.ax, legend = True, lw=3, ls = '-.')
        data_c['회사AA-스프래드'].plot(ax = ax1, kind='area', color = 'grey',
                                    alpha = 0.8, legend = True, lw = 3)

        ax1.set_ylim(bottom = data_c['회사AA-스프래드'].min() - 10, top = data_c['회사AA-스프래드'].max() + 30)
        self.esc_option()
        self.ax_legend(ax1=ax1)
        self.date_tick(frq)

        return self.fig, self.ax

    def ovs_stk_plot(self, size, frq, kspi):

        data_rt = self.data.pct_change().sub(-1).cumprod()
        kspi_rt = kspi['코스피'].pct_change().sub(-1).cumprod()
        std_dt = pd.to_datetime(ed_date) + YearBegin(-1)

        plt_dt = data_rt.sub(kspi_rt, axis=0).div(1 / 100)

        cols = plt_dt.columns
        lin_style = ['-.', '--', '-', '-.', '--']

        for col, ls in zip(cols, lin_style):

            plt_dt[col].plot(ax=self.ax, legend=True, lw=1.5, ls=ls)


        # plt_dt.plot(ax=self.ax, legend=True, lw=1.5)
        (kspi_rt.sub(kspi_rt, axis=0)).plot(ax=self.ax, legend=True, lw=1.5,
                                            color='red', ls='--')

        # self.ax_one_legend()
        self.ax.axvline(x=std_dt, color='red', lw=1.5)
        self.fig_set_size_centimeter(size)
        self.date_tick(frq)

        self.esc_option()
        self.PercFormatter()
        self.ax.legend(loc='upper center', ncol=len(self.columns) + 1,
                       bbox_to_anchor=(0.5, 1.23), frameon=False)  # legend(범주) 변경

        return self.fig, self.ax

    def kr_ov_sprd_plt(self, size, frq):

        self.fig_set_size_centimeter(size)

        self.data.columns = ['미 10년물 스프래드', '일 10년물 스프래드', '독 10년물 스프래드']
        lin_style = ['-', '-.','--']

        for col, ls in zip(self.data.columns, lin_style):

            self.data[col].plot(ax=self.ax, legend=True, lw=3, ls=ls)

        # self.data.plot(ax= self.ax, legend=True, lw=3)

        self.date_tick(frq)

        # l = ax.legend(loc='upper center', ncol=6, bbox_to_anchor=(0.5, 1.1))
        # l.set_title('')

        self.esc_option()
        self.PercFormatter()
        # for spn in ['top', 'right']:
        #     ax.spines[spn].set_color('None')

        return self.fig, self.ax

    def ted_sprd_plt(self, size, frq):

        self.fig_set_size_centimeter(size)
        cols = ['미국3개월', '리보3개월']
        lin_style = ['-.', '--']

        for col, ls in zip(cols,lin_style):
            self.data[col].plot(ax=self.ax, lw=3, ls=ls)

        # self.data[['미국3개월', '리보3개월']].plot(ax=self.ax, lw=3)
        self.data[['TED']].plot(ax=self.ax, kind='area', alpha=0.5,
                           lw=0, color='gray')

        self.ax_one_legend()
        self.date_tick(frq)
        self.esc_option()

        return self.fig, self.ax

    def fund_nch_plt(self, size):

        self.fig_set_size_centimeter(size)

        data = self.data.reset_index()
        ax1 = self.twin_x()
        an_x = 11
        an_y = data['순변화'].iloc[-1]

        data['순변화'].plot(ax=self.ax, color='red', lw=3, legend=True, kind='bar', alpha=0.5)
        self.ax.annotate(str(an_y), xy=(an_x, an_y), xytext=((an_x - 2), (an_y - 0.1)),
                         arrowprops=dict(facecolor='black', shrink=0.05))
        data['순자산'].plot(ax=ax1, legend=True, lw=3)

        self.esc_option()
        self.bar_tick()
        self.ax_legend(ax1=ax1)
        self.right_y_aestatic(ax_one=ax1, num_type= '$')

        plt.xlim(xmin=-0.5, xmax=11.5)
        plt.xticks(rotation=0)

        return self.fig, self.ax, ax1

    def PercFormatter(self):
        return self.ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:.2%}'.format(y/100)))

    def MoneyFormatter(self):
        return self.ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:,}'.format(y)))

    def right_PercFormatter(self):
        return self.ax.right_ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:.2%}'.format(y)))

    def right_MoneyFormatter(self):
        return self.ax.right_ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:,}'.format(y)))

class table:

    def __init__(self, data):

        self.data = data
        self.columns = self.data.columns
        self.ed_date = pd.to_datetime(ed_date)

    def cols_month(self):

        onm_dt = self.ed_date - relativedelta(months=1) + MonthBegin()
        thrm_dt = self.ed_date - relativedelta(months=3) + MonthBegin()
        sxm_dt = self.ed_date - relativedelta(months=6) + MonthBegin()
        ytd_dt = self.ed_date + YearBegin(-1)
        ony_dt = self.ed_date - relativedelta(months=12) + MonthBegin()

        return {"one": onm_dt, "three": thrm_dt,
                "six": sxm_dt, "ytd": ytd_dt, "YoY": ony_dt}

    def change_columns(self):

        ans = [self.ed_date.strftime('%y.%m.%d')]
        cols_mon = self.cols_month()

        for col_id in ["one", "three", "six", "ytd", "YoY"]:

            ans.append(cols_mon[col_id].strftime('%y.%m.%d'))

        return ans

    def tbl_crt_one(self):

        ans = self.data.loc[self.ed_date].apply("{:,.2f}".format)
        cols_mon = self.cols_month()

        for col_id in ["one", "three", "six", "ytd", "YoY"]:
            ans_1 = (self.data.loc[self.ed_date]).div(self.data.loc[cols_mon[col_id]]).sub(1).div(1 / 100)
            ans_1 = ans_1.apply("{:,.2f}".format)
            ans = pd.concat([ans, ans_1], axis = 1)

        ans.columns = self.change_columns()

        return ans

    def tbl_crt_two(self):

        ans = self.data.loc[self.ed_date]
        cols_mon = self.cols_month()

        for col_id in ["one", "three", "six", "ytd", "YoY"]:
            ans_1=(self.data.loc[self.ed_date]).sub(self.data.loc[cols_mon[col_id]]).div(1 / 100)

            ans = pd.concat([ans, ans_1], axis = 1)

        ans.columns = self.change_columns()
        
        return ans
        
    def tbl_crt_three(self):

        ans = self.data.loc[self.ed_date].apply("{:,.0f}".format)
        cols_mon = self.cols_month()

        for col_id in ["one", "three", "six", "ytd", "YoY"]:
            ans_1=(self.data.loc[self.ed_date]).sub(self.data.loc[cols_mon[col_id]]).div(1 / 100).apply("{:.2f}".format)
            ans = pd.concat([ans, ans_1], axis = 1)
            
        ans.columns = self.change_columns()
        
        return ans

## SQL
# - St_Date와 Ed_Date를 바꿔야 하는 것
# Ed_Date만 바꿔도 되는 것

sql_ld = sql_load()

gdp_sql = """
SELECT A.TRD_DT, AMOUNT GDP, (AMOUNT / AMOUNT_PQUARTER) - 1 전기대비
FROM FNE_ECO_DATA@D_FNDB2_UFNGDBA  a
WHERE ECO_CD = '02.79.002.046' 
          and term = 'Q' 
          and a.trd_dt BETWEEN '20160101' AND '20200331'
"""

gdp_data = sql_ld.sql_reader(gdp_sql, cond=False).iloc[-8:]
plot(gdp_data).gdp_plot(size = (23.52, 11.2))
plt.savefig('Test_Data/gdp_plot.png', bbox_inches = 'tight')
plt.close()

## Macro-Economy Data
kr_econ_sql = """
SELECT A.TRD_DT, B.전산업생산, A.동행종합지수, A.동행순환, A.선행종합지수, A.선행순환
FROM 
( 
    SELECT TRD_DT, 
           MIN(CASE WHEN ECO_CD = '04.05.003' THEN AMOUNT END) 동행종합지수, 
           MIN(CASE WHEN ECO_CD = '04.04.009' THEN AMOUNT END) 동행순환,
           MIN(CASE WHEN ECO_CD = '04.04.001' THEN AMOUNT END) 선행종합지수,
           MIN(CASE WHEN ECO_CD = '04.04.004' THEN AMOUNT END) 선행순환
    FROM FNE_ECO_DATA@D_FNDB2_UFNGDBA
    WHERE 1=1 
          AND ECO_CD in ('04.05.003', 
                         '04.04.009', 
                         '04.04.001', 
                         '04.04.004')
          AND TRD_DT BETWEEN '20170430' AND '20180430'
    GROUP BY TRD_DT
    ORDER BY TRD_DT
) A, 
(
    SELECT TRD_DT, ROUND(((AMOUNT / AMOUNT_PMON) - 1) * 100, 2) 전산업생산
    FROM FNE_ECO_DATA@D_FNDB2_UFNGDBA
    WHERE 1=1 
          AND ECO_CD = '04.57.002.001'
          AND TRD_DT BETWEEN '20170430' AND '20180430'
) B
WHERE A.TRD_DT = B.TRD_DT
ORDER BY A.TRD_DT DESC
"""

kr_econ_data = sql_ld.sql_reader(kr_econ_sql, cond=True)
kr_econ_data.to_csv('Test_Data/kr_econ_data.csv', encoding = 'cp949')

## 소비자물가지수

kr_cpi_sql = """
SELECT TRD_DT
       , AMOUNT 소비자물가
       , ROUND(((AMOUNT / AMOUNT_PYEAR) - 1) * 100, 2) 전년동월
FROM FNE_ECO_DATA@D_FNDB2_UFNGDBA
WHERE 1=1 
      AND ECO_CD IN ('06.01.019.002')
      AND TRD_DT BETWEEN '20170430' AND '20180430'
      AND TERM = 'M'
"""

kr_cpi_dt = sql_ld.sql_reader(kr_cpi_sql, cond=True)
plot(kr_cpi_dt).cpi_plot((23.52, 11.2))
plt.savefig('Test_Data/kr_cpi_plt.png', bbox_inches = 'tight')

## Dollar Data
dol_sql = """
SELECT *
FROM (SELECT aaa.trd_dt,
           NVL(LAST_VALUE (NULLIF (bbb.달러인덱스, 0))
                 IGNORE NULLS
                 OVER (ORDER BY aaa.trd_dt), 0) 달러인덱스,
           NVL(LAST_VALUE (NULLIF (bbb.원달러, 0))
                 IGNORE NULLS
                 OVER (ORDER BY aaa.trd_dt), 0) 원달러
      FROM (SELECT trd_dt
            FROM fnc_calendar@D_FNDB2_UFNGDBA
            WHERE trd_dt BETWEEN '20170321' AND '20180430') aaa,
           (SELECT a.trd_dt,
                MIN (CASE
                     WHEN a.eco_cd = '14.09.006.001' THEN a.amount END) 달러인덱스,
                MIN (CASE
                     WHEN a.eco_cd = '13.03.007.001.005' THEN a.amount END) 원달러
            FROM FNE_ECO_DATA@D_FNDB2_UFNGDBA a
            WHERE a.eco_cd IN ('14.09.006.001' -- Dollar Index --  
                               ,'13.03.007.001.005') -- 원달러--
                  AND a.term = 'D'
                  AND a.trd_dt BETWEEN '20170321' AND '20180430'
            GROUP BY a.trd_dt
            ORDER BY a.trd_dt) bbb
     WHERE aaa.trd_dt = bbb.trd_dt(+)) bb
WHERE bb.trd_dt BETWEEN '20170430' AND '20180430'
"""

dol_dt = sql_ld.sql_reader(dol_sql, cond=True)
plot(dol_dt).dol_graph((23.52, 11.2), frq = 'MS')
dol_dt.to_csv('Test_Data/dol_graph.csv', encoding = 'cp949')
plt.savefig('Test_Data/dol_graph.png', bbox_inches = 'tight')

## 수출입데이터
kr_mcr_sql = """
SELECT B.TRD_DT 
       , 국가별수출액 
       , 국가별수입액
       , 국가별수출액 - 국가별수입액 무역수지
       , 수출물량지수
       , 수입물량지수
FROM 
(
    SELECT a.trd_dt,
         MIN (CASE WHEN a.eco_cd = '13.01.005.001.001' THEN amount / 1000 END)
            국가별수출액,
         MIN (CASE WHEN a.eco_cd = '13.01.006.001.001' THEN amount / 1000 END)
            국가별수입액,
         MIN (CASE WHEN a.eco_cd = '13.01.013.002.001' THEN amount END)
            수출물량지수,
         MIN (CASE WHEN a.eco_cd = '13.01.013.004.001' THEN amount END)
            수입물량지수
    FROM FNE_ECO_DATA@D_FNDB2_UFNGDBA a
    WHERE     a.ECO_CD IN
                (                                                    -- 국가별수출액
                 '13.01.005.001.001',
                 -- 국가별수입액
                 '13.01.006.001.001',
                 -- 수출물량지수
                 '13.01.013.002.001',
                 -- 수입물량지수
                 '13.01.013.004.001')
         AND a.term = 'M'
         AND a.trd_dt BETWEEN '20170430' AND '20180430'
    GROUP BY a.trd_dt
    ORDER BY a.trd_dt DESC
) B
ORDER BY B.TRD_DT
"""

kr_mcr_dt = sql_ld.sql_reader(kr_mcr_sql, cond=True)
plot(kr_mcr_dt.sort_index()).exp_plot(size = (23.52, 11.2))
plt.savefig('Test_Data/exp_plot.png', bbox_inches = 'tight')

kr_mcr_dt[['국가별수출액', '국가별수입액', '무역수지']] = kr_mcr_dt[['국가별수출액'
                                                              , '국가별수입액'
                                                              , '무역수지']].applymap("{:,.0f}".format)
kr_mcr_dt.to_csv('Test_Data/kr_mcr_dt.csv', encoding = 'cp949')

## 국내주식 - 기간별
sql_stk_index = """
  select aa.TRD_DT, aa.코스피, aa.코스피200, aa.코스닥, aa.MKF500, aa.MKF가치,
         aa.MKF성장, aa.대형주, aa.중형주, aa.소형주
  from
  (
  select aa.trd_dt,
      nvl(last_value(nullif(min(case when bb.u_cd = 
      'I.001' then cls_prc end), 0)) 
      IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 코스피,
      nvl(last_value(nullif(min(case when bb.u_cd = 
      'I.101' then cls_prc end), 0)) 
      IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 코스피200,                   
      nvl(last_value(nullif(min(case when bb.u_cd = 'I.201' then cls_prc end), 0)) 
      IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 코스닥,           
      nvl(last_value(nullif(min(case when bb.u_cd = 'FI00' then cls_prc end), 0)) 
      IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) MKF500,                          
      nvl(last_value(nullif(min(case when bb.u_cd = 'FI00.VAL' then cls_prc end), 0)) 
      IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) MKF가치,
      nvl(last_value(nullif(min(case when bb.u_cd = 'FI00.GRO' then cls_prc end), 0)) 
      IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) MKF성장,
      nvl(last_value(nullif(min(case when bb.u_cd = 'FI00.LAR' then cls_prc end), 0)) 
      IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 대형주,
      nvl(last_value(nullif(min(case when bb.u_cd = 'FI00.MID' then cls_prc end), 0)) 
      IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 중형주,                                                                         
      nvl(last_value(nullif(min(case when bb.u_cd = 'FI00.SMA' then cls_prc end), 0)) 
      IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 소형주             
  from 
  (
      select trd_dt
      from fnc_calendar@D_FNDB2_UFNGDBA
      where trd_dt BETWEEN '20170321' AND '20180430'
  ) aa, 
  (
      select trd_dt, u_cd, cls_prc
      from FNS_UD@D_FNDB2_UFNGDBA
      where trd_dt BETWEEN '20170321' AND '20180430' and 
      u_cd in 
      (
      'I.001', 'I.101', 'I.201',
      'FI00', 'FI00.VAL', 'FI00.GRO',
      'FI00.LAR', 'FI00.MID', 'FI00.SMA'
      )
  ) bb
  where aa.trd_dt = bb.trd_dt(+)
  group by aa.trd_dt
  order by aa.trd_dt
  ) aa
  where aa.TRD_DT BETWEEN '20170430' AND '20180430'
"""

kr_stk_dt = sql_ld.sql_reader(sql_stk_index)

plot(kr_stk_dt).stk_plot(size = (23.52, 11.2), cols= ['코스피', '코스피200', '코스닥'], frq = 'MS')
plt.savefig('Test_Data/kr_stk_idx.png', bbox_inches = 'tight')
plt.close()

plot(kr_stk_dt).stk_plot(size = (23.52, 11.2),cols= ['대형주', '중형주', '소형주'], frq = 'MS')
plt.savefig('Test_Data/kr_stk_mkt.png', bbox_inches = 'tight')
plt.close()

plot(kr_stk_dt).stk_plot(size = (23.52, 11.2), cols= ['MKF500', 'MKF가치', 'MKF성장'], frq = 'MS')
plt.savefig('Test_Data/kr_stk_style.png', bbox_inches = 'tight')
plt.close()

tbl_kr_stk_idx = table(kr_stk_dt).tbl_crt_one()
tbl_kr_stk_idx.to_csv('Test_Data/tbl_kr_stk_idx.csv', encoding = 'cp949')

sql_stk_sec = """
select aa.TRD_DT, aa.에너지, aa.소재, aa.산업재, aa.경기소비재, aa.필수소비재,
       aa.의료, aa.금융, aa.IT, aa.통신서비스, aa.유틸리티
from
(
  select aa.trd_dt,
  nvl(last_value(nullif(min(case when bb.u_cd = 'FGSC.10' then cls_prc end), 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 에너지,
  nvl(last_value(nullif(min(case when bb.u_cd = 'FGSC.15' then cls_prc end), 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 소재,                   
  nvl(last_value(nullif(min(case when bb.u_cd = 'FGSC.20' then cls_prc end), 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 산업재,           
  nvl(last_value(nullif(min(case when bb.u_cd = 'FGSC.25' then cls_prc end), 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 경기소비재,                          
  nvl(last_value(nullif(min(case when bb.u_cd = 'FGSC.30' then cls_prc end), 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 필수소비재,
  nvl(last_value(nullif(min(case when bb.u_cd = 'FGSC.35' then cls_prc end), 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 의료,
  nvl(last_value(nullif(min(case when bb.u_cd = 'FGSC.40' then cls_prc end), 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 금융,
  nvl(last_value(nullif(min(case when bb.u_cd = 'FGSC.45' then cls_prc end), 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) IT,                                                                         
  nvl(last_value(nullif(min(case when bb.u_cd = 'FGSC.50' then cls_prc end), 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 통신서비스,
  nvl(last_value(nullif(min(case when bb.u_cd = 'FGSC.55' then cls_prc end), 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 유틸리티                                         
  from 
  (
  select trd_dt
  from fnc_calendar@D_FNDB2_UFNGDBA
  where trd_dt BETWEEN '20170321' AND '20180430'
  ) aa, 
  (
  select trd_dt, u_cd, cls_prc
  from FNS_UD@D_FNDB2_UFNGDBA
  where trd_dt BETWEEN '20170321' AND '20180430' and 
  u_cd in (
  'FGSC.10', 'FGSC.15', 'FGSC.20',
  'FGSC.25', 'FGSC.30', 'FGSC.35',
  'FGSC.40', 'FGSC.45', 'FGSC.50',
  'FGSC.55'
  )
  ) bb
  where aa.trd_dt = bb.trd_dt(+)
  group by aa.trd_dt
  order by aa.trd_dt
) aa
  where AA.TRD_DT BETWEEN '20170430' AND '20180430'
"""


def stk_sec_3rd_rk(data, date=ed_date):
    std_dt = pd.to_datetime(date) + YearBegin(-1)

    test_cols = data \
                    .pct_change() \
                    .sub(-1) \
                    .loc[std_dt:] \
        .prod() \
        .sort_values(ascending=False) \
        .head(3) \
        .index

    return test_cols

stk_sec_dt = sql_ld.sql_reader(sql_stk_sec)
sec_col = stk_sec_3rd_rk(stk_sec_dt)
plot(stk_sec_dt).stk_plot(size = (23.52, 11.2), cols = sec_col, frq='MS')
plt.savefig('Test_Data/stk_sec_plt.png', bbox_inches = 'tight')

tbl_kr_stk_sec = table(stk_sec_dt).tbl_crt_one()
tbl_kr_stk_sec.to_csv('Test_Data/tbl_kr_stk_sec.csv', encoding = 'cp949')

## 시가총액별 그래프 MKF500 / MKF가치 / MKF성장
## 코스피 순매수

sql_kpi_buy = """
  select cc.Yrm TRD_DT, cc.CD_NM, sum(cc.순매수) 순매수 
  from
  (
    SELECT A.trd_dt, sum(A.buy_amt - A.sell_amt) 순매수, B.CD_NM, concat(substr(A.trd_dt,1,6),'01') Yrm
    FROM FNS_J_INVEST@D_FNDB2_UFNGDBA A, (SELECT * FROM FNC_GRP_CD_DETAIL@D_FNDB2_UFNGDBA
    WHERE GRP_CD = 'TY01' and 
    cd in (0,8,11)) B, (
    SELECT GICODE FROM FNS_J_MAST@D_FNDB2_UFNGDBA
    WHERE MKT_GB = 1) D
    WHERE A.INVEST_GB = B.CD AND A.GICODE IN D.GICODE AND  
    A.TRD_DT BETWEEN '20170430' AND '20180430'
    group by A.trd_dt, B.CD_NM 
  ) cc
    group by cc.CD_NM, Yrm
    order by Yrm
"""

kpi_buy_dt = sql_ld.sql_reader(sql_kpi_buy)
plot(kpi_buy_dt).net_buy_plot((16.8, 8))
plt.savefig('Test_Data/kpi_buy_plt.png', bbox_inches = 'tight')

## 코스닥 순매수

sql_kdq_buy = """
  select cc.Yrm TRD_DT, cc.CD_NM, sum(cc.순매수) 순매수 
  from
  (
      SELECT A.trd_dt, sum(A.buy_amt - A.sell_amt) 순매수, B.CD_NM, concat(substr(A.trd_dt,1,6), '01') Yrm
      FROM FNS_J_INVEST@D_FNDB2_UFNGDBA A, (SELECT * FROM FNC_GRP_CD_DETAIL@D_FNDB2_UFNGDBA
      WHERE GRP_CD = 'TY01' and 
      cd in (0,8,11)) B, (
      SELECT GICODE FROM FNS_J_MAST@D_FNDB2_UFNGDBA
      WHERE MKT_GB = 2) D
      WHERE A.INVEST_GB = B.CD AND A.GICODE IN D.GICODE AND  
      A.TRD_DT BETWEEN '20170430' AND '20180430'
      group by A.trd_dt, B.CD_NM 
  ) cc
    group by cc.CD_NM, Yrm
    order by Yrm
"""

kdq_buy_dt = sql_ld.sql_reader(sql_kdq_buy)
plot(kdq_buy_dt).net_buy_plot((16.8,8))
plt.savefig('Test_Data/kdq_buy_plt.png', bbox_inches = 'tight')

## 국내채권
## 단기
sql_kr_bd_st = """
SELECT DISTINCT A.TRD_DT, A.콜, A.CD, A.CP, B.기준금리
FROM 
(
SELECT A.TRD_DT, 
       NVL(LAST_VALUE (NULLIF (B.콜, 0))
                 IGNORE NULLS
                 OVER (ORDER BY A.trd_dt), 0) 콜,
       NVL(LAST_VALUE (NULLIF (B.CD, 0))
                 IGNORE NULLS
                 OVER (ORDER BY A.trd_dt), 0) CD,
       NVL(LAST_VALUE (NULLIF (B.CP, 0))
                 IGNORE NULLS
                 OVER (ORDER BY A.trd_dt), 0) CP              
FROM (SELECT trd_dt
            FROM fnc_calendar@D_FNDB2_UFNGDBA
            WHERE trd_dt BETWEEN '20170321' AND '20180430') A,            
(SELECT TRD_DT,
       MIN(CASE WHEN ECO_CD ='11.02.003.003' THEN AMOUNT END) 콜
       , MIN(CASE WHEN ECO_CD ='11.02.003.005' THEN AMOUNT END) CD
       , MIN(CASE WHEN ECO_CD ='11.02.003.017' THEN AMOUNT END) CP       
FROM FNE_ECO_DATA@D_FNDB2_UFNGDBA
WHERE ECO_CD IN 
      ('11.02.003.003'
      ,'11.02.003.005'
      , '11.02.003.017'
      , '11.02.003.519')
      AND trd_dt BETWEEN '20170321' AND '20180430'
      AND TERM = 'D'
GROUP BY TRD_DT) B
WHERE A.TRD_DT = B.TRD_DT(+)
) A, 
(
SELECT A.TRD_DT, NVL(LAST_VALUE (NULLIF (B.AMOUNT, 0))
                   IGNORE NULLS
                   OVER (ORDER BY A.trd_dt), 0) 기준금리
FROM
(SELECT TRD_DT
    FROM fnc_calendar@D_FNDB2_UFNGDBA
    WHERE TRD_DT BETWEEN '20170321' AND '20201231') A, 
(SELECT TRD_DT, AMOUNT
 FROM FNE_ECO_DATA@D_FNDB2_UFNGDBA B
 WHERE 1=1
       AND TRD_DT BETWEEN '20170321' AND '20201231'
       AND B.ECO_CD = '11.02.003.519' ) B
WHERE A.TRD_DT = B.TRD_DT(+)    
) B
WHERE 1=1
      AND A.TRD_DT = B.TRD_DT(+)
      AND A.TRD_DT BETWEEN '20170430' AND '20180430'
      ORDER BY A.TRD_DT
"""

kr_bd_st_dt = sql_ld.sql_reader(sql_kr_bd_st)
plot(kr_bd_st_dt).kr_bd_st_plt(size = (23.52, 11.2), frq='MS')
plt.savefig('Test_Data/kr_bd_st_plt.png', bbox_inches = 'tight')

tbl_kr_bd_st = table(kr_bd_st_dt).tbl_crt_two()
tbl_kr_bd_st.to_csv('Test_Data/tbl_kr_bd_st.csv', encoding = 'cp949')

## 중장기

sql_kr_bd = """
SELECT A.TRD_DT, A.국고_1Y, A.국고_3Y, A.국고_5Y, A.국고_10Y
       , A.국고_20Y, A.국고_30Y, B.회사_3Y 
FROM
(
SELECT trd_dt,
     MIN (CASE WHEN a1.exp_month = '0012' THEN a1.yield END) 국고_1y,
     MIN (CASE WHEN a1.exp_month = '0036' THEN a1.yield END) 국고_3y,
     MIN (CASE WHEN a1.exp_month = '0060' THEN a1.yield END) 국고_5y,
     MIN (CASE WHEN a1.exp_month = '0120' THEN a1.yield END) 국고_10y,
     MIN (CASE WHEN a1.exp_month = '0240' THEN a1.yield END) 국고_20y,
     MIN (CASE WHEN a1.exp_month = '0360' THEN a1.yield END) 국고_30y
FROM FNB_BOND_MATRIX@D_FNDB2_UFNGDBA a1
WHERE     a1.bond_cd IN ('1013000')
     AND a1.INST_CD = 'C'
     AND exp_month IN
            ('0001','0003','0006',
             '0012','0024','0036',
             '0060','0120','0240','0360')
     AND a1.trd_dt BETWEEN '20170321' AND '20180430'
GROUP BY trd_dt
ORDER BY trd_dt
) A, 
(
    SELECT A.TRD_DT, NVL(LAST_VALUE (NULLIF (B.YIELD, 0))
                           IGNORE NULLS
                           OVER (ORDER BY A.TRD_DT), 0) 회사_3Y
    FROM 
    (
        SELECT TRD_DT
          FROM fnc_calendar@D_FNDB2_UFNGDBA
          WHERE TRD_DT BETWEEN '20170321' AND '20180430'    
    ) A, 
    (
        SELECT TRD_DT, YIELD
        FROM FNB_BOND_MATRIX@D_FNDB2_UFNGDBA
        WHERE BOND_CD IN ('7010123')
              AND TRD_DT BETWEEN '20170321' AND '20180430'
              AND EXP_MONTH = '0036'
              AND INST_CD = '9'
    ) B
    WHERE A.TRD_DT = B.TRD_DT(+)
) B
WHERE A.TRD_DT = B.TRD_DT
      AND A.TRD_DT BETWEEN '20170430' AND '20180430'
"""

## 컬럼명 바꾸기 ㅇㅇ
kr_bd_dt = sql_ld.sql_reader(sql_kr_bd)
kr_bd_dt_c = kr_bd_dt[['국고_3Y', '국고_5Y', '국고_10Y', '국고_20Y', '국고_30Y']]
kr_bd_dt_c.columns = ['국고3년', '국고5년', '국고10년', '국고20년', '국고30년']
plot(kr_bd_dt_c).kr_bd_st_plt(size = (23.52, 11.2), frq='MS')
plt.savefig('Test_Data/kr_bd_plt.png', bbox_inches = 'tight')

plot(kr_bd_dt).yld_curve(size = (23.52, 11.2), lt = ['국고_3Y', '국고_5Y', '국고_10Y', '국고_20Y', '국고_30Y'])
plt.savefig('Test_Data/kr_bd_plt_cuv.png', bbox_inches = 'tight')

kr_bd_sprd = kr_bd_dt[['국고_3Y', '회사_3Y']]
plot(kr_bd_sprd).kr_sprd_grp((23.52, 11.2), frq='MS')
plt.savefig('Test_Data/kr_bd_trcorp_sprd.png', bbox_inches = 'tight')

tbl_kr_bd = table(kr_bd_dt).tbl_crt_two()
tbl_kr_bd.to_csv('Test_Data/tbl_kr_bd.csv', encoding = 'cp949')

## MMF

sql_mmf_chg = """
select d.DTE TRD_DT, d.MMF설정액, d.MMF순자산  
from
(
  select c.DTE, nvl(last_value(nullif(c.MMF설정액, 0)) 
  IGNORE NULLS OVER (ORDER BY c.DTE), 0) MMF설정액,
  nvl(last_value(nullif(c.MMF순자산, 0)) 
  IGNORE NULLS OVER (ORDER BY c.DTE), 0) MMF순자산    
  from 
  (
  select a.trd_dt DTE, b.MMF설정액, b.MMF순자산
  from
  (
  select TRD_DT
  from fnc_calendar@D_FNDB2_UFNGDBA 
  where TRD_DT BETWEEN '20170321' AND '20180430'
  ) a, 
  (
  select aa.trd_dt, 
  min(case when aa.remain_gb = '1' then round(aa.money /
  power(10, 8)) end) MMF설정액,
  min(case when aa.remain_gb = '2' then round(aa.money  /
  power(10, 8)) end) MMF순자산
  from FF_ART019 aa  
  where aa.co_cd = '000' and 
  aa.offer_gb = 'A' and
  aa.region_gb = 'A' and 
  aa.remain_gb in ('1', '2') and 
  aa.trd_dt BETWEEN '20170321' AND '20180430'
  group by aa.trd_dt 
  order by aa.trd_dt 
  ) b
  where a.trd_dt = b.trd_dt(+)
  order by a.trd_dt
  ) c 
) d
  where d.DTE BETWEEN '20170430' AND '20180430'
"""

mmf_chg_dt = sql_ld.sql_reader(sql_mmf_chg)
mmf_chg_dt_tbl = table(mmf_chg_dt).tbl_crt_one()
mmf_chg_dt_tbl.to_csv('Test_Data/mmf_chg_dt_tbl.csv', encoding = 'cp949')

## MMF - Plot

kr_mmf_sql_1 = """
   select 월 TRD_DT, sum(aa.MMF) / power(10, 10) MMF
    from
    (
    select concat(substr(a.trd_dt, 1, 6), '01') 월,
    min(case when a.peer_cd = 'DF00A' then a.set_amt - a.set_amt_1d end) MMF       
    from F2_CDT021 a,
    (
    select m.PEER_CD, m.PEER_NM 
    from F2_SGM010 m
    where m.peer_gb = 'G'  
    and m.offer_gb = 'A'
    and m.class_gb  = 'L'
    and m.peer_cd in ('DF00A')
    ) b 
    where a.peer_cd = b.peer_cd and a.co_cd = '000'
    and a.trd_dt BETWEEN '20170430' AND '20180430'
    group by a.trd_dt 
    ) aa
    group by aa.월
    order by aa.월
"""

kr_mmf_sql_1_dt = sql_ld.sql_reader(kr_mmf_sql_1)
plot(kr_mmf_sql_1_dt).mmf_chg_plt((16.8,8))
plt.savefig('Test_Data/mmf_chg_plt.png', bbox_inches = 'tight')

## 해외주식
sql_ov_stk = """
select *
from 
(
  select aa.trd_dt,
  nvl(last_value(nullif(bb.ACWIETF, 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) ACWIETF,
  nvl(last_value(nullif(bb.SP500, 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) SP500,
  nvl(last_value(nullif(bb.상해, 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 상해,                              
  nvl(last_value(nullif(bb.토픽스, 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 토픽스,          
  nvl(last_value(nullif(bb.STOXX600, 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) STOXX600
  from
  (
  select trd_dt
  from fnc_calendar@D_FNDB2_UFNGDBA
  where trd_dt BETWEEN '20170321' AND '20180430'
  ) aa,
  (
  select a.trd_dt, 
  min(case when ITEM_CD = 'I.ISACWI' then a.cls_prc end) ACWIETF,
  min(case when ITEM_CD = 'I.GSPC' then a.cls_prc end) SP500,               
  min(case when ITEM_CD = 'I.SSEC' then a.cls_prc end) 상해,       
  min(case when ITEM_CD = 'I.TOPX' then a.cls_prc end) 토픽스,       
  min(case when ITEM_CD = 'I.STOXX600' then a.cls_prc end) STOXX600       
  from fgs_ovs_index_d@D_FNDB2_UFNGDBA a
  where a.trd_dt BETWEEN '20170321' AND '20180430' and
  a.ITEM_CD in (
  'I.ISACWI', 'I.GSPC', 'I.SZ399106',
  'I.SZ399106', 'I.SSEC', 'I.TOPX', 
  'I.ISEFA', 'I.STOXX600', 'I.IXIC'
  )
  group by a.trd_dt
  order by a.trd_dt
  ) bb
  where aa.trd_dt = bb.trd_dt(+)
) cc
  where cc.trd_dt BETWEEN '20170430' AND '20180430'
"""

ov_stk_dt = sql_ld.sql_reader(sql_ov_stk)
plot(ov_stk_dt).ovs_stk_plot(size = (25, 6),kspi = kr_stk_dt, frq='MS')
plt.savefig('Test_Data/ov_stk_plt.png', bbox_inches = 'tight')

tbl_ov_st = table(ov_stk_dt).tbl_crt_one()
tbl_ov_st.to_csv('Test_Data/ov_stk_tbl.csv', encoding = 'cp949')


## 해외채권
## 미국
sql_us_bd = """
select cc.trd_dt, cc.미국_1Y, cc.미국_3Y, cc.미국_5Y, cc.미국_10Y, 
       cc.미국_30Y
from 
(
    select aa.trd_dt, 
    nvl(last_value(nullif(bb.us_1y, 0)) 
    IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 미국_1Y,               
    nvl(last_value(nullif(bb.us_3y, 0)) 
    IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 미국_3Y,               
    nvl(last_value(nullif(bb.us_5y, 0)) 
    IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 미국_5Y,               
    nvl(last_value(nullif(bb.us_10y, 0)) 
    IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 미국_10Y,               
    nvl(last_value(nullif(bb.us_30y, 0)) 
    IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 미국_30Y               
    from
    (
      select trd_dt
      from fnc_calendar@D_FNDB2_UFNGDBA
      where trd_dt BETWEEN '20170321' AND '20180430'
    ) aa,
    (
      select trd_dt, 
      min(case when eco_cd = '14.07.001.007.003' then amount end) US_1Y,
      min(case when eco_cd = '14.07.001.007.012' then amount end) US_3Y,
      min(case when eco_cd = '14.07.001.007.007' then amount end) US_5Y,
      min(case when eco_cd = '14.07.001.007.004' then amount end) US_10Y, 
      min(case when eco_cd = '14.07.001.007.006' then amount end) US_30Y                                                                                                                                                                                                             
      from FNE_ECO_DATA@D_FNDB2_UFNGDBA 
      WHERE trd_dt BETWEEN '20170321' AND '20180430' and
    eco_cd in 
    (
      '14.07.001.007.003', '14.07.001.007.012', '14.07.001.007.007',
      '14.07.001.007.004', '14.07.001.007.006'
    )  and TERM = 'D'
    group by trd_dt
    ) bb
    where aa.trd_dt = bb.trd_dt(+)
) cc
  where cc.trd_dt BETWEEN '20170430' AND '20180430'
"""

ov_us_bd_dt = sql_ld.sql_reader(sql_us_bd)
plot(ov_us_bd_dt).yld_curve(size = (23.52, 11.2), lt = ['미국_1Y','미국_3Y', '미국_5Y', '미국_10Y', '미국_30Y'])
plt.savefig('Test_Data/ov_bd_plt.png', bbox_inches = 'tight')

## 일본 / 독일

sql_gj_bd = """
select cc.trd_dt,
       cc.JP_1Y, cc.JP_3Y, cc.JP_5Y, cc.JP_10Y, cc.JP_30Y,
       cc.GM_1Y, cc.GM_3Y, cc.GM_5Y, cc.GM_10Y, cc.GM_30Y
from 
(
  select aa.trd_dt,
  nvl(last_value(nullif(bb.JP_1Y, 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) JP_1Y,
  nvl(last_value(nullif(bb.JP_3Y, 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) JP_3Y,                   
  nvl(last_value(nullif(bb.JP_5Y, 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) JP_5Y,                   
  nvl(last_value(nullif(bb.JP_10Y, 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) JP_10Y,                   
  nvl(last_value(nullif(bb.JP_30Y, 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) JP_30Y,                   
  nvl(last_value(nullif(bb.GM_1Y, 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) GM_1Y,                   
  nvl(last_value(nullif(bb.GM_3Y, 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) GM_3Y,                   
  nvl(last_value(nullif(bb.GM_5Y, 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) GM_5Y,                   
  nvl(last_value(nullif(bb.GM_10Y, 0))  
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) GM_10Y,                   
  nvl(last_value(nullif(bb.GM_30Y, 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) GM_30Y                   
  from 
  (
  select trd_dt
  from fnc_calendar@D_FNDB2_UFNGDBA
  where trd_dt BETWEEN '20170321' AND '20180430'
  ) aa,
  (
      select trd_dt, 
      -- 일본
      min(case when eco_cd = '14.07.001.006.003' then amount end) JP_1Y, 
      min(case when eco_cd = '14.07.001.006.008' then amount end) JP_3Y,        
      min(case when eco_cd = '14.07.001.006.011' then amount end) JP_5Y,
      min(case when eco_cd = '14.07.001.006.004' then amount end) JP_10Y,                
      min(case when eco_cd = '14.07.001.006.009' then amount end) JP_30Y,               
      --독일
      min(case when eco_cd = '14.07.001.002.012' then amount end) GM_1Y,        
      min(case when eco_cd = '14.07.001.002.004' then amount end) GM_3Y,        
      min(case when eco_cd = '14.07.001.002.007' then amount end) GM_5Y,        
      min(case when eco_cd = '14.07.001.002.001' then amount end) GM_10Y,        
      min(case when eco_cd = '14.07.001.002.005' then amount end) GM_30Y        
      from FNE_ECO_DATA@D_FNDB2_UFNGDBA
      WHERE trd_dt BETWEEN '20170321' AND '20180430' and
      eco_cd in (
      -- 일본
      '14.07.001.006.003', '14.07.001.006.008',
      '14.07.001.006.011', '14.07.001.006.004',
      '14.07.001.006.009', 
      -- 독일
      '14.07.001.002.012', '14.07.001.002.004', 
      '14.07.001.002.007', '14.07.001.002.001',
      '14.07.001.002.005'
      )  and TERM = 'D'
      group by trd_dt
      order by trd_dt 
  ) bb
  where aa.trd_dt = bb.trd_dt(+)
) cc
  where cc.trd_dt BETWEEN '20170430' AND '20180430'
"""

ov_gj_bd_dt = sql_ld.sql_reader(sql_gj_bd)

ov_bd_tbl = pd.concat([table(ov_us_bd_dt).tbl_crt_two(),
                       table(ov_gj_bd_dt).tbl_crt_two()], axis = 0)

ov_bd_tbl.to_csv('Test_Data/ov_bd_tbl.csv', encoding = 'cp949')

sql_kr_ov_sprd = """
SELECT C.TRD_DT
       , C.KR_10Y - C.US_10Y US_SPREAD
       , C.KR_10Y - C.JP_10Y JP_SPREAD
       , C.KR_10Y - C.GM_10Y GM_SPREAD
FROM 
(
    SELECT A.TRD_DT,
           NVL(LAST_VALUE(NULLIF(B.KR_10Y, 0))
           IGNORE NULLS OVER (ORDER BY A.trd_dt), 0) KR_10Y,
           NVL(LAST_VALUE(NULLIF(B.US_10Y, 0))
           IGNORE NULLS OVER (ORDER BY A.trd_dt), 0) US_10Y,
           NVL(LAST_VALUE(NULLIF(B.JP_10Y, 0))
           IGNORE NULLS OVER (ORDER BY A.trd_dt), 0) JP_10Y,
           NVL(LAST_VALUE(NULLIF(B.GM_10Y, 0))
           IGNORE NULLS OVER (ORDER BY A.trd_dt), 0) GM_10Y       
    FROM
    (
    SELECT trd_dt
        FROM fnc_calendar@D_FNDB2_UFNGDBA
            WHERE trd_dt BETWEEN '20170321' AND '20180430'
    ) A,
    (
    SELECT trd_dt,
           MIN (CASE WHEN eco_cd = '11.02.003.021' THEN amount END) KR_10Y,
           MIN (CASE WHEN eco_cd = '14.07.001.007.004' THEN amount END) US_10Y,
           MIN (CASE WHEN eco_cd = '14.07.001.006.004' THEN amount END) JP_10Y,
           MIN (CASE WHEN eco_cd = '14.07.001.002.001' THEN amount END) GM_10Y
        FROM FNE_ECO_DATA@D_FNDB2_UFNGDBA 
        WHERE trd_dt BETWEEN '20170321' AND '20180430' AND
              eco_cd in 
            ('11.02.003.021' -- 한국
              , '14.07.001.007.004' -- 미국
              , '14.07.001.006.004' -- 일본
              , '14.07.001.002.001' -- 독일          
            ) and TERM = 'D'
    GROUP BY trd_dt
    ORDER BY TRD_DT
    ) B
    WHERE A.TRD_DT = B.TRD_DT(+)
) C
    WHERE C.TRD_DT BETWEEN '20170430' AND '20180430'
"""

## 주요국 5년

sql_ov_bd_5yr = """
SELECT A.TRD_DT, 한국5년, 미국5년, 일본5년, 독일5년 
FROM
(
    SELECT A.TRD_DT,   
           NVL(LAST_VALUE(NULLIF(B.한국5년, 0))
           IGNORE NULLS OVER (ORDER BY A.trd_dt), 0) 한국5년,
           NVL(LAST_VALUE(NULLIF(B.미국5년, 0))
           IGNORE NULLS OVER (ORDER BY A.trd_dt), 0) 미국5년,
           NVL(LAST_VALUE(NULLIF(B.일본5년, 0))
           IGNORE NULLS OVER (ORDER BY A.trd_dt), 0) 일본5년,
           NVL(LAST_VALUE(NULLIF(B.독일5년, 0))
           IGNORE NULLS OVER (ORDER BY A.trd_dt), 0) 독일5년
    FROM
    (
    SELECT trd_dt
        FROM fnc_calendar@D_FNDB2_UFNGDBA
            WHERE trd_dt BETWEEN '20170321' AND '20180430'
    ) A,
    (
    SELECT TRD_DT,
           MIN (CASE WHEN eco_cd = '11.02.003.014' THEN amount END) AS "한국5년",
           MIN (CASE WHEN eco_cd = '14.07.001.007.007' THEN amount END) "미국5년",
           MIN (CASE WHEN eco_cd = '14.07.001.006.011' THEN amount END) "일본5년",
           MIN (CASE WHEN eco_cd = '14.07.001.002.007' THEN amount END) "독일5년"
        FROM FNE_ECO_DATA@D_FNDB2_UFNGDBA 
        WHERE TRD_DT BETWEEN '20170321' AND '20180430' AND
              ECO_CD in 
            ('11.02.003.014' -- 한국
              , '14.07.001.007.007' -- 미국
              , '14.07.001.006.011' -- 일본
              , '14.07.001.002.007' -- 독일          
            ) and TERM = 'D'
    GROUP BY TRD_DT
    ORDER BY TRD_DT    
    ) B
    WHERE 1=1
          AND A.TRD_DT = B.TRD_DT(+)
) A
WHERE A.TRD_DT BETWEEN '20170430' AND '20180430'

"""
ov_bd_5yr = sql_ld.sql_reader(sql_ov_bd_5yr)
plot(ov_bd_5yr).kr_bd_st_plt(size = (23.52, 11.2), frq='MS')
plt.savefig('Test_Data/ov_bd_5yr.png', bbox_inches = 'tight')

## 한국 / 주요국 10년 스프래드

kr_sprd_dt = sql_ld.sql_reader(sql_kr_ov_sprd)
plot(kr_sprd_dt).kr_ov_sprd_plt(size=(23.52, 11.2), frq='MS')
plt.savefig('Test_Data/kr_bd_sprd_plt.png', bbox_inches = 'tight')

## TED Spread

sql_ted_sprd = """
SELECT cc.trd_dt,
       cc.ThreeM  미국3개월,
       cc.Libor 리보3개월,
       cc.TED TED
  FROM (SELECT aa.trd_dt,
               NVL (
                  LAST_VALUE (NULLIF (bb.ThreeM, 0))
                     IGNORE NULLS
                     OVER (ORDER BY aa.trd_dt),
                  0)
                  ThreeM,
               NVL (
                  LAST_VALUE (NULLIF (bb.Libor, 0))
                     IGNORE NULLS
                     OVER (ORDER BY aa.trd_dt),
                  0)
                  Libor,
               NVL (
                  LAST_VALUE (NULLIF (bb.TED, 0))
                     IGNORE NULLS
                     OVER (ORDER BY aa.trd_dt),
                  0)
                  TED
          FROM (SELECT trd_dt
                  FROM fnc_calendar@D_FNDB2_UFNGDBA
                 WHERE trd_dt BETWEEN '20170321' AND '20180430') aa,
               (  SELECT trd_dt,
                         MIN (
                            CASE
                               WHEN eco_cd = '14.07.001.007.001' THEN amount
                            END)
                            ThreeM,
                         MIN (
                            CASE
                               WHEN eco_cd = '14.07.002.002.029' THEN amount
                            END)
                            Libor,
                           MIN (
                              CASE
                                 WHEN eco_cd = '14.07.002.002.029' THEN amount
                              END)
                         - MIN (
                              CASE
                                 WHEN eco_cd = '14.07.001.007.001' THEN amount
                              END)
                            TED
                    FROM FNE_ECO_DATA@D_FNDB2_UFNGDBA
                   WHERE     term = 'D'
                         AND trd_dt BETWEEN '20170321' AND '20180430'
                         AND eco_cd IN
                                ('14.07.001.007.001', '14.07.002.002.029')
                GROUP BY trd_dt
                ORDER BY trd_dt) bb
         WHERE aa.trd_dt = bb.trd_dt(+)) cc
 WHERE cc.trd_dt BETWEEN '20170430' AND '20180430'
"""

ted_sprd_dt = sql_ld.sql_reader(sql_ted_sprd)
plot(ted_sprd_dt).ted_sprd_plt(size = (23.52, 11.2), frq='MS')
plt.savefig('Test_Data/ted_sprd_plt.png', bbox_inches = 'tight')

### 펀드분석

## Business Day Function

"""
IF Holyday then == date none 
the last row of fnc_cal will be the answer yeahp.
"""

## business day

sql_bday = """
select TRD_DT
from fnc_calendar@D_FNDB2_UFNGDBA
where 1=1 
      AND TRD_DT BETWEEN '20180810' AND '20180430'
      AND HOLIDAY_YN = 'N'
ORDER BY TRD_DT
"""

## 펀드 유형별 순자산
sql_fdtype_NtF = """
select d.peer_nm, round(설정액) 설정액,
       round(순자산) 순자산,
round((순자산 / OneM - 1) * 100, 2) OneM,
round((순자산 / ThrM - 1) * 100, 2) ThrM,
round((순자산 / SxM - 1) * 100, 2) SxM,
round((순자산 / YTD - 1) * 100, 2) YTD,
round((순자산 / OneY - 1) * 100, 2) OneY
from 
(
  select c.peer_nm, 
  a.set_amt / power(10,8) 설정액, 
  a.nav / power(10,8) 순자산,
  a.nav_1M / power(10,8) OneM,
  a.nav_3M / power(10,8) ThrM,
  a.nav_6M / power(10,8) SxM,
  a.nav_YTD / power(10,8) YTD, 
  a.nav_1Y / power(10,8) OneY
  from F2_CDT021 a, 
  (
  select PEER_CD, PEER_NM 
  from F2_SGM010 m
  where m.peer_gb = 'G' -- 일반유형
  and m.offer_gb = '1' -- 공모
  and m.class_gb  = 'L' -- 대유형
  and m.peer_nm in ('국내주식형', '국내혼합형','국내채권형', 
  'MMF', '해외주식형', '해외혼합형', '해외채권형',
  '국내부동산', '해외부동산','대안투자형', '기타형','전체')
  ) c
  where a.peer_cd = c.peer_cd and a.co_cd = '000'
  and a.trd_dt = '20180430'
  union all
  select '전체', 
  sum(a.set_amt / power(10,8)) 설정액, 
  sum(a.nav / power(10,8)) 순자산,
  sum(a.nav_1M / power(10,8)) OneM,
  sum(a.nav_3M / power(10,8)) ThrM,
  sum(a.nav_6M / power(10,8)) SxM,
  sum(a.nav_YTD / power(10,8)) YTD,
  sum(a.nav_1Y / power(10,8)) OneY
  from F2_CDT021 a, 
  (
  select PEER_CD, PEER_NM 
  from F2_SGM010 m
  where m.peer_gb = 'G' -- 일반유형
  and m.offer_gb = '1' -- 공모
  and m.class_gb  = 'L' -- 대유형
  and m.peer_nm in ('국내주식형', '국내혼합형','국내채권형', 
  'MMF', '해외주식형', '해외혼합형', '해외채권형',
  '국내부동산', '해외부동산','대안투자형', '기타형','전체')
  ) c
  where a.peer_cd = c.peer_cd and a.co_cd = '000'
  and a.trd_dt = '20180430'
) d
"""

## 펀드 유형별 수익률
sql_fdtype_rt = """
  select c.peer_nm 구분, 
       round(a.nav / power(10, 8)) 순자산, 
        round((RT_1M - 1) * 100, 2) as OneM,
        round((RT_3M - 1) * 100, 2) as ThrM,
        round((RT_6M - 1) * 100, 2) as SxM,
        round((RT_YTD - 1) * 100, 2) as YTD,
        round((RT_1Y - 1) * 100, 2) as OneY
  from F2_CDT021 a, 
  (
    select PEER_CD, PEER_NM 
    from F2_SGM010 m
    where m.peer_gb = 'G' -- 일반유형
    and m.offer_gb = '1' -- 공모
    and m.class_gb  = 'L' -- 대유형
    and m.peer_nm in ('국내주식형', '국내혼합형','국내채권형', 
    'MMF', '해외주식형', '해외혼합형', '해외채권형',
    '국내부동산', '해외부동산','대안투자형', '기타형','전체')
  ) c
    where a.peer_cd = c.peer_cd and a.co_cd = '000'
    and a.trd_dt = '20180430'
"""

## 펀드 지역별 수익률
sql_fdrg_rt = """
  select b.cd_nm, round(a.nav / power(10, 8)) 순자산,
       round((RT_1M - 1) * 100, 2) as OneM,
       round((RT_3M - 1) * 100, 2) as Thrm,
       round((RT_6M - 1) * 100, 2) as SxM,
       round((RT_YTD - 1) * 100, 2) as YTD,       
       round((RT_1Y - 1) * 100, 2) as OneY
  from F2_CDT521 a,
  (
    select distinct CD, CD_NM
    from FF_SCM021
    where substr(CD, 1,2) = 'W0' AND cd not in ('W0000', 
    'W0025') and
    grp_cd = 'FBM0111' 
    order by CD
  ) b
  where b.cd = a.class_cd and peer_cd = 'OS001' and
  a.co_cd = '000' and a.trd_dt = '20180430'
"""

## 공모펀드 전체 순자산 변화

sql_fund_ncg = """
select concat(substr(cc.trd_dt, 1, 6), '01') TRD_DT, 
max(case when cc.trd_dt = last_day(cc.trd_dt) then 
round(cc.순자산 / power(10,11)) else 0 end) 순자산,
round(sum(cc.순변화) / power(10, 11)) 순변화
from 
(
  select aa.trd_dt, 
  nvl(last_value(nullif(bb.순자산, 0)) 
  IGNORE NULLS OVER (ORDER BY aa.trd_dt), 0) 순자산,
  bb.순변화        
  from
  (
  select trd_dt
  from fnc_calendar@D_FNDB2_UFNGDBA
  where trd_dt BETWEEN '20170430' AND '20180430'
  ) aa, 
  (
  select a.trd_dt, 
  sum(a.nav) 순자산,
  (sum(a.nav) - sum(a.nav_1D)) 순변화 
  from F2_CDT021 a, 
  (
  select PEER_CD, PEER_NM 
  from F2_SGM010 m
  where m.peer_gb = 'G' -- 일반유형
  and m.offer_gb = '1' -- 공모
  and m.class_gb  = 'L' -- 대유형
  and m.peer_nm in ('국내주식형', '국내혼합형','국내채권형', 
  'MMF', '해외주식형', '해외혼합형', '해외채권형',
  '국내부동산', '해외부동산','대안투자형', '기타형','전체')
  ) c
  where a.peer_cd = c.peer_cd and a.co_cd = '000'
  and a.trd_dt BETWEEN '20170430' AND '20180430'
  group by a.trd_dt
  order by a.trd_dt
  ) bb
  where aa.trd_dt = bb.trd_dt(+)
  order by aa.trd_dt
) cc
  group by substr(cc.trd_dt, 1, 6)
  order by substr(cc.trd_dt, 1, 6)
"""

fund_ncg_dt = sql_ld.sql_reader(sql_fund_ncg)
plot(fund_ncg_dt).fund_nch_plt(size = (25, 7))
plt.savefig('Test_Data/fund_ncg_plt.png', bbox_inches = 'tight')

fdtype_NtF_tbl = sql_ld.fnd_sql_qry(sql_fdtype_NtF)
fdtype_NtF_tbl[['설정액', '순자산']] = fdtype_NtF_tbl[['설정액', '순자산']].applymap(("{:,.0f}".format))

fdtype_rt_tbl = sql_ld.fnd_sql_qry(sql_fdtype_rt)
fdtype_rt_tbl['순자산'] = fdtype_rt_tbl['순자산'].map(("{:,.0f}".format))

fdrg_rt_tbl = sql_ld.fnd_sql_qry(sql_fdrg_rt)
fdrg_rt_tbl['순자산'] = fdrg_rt_tbl['순자산'].map(("{:,.0f}".format))

fdtype_NtF_tbl.to_csv('Test_Data/fdtype_NtF_tbl.csv', encoding = 'cp949') ## 수출입표
fdtype_rt_tbl.to_csv('Test_Data/fdtype_rt_tbl.csv', encoding = 'cp949') ## 수출입표
fdrg_rt_tbl.to_csv('Test_Data/fdrg_rt_tbl.csv', encoding = 'cp949') ## 수출입표