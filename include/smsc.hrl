%% -*- coding: utf-8 -*-
%% Author: Andrey Andruschenko <apofiget@gmail.com>

-define(URL, "http://smsc.ru/sys/").

%% Error code in server response when try to send message
-define(ERR_CODE, [{1, <<"Ошибка в параметрах."/utf8>>},
                   {2, <<"Неверный логин или пароль."/utf8>>},
                   {3, <<"Недостаточно средств на счете Клиента."/utf8>>},
                   {4, <<"IP-адрес временно заблокирован из-за частых ошибок в запросах."/utf8>>},
                   {5, <<"Неверный формат даты."/utf8>>},
                   {6, <<"Сообщение запрещено (по тексту или по имени отправителя)."/utf8>>},
                   {7, <<"Неверный формат номера телефона."/utf8>>},
                   {8, <<"Сообщение на указанный номер не может быть доставлено."/utf8>>},
                   {9, <<"Отправка более одного одинакового запроса на передачу SMS-сообщения либо более пяти одинаковых запросов на получение стоимости сообщения в течение минуты."/utf8>>}]).

%% Error code in server response on status request of sms/hlr
-define(STATUS_CODE, [{1, <<"Ошибка в параметрах."/utf8>>},
                      {2, <<"Неверный логин или пароль."/utf8>>},
                      {3, <<"Сообщение не найдено. При множественном запросе для данной ошибки возвращается статус с кодом '-3'."/utf8>>},
                      {4, <<"IP-адрес временно заблокирован."/utf8>>},
                      {5, <<"Ошибка удаления сообщения."/utf8>>},
                      {9, <<"Попытка отправки более пяти запросов на получение статуса одного и того же сообщения в течение минуты."/utf8>>}]).

%% Error code in sms/hlr response - field <status>
-define(SMS_HLR_STATUS_CODE, [{-3,  <<"Сообщение не найдено"/utf8>>},
                              {-1, <<"Ожидает отправки"/utf8>>},
                              {0,  <<"Передано оператору"/utf8>>},
                              {1,  <<"Доставлено"/utf8>>},
                              {4,  <<"Просрочено"/utf8>>},
                              {20, <<"Невозможно доставить"/utf8>>},
                              {22, <<"Неверный номер"/utf8>>},
                              {23, <<"Запрещено"/utf8>>},
                              {24, <<"Недостаточно средств"/utf8>>},
                              {25, <<"Недоступный номер"/utf8>>}]).

%% Error code in sms/hlr response - field <err>
-define(SMS_HRL_ERR_CODE,  [{0, <<"Нет ошибки"/utf8>>},
                            {1, <<"Абонент не существует"/utf8>>},
                            {6, <<"Абонент не в сети"/utf8>>},
                            {11, <<"Нет услуги SMS"/utf8>>},
                            {13, <<"Абонент заблокирован"/utf8>>},
                            {21, <<"Нет поддержки SMS"/utf8>>},
                            {245, <<"Статус не получен"/utf8>>},
                            {246, <<"Ограничение по времени"/utf8>>},
                            {247, <<"Превышен лимит сообщений"/utf8>>},
                            {248, <<"Нет маршрута"/utf8>>},
                            {249, <<"Неверный формат номера"/utf8>>},
                            {250, <<"Номер запрещен настройками"/utf8>>},
                            {251, <<"Превышен лимит на один номер"/utf8>>},
                            {252, <<"Номер запрещен"/utf8>>},
                            {253, <<"Запрещено спам-фильтром"/utf8>>},
                            {254, <<"Запрещенный sender id"/utf8>>},
                            {255, <<"Отклонено оператором"/utf8>>}]).

-define(BALANCE_CODE, [{1, <<"Ошибка в параметрах."/utf8>>},
                       {2, <<"Неверный логин или пароль."/utf8>>},
                       {4, <<"IP-адрес временно заблокирован."/utf8>>},
                       {9, <<"Попытка отправки более десяти запросов на получение баланса в течение минуты."/utf8>>}]).

-define(PHONES_CODE, [{1, <<"Ошибка в параметрах."/utf8>>},
                      {2, <<"Неверный логин или пароль."/utf8>>},
                      {3, <<"Записи не найдены."/utf8>>},
                      {4, <<"IP-адрес временно заблокирован."/utf8>>},
                      {5, <<"Ошибка выполнения операции."/utf8>>},
                      {9, <<"Попытка отправки более трех одинаковых запросов на операции с группами, контактами или записями 'черного' списка в течение минуты."/utf8>>}]).

-define(OPER_CODE,   [{1, <<"Ошибка в параметрах."/utf8>>},
                      {2, <<"Неверный логин или пароль."/utf8>>},
                      {3, <<"Оператор не найден."/utf8>>},
                      {4, <<"IP-адрес временно заблокирован."/utf8>>},
                      {5, <<"Ошибка выполнения операции."/utf8>>},
                      {9, <<"Попытка отправки более трех одинаковых запросов или любых 100 запросов на получение информации об операторе абонента в течение минуты."/utf8>>}]).

-define(STAT_CODE,   [{1, <<"Ошибка в параметрах."/utf8>>},
                      {2, <<"Неверный логин или пароль."/utf8>>},
                      {4, <<"IP-адрес временно заблокирован."/utf8>>},
                      {9, <<"Попытка отправки более трех одинаковых запросов на получение списка входящих сообщений в течение минуты."/utf8>>}]).

