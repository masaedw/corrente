-- ユーザ
create table users (
id integer primary key,
twitter_id text,
screen_name text
);

-- 例文
create table items (
id integer primary key,
twitter_id integer,
example text,
native text
);

-- 戦績
create table records (  
id integer primary key, 
twitter_id integer,
item_id integer,
date text
);

-- 例文のレーティング
create table rates (
id integer primary key,
twitter_id integer,
item_id integer,
rate real
);
