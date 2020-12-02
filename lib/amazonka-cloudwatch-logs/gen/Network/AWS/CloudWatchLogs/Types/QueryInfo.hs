{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.QueryInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.QueryInfo where

import Network.AWS.CloudWatchLogs.Types.QueryStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about one CloudWatch Logs Insights query that matches the request in a @DescribeQueries@ operation.
--
--
--
-- /See:/ 'queryInfo' smart constructor.
data QueryInfo = QueryInfo'
  { _qiStatus :: !(Maybe QueryStatus),
    _qiQueryId :: !(Maybe Text),
    _qiLogGroupName :: !(Maybe Text),
    _qiQueryString :: !(Maybe Text),
    _qiCreateTime :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QueryInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qiStatus' - The status of this query. Possible values are @Cancelled@ , @Complete@ , @Failed@ , @Running@ , @Scheduled@ , and @Unknown@ .
--
-- * 'qiQueryId' - The unique ID number of this query.
--
-- * 'qiLogGroupName' - The name of the log group scanned by this query.
--
-- * 'qiQueryString' - The query string used in this query.
--
-- * 'qiCreateTime' - The date and time that this query was created.
queryInfo ::
  QueryInfo
queryInfo =
  QueryInfo'
    { _qiStatus = Nothing,
      _qiQueryId = Nothing,
      _qiLogGroupName = Nothing,
      _qiQueryString = Nothing,
      _qiCreateTime = Nothing
    }

-- | The status of this query. Possible values are @Cancelled@ , @Complete@ , @Failed@ , @Running@ , @Scheduled@ , and @Unknown@ .
qiStatus :: Lens' QueryInfo (Maybe QueryStatus)
qiStatus = lens _qiStatus (\s a -> s {_qiStatus = a})

-- | The unique ID number of this query.
qiQueryId :: Lens' QueryInfo (Maybe Text)
qiQueryId = lens _qiQueryId (\s a -> s {_qiQueryId = a})

-- | The name of the log group scanned by this query.
qiLogGroupName :: Lens' QueryInfo (Maybe Text)
qiLogGroupName = lens _qiLogGroupName (\s a -> s {_qiLogGroupName = a})

-- | The query string used in this query.
qiQueryString :: Lens' QueryInfo (Maybe Text)
qiQueryString = lens _qiQueryString (\s a -> s {_qiQueryString = a})

-- | The date and time that this query was created.
qiCreateTime :: Lens' QueryInfo (Maybe Natural)
qiCreateTime = lens _qiCreateTime (\s a -> s {_qiCreateTime = a}) . mapping _Nat

instance FromJSON QueryInfo where
  parseJSON =
    withObject
      "QueryInfo"
      ( \x ->
          QueryInfo'
            <$> (x .:? "status")
            <*> (x .:? "queryId")
            <*> (x .:? "logGroupName")
            <*> (x .:? "queryString")
            <*> (x .:? "createTime")
      )

instance Hashable QueryInfo

instance NFData QueryInfo
