{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DescribeAttackStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the number and type of attacks AWS Shield has detected in the last year for all resources that belong to your account, regardless of whether you've defined Shield protections for them. This operation is available to Shield customers as well as to Shield Advanced customers.
--
--
-- The operation returns data for the time range of midnight UTC, one year ago, to midnight UTC, today. For example, if the current time is @2020-10-26 15:39:32 PDT@ , equal to @2020-10-26 22:39:32 UTC@ , then the time range for the attack data returned is from @2019-10-26 00:00:00 UTC@ to @2020-10-26 00:00:00 UTC@ .
--
-- The time range indicates the period covered by the attack statistics data items.
module Network.AWS.Shield.DescribeAttackStatistics
  ( -- * Creating a Request
    describeAttackStatistics,
    DescribeAttackStatistics,

    -- * Destructuring the Response
    describeAttackStatisticsResponse,
    DescribeAttackStatisticsResponse,

    -- * Response Lenses
    dasrsResponseStatus,
    dasrsTimeRange,
    dasrsDataItems,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types

-- | /See:/ 'describeAttackStatistics' smart constructor.
data DescribeAttackStatistics = DescribeAttackStatistics'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAttackStatistics' with the minimum fields required to make a request.
describeAttackStatistics ::
  DescribeAttackStatistics
describeAttackStatistics = DescribeAttackStatistics'

instance AWSRequest DescribeAttackStatistics where
  type Rs DescribeAttackStatistics = DescribeAttackStatisticsResponse
  request = postJSON shield
  response =
    receiveJSON
      ( \s h x ->
          DescribeAttackStatisticsResponse'
            <$> (pure (fromEnum s))
            <*> (x .:> "TimeRange")
            <*> (x .?> "DataItems" .!@ mempty)
      )

instance Hashable DescribeAttackStatistics

instance NFData DescribeAttackStatistics

instance ToHeaders DescribeAttackStatistics where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSShield_20160616.DescribeAttackStatistics" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeAttackStatistics where
  toJSON = const (Object mempty)

instance ToPath DescribeAttackStatistics where
  toPath = const "/"

instance ToQuery DescribeAttackStatistics where
  toQuery = const mempty

-- | /See:/ 'describeAttackStatisticsResponse' smart constructor.
data DescribeAttackStatisticsResponse = DescribeAttackStatisticsResponse'
  { _dasrsResponseStatus ::
      !Int,
    _dasrsTimeRange ::
      !TimeRange,
    _dasrsDataItems ::
      ![AttackStatisticsDataItem]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAttackStatisticsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasrsResponseStatus' - -- | The response status code.
--
-- * 'dasrsTimeRange' - Undocumented member.
--
-- * 'dasrsDataItems' - The data that describes the attacks detected during the time period.
describeAttackStatisticsResponse ::
  -- | 'dasrsResponseStatus'
  Int ->
  -- | 'dasrsTimeRange'
  TimeRange ->
  DescribeAttackStatisticsResponse
describeAttackStatisticsResponse pResponseStatus_ pTimeRange_ =
  DescribeAttackStatisticsResponse'
    { _dasrsResponseStatus =
        pResponseStatus_,
      _dasrsTimeRange = pTimeRange_,
      _dasrsDataItems = mempty
    }

-- | -- | The response status code.
dasrsResponseStatus :: Lens' DescribeAttackStatisticsResponse Int
dasrsResponseStatus = lens _dasrsResponseStatus (\s a -> s {_dasrsResponseStatus = a})

-- | Undocumented member.
dasrsTimeRange :: Lens' DescribeAttackStatisticsResponse TimeRange
dasrsTimeRange = lens _dasrsTimeRange (\s a -> s {_dasrsTimeRange = a})

-- | The data that describes the attacks detected during the time period.
dasrsDataItems :: Lens' DescribeAttackStatisticsResponse [AttackStatisticsDataItem]
dasrsDataItems = lens _dasrsDataItems (\s a -> s {_dasrsDataItems = a}) . _Coerce

instance NFData DescribeAttackStatisticsResponse
