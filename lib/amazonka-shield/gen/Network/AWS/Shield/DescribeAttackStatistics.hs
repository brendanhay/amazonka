{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- The operation returns data for the time range of midnight UTC, one year ago, to midnight UTC, today. For example, if the current time is @2020-10-26 15:39:32 PDT@ , equal to @2020-10-26 22:39:32 UTC@ , then the time range for the attack data returned is from @2019-10-26 00:00:00 UTC@ to @2020-10-26 00:00:00 UTC@ .
-- The time range indicates the period covered by the attack statistics data items.
module Network.AWS.Shield.DescribeAttackStatistics
  ( -- * Creating a request
    DescribeAttackStatistics (..),
    mkDescribeAttackStatistics,

    -- * Destructuring the response
    DescribeAttackStatisticsResponse (..),
    mkDescribeAttackStatisticsResponse,

    -- ** Response lenses
    dasrsTimeRange,
    dasrsDataItems,
    dasrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkDescribeAttackStatistics' smart constructor.
data DescribeAttackStatistics = DescribeAttackStatistics'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAttackStatistics' with the minimum fields required to make a request.
mkDescribeAttackStatistics ::
  DescribeAttackStatistics
mkDescribeAttackStatistics = DescribeAttackStatistics'

instance Lude.AWSRequest DescribeAttackStatistics where
  type Rs DescribeAttackStatistics = DescribeAttackStatisticsResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAttackStatisticsResponse'
            Lude.<$> (x Lude..:> "TimeRange")
            Lude.<*> (x Lude..?> "DataItems" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAttackStatistics where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.DescribeAttackStatistics" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAttackStatistics where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DescribeAttackStatistics where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAttackStatistics where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAttackStatisticsResponse' smart constructor.
data DescribeAttackStatisticsResponse = DescribeAttackStatisticsResponse'
  { timeRange :: TimeRange,
    -- | The data that describes the attacks detected during the time period.
    dataItems :: [AttackStatisticsDataItem],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAttackStatisticsResponse' with the minimum fields required to make a request.
--
-- * 'timeRange' -
-- * 'dataItems' - The data that describes the attacks detected during the time period.
-- * 'responseStatus' - The response status code.
mkDescribeAttackStatisticsResponse ::
  -- | 'timeRange'
  TimeRange ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAttackStatisticsResponse
mkDescribeAttackStatisticsResponse pTimeRange_ pResponseStatus_ =
  DescribeAttackStatisticsResponse'
    { timeRange = pTimeRange_,
      dataItems = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'timeRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrsTimeRange :: Lens.Lens' DescribeAttackStatisticsResponse TimeRange
dasrsTimeRange = Lens.lens (timeRange :: DescribeAttackStatisticsResponse -> TimeRange) (\s a -> s {timeRange = a} :: DescribeAttackStatisticsResponse)
{-# DEPRECATED dasrsTimeRange "Use generic-lens or generic-optics with 'timeRange' instead." #-}

-- | The data that describes the attacks detected during the time period.
--
-- /Note:/ Consider using 'dataItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrsDataItems :: Lens.Lens' DescribeAttackStatisticsResponse [AttackStatisticsDataItem]
dasrsDataItems = Lens.lens (dataItems :: DescribeAttackStatisticsResponse -> [AttackStatisticsDataItem]) (\s a -> s {dataItems = a} :: DescribeAttackStatisticsResponse)
{-# DEPRECATED dasrsDataItems "Use generic-lens or generic-optics with 'dataItems' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrsResponseStatus :: Lens.Lens' DescribeAttackStatisticsResponse Lude.Int
dasrsResponseStatus = Lens.lens (responseStatus :: DescribeAttackStatisticsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAttackStatisticsResponse)
{-# DEPRECATED dasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
