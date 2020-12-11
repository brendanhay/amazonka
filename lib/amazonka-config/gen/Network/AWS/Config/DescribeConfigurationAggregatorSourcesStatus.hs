{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeConfigurationAggregatorSourcesStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns status information for sources within an aggregator. The status includes information about the last time AWS Config verified authorization between the source account and an aggregator account. In case of a failure, the status contains the related error code or message.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeConfigurationAggregatorSourcesStatus
  ( -- * Creating a request
    DescribeConfigurationAggregatorSourcesStatus (..),
    mkDescribeConfigurationAggregatorSourcesStatus,

    -- ** Request lenses
    dcassNextToken,
    dcassLimit,
    dcassUpdateStatus,
    dcassConfigurationAggregatorName,

    -- * Destructuring the response
    DescribeConfigurationAggregatorSourcesStatusResponse (..),
    mkDescribeConfigurationAggregatorSourcesStatusResponse,

    -- ** Response lenses
    dcassrsAggregatedSourceStatusList,
    dcassrsNextToken,
    dcassrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeConfigurationAggregatorSourcesStatus' smart constructor.
data DescribeConfigurationAggregatorSourcesStatus = DescribeConfigurationAggregatorSourcesStatus'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    limit ::
      Lude.Maybe
        Lude.Natural,
    updateStatus ::
      Lude.Maybe
        ( Lude.NonEmpty
            AggregatedSourceStatusType
        ),
    configurationAggregatorName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfigurationAggregatorSourcesStatus' with the minimum fields required to make a request.
--
-- * 'configurationAggregatorName' - The name of the configuration aggregator.
-- * 'limit' - The maximum number of AggregatorSourceStatus returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'updateStatus' - Filters the status type.
--
--
--     * Valid value FAILED indicates errors while moving data.
--
--
--     * Valid value SUCCEEDED indicates the data was successfully moved.
--
--
--     * Valid value OUTDATED indicates the data is not the most recent.
mkDescribeConfigurationAggregatorSourcesStatus ::
  -- | 'configurationAggregatorName'
  Lude.Text ->
  DescribeConfigurationAggregatorSourcesStatus
mkDescribeConfigurationAggregatorSourcesStatus
  pConfigurationAggregatorName_ =
    DescribeConfigurationAggregatorSourcesStatus'
      { nextToken =
          Lude.Nothing,
        limit = Lude.Nothing,
        updateStatus = Lude.Nothing,
        configurationAggregatorName =
          pConfigurationAggregatorName_
      }

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcassNextToken :: Lens.Lens' DescribeConfigurationAggregatorSourcesStatus (Lude.Maybe Lude.Text)
dcassNextToken = Lens.lens (nextToken :: DescribeConfigurationAggregatorSourcesStatus -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeConfigurationAggregatorSourcesStatus)
{-# DEPRECATED dcassNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of AggregatorSourceStatus returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcassLimit :: Lens.Lens' DescribeConfigurationAggregatorSourcesStatus (Lude.Maybe Lude.Natural)
dcassLimit = Lens.lens (limit :: DescribeConfigurationAggregatorSourcesStatus -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeConfigurationAggregatorSourcesStatus)
{-# DEPRECATED dcassLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Filters the status type.
--
--
--     * Valid value FAILED indicates errors while moving data.
--
--
--     * Valid value SUCCEEDED indicates the data was successfully moved.
--
--
--     * Valid value OUTDATED indicates the data is not the most recent.
--
--
--
-- /Note:/ Consider using 'updateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcassUpdateStatus :: Lens.Lens' DescribeConfigurationAggregatorSourcesStatus (Lude.Maybe (Lude.NonEmpty AggregatedSourceStatusType))
dcassUpdateStatus = Lens.lens (updateStatus :: DescribeConfigurationAggregatorSourcesStatus -> Lude.Maybe (Lude.NonEmpty AggregatedSourceStatusType)) (\s a -> s {updateStatus = a} :: DescribeConfigurationAggregatorSourcesStatus)
{-# DEPRECATED dcassUpdateStatus "Use generic-lens or generic-optics with 'updateStatus' instead." #-}

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcassConfigurationAggregatorName :: Lens.Lens' DescribeConfigurationAggregatorSourcesStatus Lude.Text
dcassConfigurationAggregatorName = Lens.lens (configurationAggregatorName :: DescribeConfigurationAggregatorSourcesStatus -> Lude.Text) (\s a -> s {configurationAggregatorName = a} :: DescribeConfigurationAggregatorSourcesStatus)
{-# DEPRECATED dcassConfigurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead." #-}

instance Page.AWSPager DescribeConfigurationAggregatorSourcesStatus where
  page rq rs
    | Page.stop (rs Lens.^. dcassrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dcassrsAggregatedSourceStatusList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcassNextToken Lens..~ rs Lens.^. dcassrsNextToken

instance
  Lude.AWSRequest
    DescribeConfigurationAggregatorSourcesStatus
  where
  type
    Rs DescribeConfigurationAggregatorSourcesStatus =
      DescribeConfigurationAggregatorSourcesStatusResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeConfigurationAggregatorSourcesStatusResponse'
            Lude.<$> (x Lude..?> "AggregatedSourceStatusList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    DescribeConfigurationAggregatorSourcesStatus
  where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeConfigurationAggregatorSourcesStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeConfigurationAggregatorSourcesStatus where
  toJSON DescribeConfigurationAggregatorSourcesStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            ("UpdateStatus" Lude..=) Lude.<$> updateStatus,
            Lude.Just
              ( "ConfigurationAggregatorName"
                  Lude..= configurationAggregatorName
              )
          ]
      )

instance Lude.ToPath DescribeConfigurationAggregatorSourcesStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeConfigurationAggregatorSourcesStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeConfigurationAggregatorSourcesStatusResponse' smart constructor.
data DescribeConfigurationAggregatorSourcesStatusResponse = DescribeConfigurationAggregatorSourcesStatusResponse'
  { aggregatedSourceStatusList ::
      Lude.Maybe
        [AggregatedSourceStatus],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DescribeConfigurationAggregatorSourcesStatusResponse' with the minimum fields required to make a request.
--
-- * 'aggregatedSourceStatusList' - Returns an AggregatedSourceStatus object.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'responseStatus' - The response status code.
mkDescribeConfigurationAggregatorSourcesStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeConfigurationAggregatorSourcesStatusResponse
mkDescribeConfigurationAggregatorSourcesStatusResponse
  pResponseStatus_ =
    DescribeConfigurationAggregatorSourcesStatusResponse'
      { aggregatedSourceStatusList =
          Lude.Nothing,
        nextToken = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Returns an AggregatedSourceStatus object.
--
-- /Note:/ Consider using 'aggregatedSourceStatusList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcassrsAggregatedSourceStatusList :: Lens.Lens' DescribeConfigurationAggregatorSourcesStatusResponse (Lude.Maybe [AggregatedSourceStatus])
dcassrsAggregatedSourceStatusList = Lens.lens (aggregatedSourceStatusList :: DescribeConfigurationAggregatorSourcesStatusResponse -> Lude.Maybe [AggregatedSourceStatus]) (\s a -> s {aggregatedSourceStatusList = a} :: DescribeConfigurationAggregatorSourcesStatusResponse)
{-# DEPRECATED dcassrsAggregatedSourceStatusList "Use generic-lens or generic-optics with 'aggregatedSourceStatusList' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcassrsNextToken :: Lens.Lens' DescribeConfigurationAggregatorSourcesStatusResponse (Lude.Maybe Lude.Text)
dcassrsNextToken = Lens.lens (nextToken :: DescribeConfigurationAggregatorSourcesStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeConfigurationAggregatorSourcesStatusResponse)
{-# DEPRECATED dcassrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcassrsResponseStatus :: Lens.Lens' DescribeConfigurationAggregatorSourcesStatusResponse Lude.Int
dcassrsResponseStatus = Lens.lens (responseStatus :: DescribeConfigurationAggregatorSourcesStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeConfigurationAggregatorSourcesStatusResponse)
{-# DEPRECATED dcassrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
