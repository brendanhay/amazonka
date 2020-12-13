{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeConfigurationAggregators
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of one or more configuration aggregators. If the configuration aggregator is not specified, this action returns the details for all the configuration aggregators associated with the account.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeConfigurationAggregators
  ( -- * Creating a request
    DescribeConfigurationAggregators (..),
    mkDescribeConfigurationAggregators,

    -- ** Request lenses
    dcaNextToken,
    dcaLimit,
    dcaConfigurationAggregatorNames,

    -- * Destructuring the response
    DescribeConfigurationAggregatorsResponse (..),
    mkDescribeConfigurationAggregatorsResponse,

    -- ** Response lenses
    dcarsNextToken,
    dcarsConfigurationAggregators,
    dcarsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeConfigurationAggregators' smart constructor.
data DescribeConfigurationAggregators = DescribeConfigurationAggregators'
  { -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of configuration aggregators returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
    limit :: Lude.Maybe Lude.Natural,
    -- | The name of the configuration aggregators.
    configurationAggregatorNames :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfigurationAggregators' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'limit' - The maximum number of configuration aggregators returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
-- * 'configurationAggregatorNames' - The name of the configuration aggregators.
mkDescribeConfigurationAggregators ::
  DescribeConfigurationAggregators
mkDescribeConfigurationAggregators =
  DescribeConfigurationAggregators'
    { nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      configurationAggregatorNames = Lude.Nothing
    }

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaNextToken :: Lens.Lens' DescribeConfigurationAggregators (Lude.Maybe Lude.Text)
dcaNextToken = Lens.lens (nextToken :: DescribeConfigurationAggregators -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeConfigurationAggregators)
{-# DEPRECATED dcaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of configuration aggregators returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaLimit :: Lens.Lens' DescribeConfigurationAggregators (Lude.Maybe Lude.Natural)
dcaLimit = Lens.lens (limit :: DescribeConfigurationAggregators -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeConfigurationAggregators)
{-# DEPRECATED dcaLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The name of the configuration aggregators.
--
-- /Note:/ Consider using 'configurationAggregatorNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaConfigurationAggregatorNames :: Lens.Lens' DescribeConfigurationAggregators (Lude.Maybe [Lude.Text])
dcaConfigurationAggregatorNames = Lens.lens (configurationAggregatorNames :: DescribeConfigurationAggregators -> Lude.Maybe [Lude.Text]) (\s a -> s {configurationAggregatorNames = a} :: DescribeConfigurationAggregators)
{-# DEPRECATED dcaConfigurationAggregatorNames "Use generic-lens or generic-optics with 'configurationAggregatorNames' instead." #-}

instance Page.AWSPager DescribeConfigurationAggregators where
  page rq rs
    | Page.stop (rs Lens.^. dcarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dcarsConfigurationAggregators) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcaNextToken Lens..~ rs Lens.^. dcarsNextToken

instance Lude.AWSRequest DescribeConfigurationAggregators where
  type
    Rs DescribeConfigurationAggregators =
      DescribeConfigurationAggregatorsResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeConfigurationAggregatorsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ConfigurationAggregators" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeConfigurationAggregators where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeConfigurationAggregators" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeConfigurationAggregators where
  toJSON DescribeConfigurationAggregators' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            ("ConfigurationAggregatorNames" Lude..=)
              Lude.<$> configurationAggregatorNames
          ]
      )

instance Lude.ToPath DescribeConfigurationAggregators where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeConfigurationAggregators where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeConfigurationAggregatorsResponse' smart constructor.
data DescribeConfigurationAggregatorsResponse = DescribeConfigurationAggregatorsResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Returns a ConfigurationAggregators object.
    configurationAggregators :: Lude.Maybe [ConfigurationAggregator],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfigurationAggregatorsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'configurationAggregators' - Returns a ConfigurationAggregators object.
-- * 'responseStatus' - The response status code.
mkDescribeConfigurationAggregatorsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeConfigurationAggregatorsResponse
mkDescribeConfigurationAggregatorsResponse pResponseStatus_ =
  DescribeConfigurationAggregatorsResponse'
    { nextToken =
        Lude.Nothing,
      configurationAggregators = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcarsNextToken :: Lens.Lens' DescribeConfigurationAggregatorsResponse (Lude.Maybe Lude.Text)
dcarsNextToken = Lens.lens (nextToken :: DescribeConfigurationAggregatorsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeConfigurationAggregatorsResponse)
{-# DEPRECATED dcarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Returns a ConfigurationAggregators object.
--
-- /Note:/ Consider using 'configurationAggregators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcarsConfigurationAggregators :: Lens.Lens' DescribeConfigurationAggregatorsResponse (Lude.Maybe [ConfigurationAggregator])
dcarsConfigurationAggregators = Lens.lens (configurationAggregators :: DescribeConfigurationAggregatorsResponse -> Lude.Maybe [ConfigurationAggregator]) (\s a -> s {configurationAggregators = a} :: DescribeConfigurationAggregatorsResponse)
{-# DEPRECATED dcarsConfigurationAggregators "Use generic-lens or generic-optics with 'configurationAggregators' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcarsResponseStatus :: Lens.Lens' DescribeConfigurationAggregatorsResponse Lude.Int
dcarsResponseStatus = Lens.lens (responseStatus :: DescribeConfigurationAggregatorsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeConfigurationAggregatorsResponse)
{-# DEPRECATED dcarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
