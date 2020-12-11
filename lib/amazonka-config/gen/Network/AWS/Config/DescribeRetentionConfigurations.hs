{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeRetentionConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of one or more retention configurations. If the retention configuration name is not specified, this action returns the details for all the retention configurations for that account.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeRetentionConfigurations
  ( -- * Creating a request
    DescribeRetentionConfigurations (..),
    mkDescribeRetentionConfigurations,

    -- ** Request lenses
    drcRetentionConfigurationNames,
    drcNextToken,

    -- * Destructuring the response
    DescribeRetentionConfigurationsResponse (..),
    mkDescribeRetentionConfigurationsResponse,

    -- ** Response lenses
    drsRetentionConfigurations,
    drsNextToken,
    drsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeRetentionConfigurations' smart constructor.
data DescribeRetentionConfigurations = DescribeRetentionConfigurations'
  { retentionConfigurationNames ::
      Lude.Maybe [Lude.Text],
    nextToken ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRetentionConfigurations' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'retentionConfigurationNames' - A list of names of retention configurations for which you want details. If you do not specify a name, AWS Config returns details for all the retention configurations for that account.
mkDescribeRetentionConfigurations ::
  DescribeRetentionConfigurations
mkDescribeRetentionConfigurations =
  DescribeRetentionConfigurations'
    { retentionConfigurationNames =
        Lude.Nothing,
      nextToken = Lude.Nothing
    }

-- | A list of names of retention configurations for which you want details. If you do not specify a name, AWS Config returns details for all the retention configurations for that account.
--
-- /Note:/ Consider using 'retentionConfigurationNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcRetentionConfigurationNames :: Lens.Lens' DescribeRetentionConfigurations (Lude.Maybe [Lude.Text])
drcRetentionConfigurationNames = Lens.lens (retentionConfigurationNames :: DescribeRetentionConfigurations -> Lude.Maybe [Lude.Text]) (\s a -> s {retentionConfigurationNames = a} :: DescribeRetentionConfigurations)
{-# DEPRECATED drcRetentionConfigurationNames "Use generic-lens or generic-optics with 'retentionConfigurationNames' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcNextToken :: Lens.Lens' DescribeRetentionConfigurations (Lude.Maybe Lude.Text)
drcNextToken = Lens.lens (nextToken :: DescribeRetentionConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeRetentionConfigurations)
{-# DEPRECATED drcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager DescribeRetentionConfigurations where
  page rq rs
    | Page.stop (rs Lens.^. drsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. drsRetentionConfigurations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& drcNextToken Lens..~ rs Lens.^. drsNextToken

instance Lude.AWSRequest DescribeRetentionConfigurations where
  type
    Rs DescribeRetentionConfigurations =
      DescribeRetentionConfigurationsResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeRetentionConfigurationsResponse'
            Lude.<$> (x Lude..?> "RetentionConfigurations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeRetentionConfigurations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeRetentionConfigurations" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeRetentionConfigurations where
  toJSON DescribeRetentionConfigurations' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RetentionConfigurationNames" Lude..=)
              Lude.<$> retentionConfigurationNames,
            ("NextToken" Lude..=) Lude.<$> nextToken
          ]
      )

instance Lude.ToPath DescribeRetentionConfigurations where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeRetentionConfigurations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeRetentionConfigurationsResponse' smart constructor.
data DescribeRetentionConfigurationsResponse = DescribeRetentionConfigurationsResponse'
  { retentionConfigurations ::
      Lude.Maybe
        [RetentionConfiguration],
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
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRetentionConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'responseStatus' - The response status code.
-- * 'retentionConfigurations' - Returns a retention configuration object.
mkDescribeRetentionConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeRetentionConfigurationsResponse
mkDescribeRetentionConfigurationsResponse pResponseStatus_ =
  DescribeRetentionConfigurationsResponse'
    { retentionConfigurations =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns a retention configuration object.
--
-- /Note:/ Consider using 'retentionConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsRetentionConfigurations :: Lens.Lens' DescribeRetentionConfigurationsResponse (Lude.Maybe [RetentionConfiguration])
drsRetentionConfigurations = Lens.lens (retentionConfigurations :: DescribeRetentionConfigurationsResponse -> Lude.Maybe [RetentionConfiguration]) (\s a -> s {retentionConfigurations = a} :: DescribeRetentionConfigurationsResponse)
{-# DEPRECATED drsRetentionConfigurations "Use generic-lens or generic-optics with 'retentionConfigurations' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextToken :: Lens.Lens' DescribeRetentionConfigurationsResponse (Lude.Maybe Lude.Text)
drsNextToken = Lens.lens (nextToken :: DescribeRetentionConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeRetentionConfigurationsResponse)
{-# DEPRECATED drsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeRetentionConfigurationsResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeRetentionConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeRetentionConfigurationsResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
