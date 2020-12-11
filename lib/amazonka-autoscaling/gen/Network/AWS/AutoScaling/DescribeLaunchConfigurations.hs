{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeLaunchConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more launch configurations.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeLaunchConfigurations
  ( -- * Creating a request
    DescribeLaunchConfigurations (..),
    mkDescribeLaunchConfigurations,

    -- ** Request lenses
    dlcLaunchConfigurationNames,
    dlcNextToken,
    dlcMaxRecords,

    -- * Destructuring the response
    DescribeLaunchConfigurationsResponse (..),
    mkDescribeLaunchConfigurationsResponse,

    -- ** Response lenses
    dlcrsNextToken,
    dlcrsResponseStatus,
    dlcrsLaunchConfigurations,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLaunchConfigurations' smart constructor.
data DescribeLaunchConfigurations = DescribeLaunchConfigurations'
  { launchConfigurationNames ::
      Lude.Maybe [Lude.Text],
    nextToken :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLaunchConfigurations' with the minimum fields required to make a request.
--
-- * 'launchConfigurationNames' - The launch configuration names. If you omit this parameter, all launch configurations are described.
-- * 'maxRecords' - The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
mkDescribeLaunchConfigurations ::
  DescribeLaunchConfigurations
mkDescribeLaunchConfigurations =
  DescribeLaunchConfigurations'
    { launchConfigurationNames =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The launch configuration names. If you omit this parameter, all launch configurations are described.
--
-- /Note:/ Consider using 'launchConfigurationNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcLaunchConfigurationNames :: Lens.Lens' DescribeLaunchConfigurations (Lude.Maybe [Lude.Text])
dlcLaunchConfigurationNames = Lens.lens (launchConfigurationNames :: DescribeLaunchConfigurations -> Lude.Maybe [Lude.Text]) (\s a -> s {launchConfigurationNames = a} :: DescribeLaunchConfigurations)
{-# DEPRECATED dlcLaunchConfigurationNames "Use generic-lens or generic-optics with 'launchConfigurationNames' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcNextToken :: Lens.Lens' DescribeLaunchConfigurations (Lude.Maybe Lude.Text)
dlcNextToken = Lens.lens (nextToken :: DescribeLaunchConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLaunchConfigurations)
{-# DEPRECATED dlcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcMaxRecords :: Lens.Lens' DescribeLaunchConfigurations (Lude.Maybe Lude.Int)
dlcMaxRecords = Lens.lens (maxRecords :: DescribeLaunchConfigurations -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeLaunchConfigurations)
{-# DEPRECATED dlcMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeLaunchConfigurations where
  page rq rs
    | Page.stop (rs Lens.^. dlcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dlcrsLaunchConfigurations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dlcNextToken Lens..~ rs Lens.^. dlcrsNextToken

instance Lude.AWSRequest DescribeLaunchConfigurations where
  type
    Rs DescribeLaunchConfigurations =
      DescribeLaunchConfigurationsResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DescribeLaunchConfigurationsResult"
      ( \s h x ->
          DescribeLaunchConfigurationsResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "LaunchConfigurations" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
      )

instance Lude.ToHeaders DescribeLaunchConfigurations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeLaunchConfigurations where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLaunchConfigurations where
  toQuery DescribeLaunchConfigurations' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeLaunchConfigurations" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "LaunchConfigurationNames"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> launchConfigurationNames),
        "NextToken" Lude.=: nextToken,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | /See:/ 'mkDescribeLaunchConfigurationsResponse' smart constructor.
data DescribeLaunchConfigurationsResponse = DescribeLaunchConfigurationsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int,
    launchConfigurations ::
      [LaunchConfiguration]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLaunchConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'launchConfigurations' - The launch configurations.
-- * 'nextToken' - A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
-- * 'responseStatus' - The response status code.
mkDescribeLaunchConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLaunchConfigurationsResponse
mkDescribeLaunchConfigurationsResponse pResponseStatus_ =
  DescribeLaunchConfigurationsResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      launchConfigurations = Lude.mempty
    }

-- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrsNextToken :: Lens.Lens' DescribeLaunchConfigurationsResponse (Lude.Maybe Lude.Text)
dlcrsNextToken = Lens.lens (nextToken :: DescribeLaunchConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLaunchConfigurationsResponse)
{-# DEPRECATED dlcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrsResponseStatus :: Lens.Lens' DescribeLaunchConfigurationsResponse Lude.Int
dlcrsResponseStatus = Lens.lens (responseStatus :: DescribeLaunchConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLaunchConfigurationsResponse)
{-# DEPRECATED dlcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The launch configurations.
--
-- /Note:/ Consider using 'launchConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrsLaunchConfigurations :: Lens.Lens' DescribeLaunchConfigurationsResponse [LaunchConfiguration]
dlcrsLaunchConfigurations = Lens.lens (launchConfigurations :: DescribeLaunchConfigurationsResponse -> [LaunchConfiguration]) (\s a -> s {launchConfigurations = a} :: DescribeLaunchConfigurationsResponse)
{-# DEPRECATED dlcrsLaunchConfigurations "Use generic-lens or generic-optics with 'launchConfigurations' instead." #-}
