{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSpotDatafeedSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the data feed for Spot Instances. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-data-feeds.html Spot Instance data feed> in the /Amazon EC2 User Guide for Linux Instances/ .
module Network.AWS.EC2.DescribeSpotDatafeedSubscription
  ( -- * Creating a request
    DescribeSpotDatafeedSubscription (..),
    mkDescribeSpotDatafeedSubscription,

    -- ** Request lenses
    dsdsDryRun,

    -- * Destructuring the response
    DescribeSpotDatafeedSubscriptionResponse (..),
    mkDescribeSpotDatafeedSubscriptionResponse,

    -- ** Response lenses
    dsdsrsSpotDatafeedSubscription,
    dsdsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeSpotDatafeedSubscription.
--
-- /See:/ 'mkDescribeSpotDatafeedSubscription' smart constructor.
newtype DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSpotDatafeedSubscription' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDescribeSpotDatafeedSubscription ::
  DescribeSpotDatafeedSubscription
mkDescribeSpotDatafeedSubscription =
  DescribeSpotDatafeedSubscription' {dryRun = Lude.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsDryRun :: Lens.Lens' DescribeSpotDatafeedSubscription (Lude.Maybe Lude.Bool)
dsdsDryRun = Lens.lens (dryRun :: DescribeSpotDatafeedSubscription -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeSpotDatafeedSubscription)
{-# DEPRECATED dsdsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DescribeSpotDatafeedSubscription where
  type
    Rs DescribeSpotDatafeedSubscription =
      DescribeSpotDatafeedSubscriptionResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeSpotDatafeedSubscriptionResponse'
            Lude.<$> (x Lude..@? "spotDatafeedSubscription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSpotDatafeedSubscription where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeSpotDatafeedSubscription where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSpotDatafeedSubscription where
  toQuery DescribeSpotDatafeedSubscription' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeSpotDatafeedSubscription" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun
      ]

-- | Contains the output of DescribeSpotDatafeedSubscription.
--
-- /See:/ 'mkDescribeSpotDatafeedSubscriptionResponse' smart constructor.
data DescribeSpotDatafeedSubscriptionResponse = DescribeSpotDatafeedSubscriptionResponse'
  { -- | The Spot Instance data feed subscription.
    spotDatafeedSubscription :: Lude.Maybe SpotDatafeedSubscription,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSpotDatafeedSubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'spotDatafeedSubscription' - The Spot Instance data feed subscription.
-- * 'responseStatus' - The response status code.
mkDescribeSpotDatafeedSubscriptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSpotDatafeedSubscriptionResponse
mkDescribeSpotDatafeedSubscriptionResponse pResponseStatus_ =
  DescribeSpotDatafeedSubscriptionResponse'
    { spotDatafeedSubscription =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Spot Instance data feed subscription.
--
-- /Note:/ Consider using 'spotDatafeedSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsrsSpotDatafeedSubscription :: Lens.Lens' DescribeSpotDatafeedSubscriptionResponse (Lude.Maybe SpotDatafeedSubscription)
dsdsrsSpotDatafeedSubscription = Lens.lens (spotDatafeedSubscription :: DescribeSpotDatafeedSubscriptionResponse -> Lude.Maybe SpotDatafeedSubscription) (\s a -> s {spotDatafeedSubscription = a} :: DescribeSpotDatafeedSubscriptionResponse)
{-# DEPRECATED dsdsrsSpotDatafeedSubscription "Use generic-lens or generic-optics with 'spotDatafeedSubscription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsrsResponseStatus :: Lens.Lens' DescribeSpotDatafeedSubscriptionResponse Lude.Int
dsdsrsResponseStatus = Lens.lens (responseStatus :: DescribeSpotDatafeedSubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSpotDatafeedSubscriptionResponse)
{-# DEPRECATED dsdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
