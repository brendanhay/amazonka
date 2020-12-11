{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateSpotDatafeedSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a data feed for Spot Instances, enabling you to view Spot Instance usage logs. You can create one data feed per AWS account. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-data-feeds.html Spot Instance data feed> in the /Amazon EC2 User Guide for Linux Instances/ .
module Network.AWS.EC2.CreateSpotDatafeedSubscription
  ( -- * Creating a request
    CreateSpotDatafeedSubscription (..),
    mkCreateSpotDatafeedSubscription,

    -- ** Request lenses
    csdsPrefix,
    csdsDryRun,
    csdsBucket,

    -- * Destructuring the response
    CreateSpotDatafeedSubscriptionResponse (..),
    mkCreateSpotDatafeedSubscriptionResponse,

    -- ** Response lenses
    csdsrsSpotDatafeedSubscription,
    csdsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for CreateSpotDatafeedSubscription.
--
-- /See:/ 'mkCreateSpotDatafeedSubscription' smart constructor.
data CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscription'
  { prefix ::
      Lude.Maybe Lude.Text,
    dryRun ::
      Lude.Maybe Lude.Bool,
    bucket :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSpotDatafeedSubscription' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the Amazon S3 bucket in which to store the Spot Instance data feed. For more information about bucket names, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html#bucketnamingrules Rules for bucket naming> in the /Amazon S3 Developer Guide/ .
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'prefix' - The prefix for the data feed file names.
mkCreateSpotDatafeedSubscription ::
  -- | 'bucket'
  Lude.Text ->
  CreateSpotDatafeedSubscription
mkCreateSpotDatafeedSubscription pBucket_ =
  CreateSpotDatafeedSubscription'
    { prefix = Lude.Nothing,
      dryRun = Lude.Nothing,
      bucket = pBucket_
    }

-- | The prefix for the data feed file names.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdsPrefix :: Lens.Lens' CreateSpotDatafeedSubscription (Lude.Maybe Lude.Text)
csdsPrefix = Lens.lens (prefix :: CreateSpotDatafeedSubscription -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: CreateSpotDatafeedSubscription)
{-# DEPRECATED csdsPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdsDryRun :: Lens.Lens' CreateSpotDatafeedSubscription (Lude.Maybe Lude.Bool)
csdsDryRun = Lens.lens (dryRun :: CreateSpotDatafeedSubscription -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateSpotDatafeedSubscription)
{-# DEPRECATED csdsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The name of the Amazon S3 bucket in which to store the Spot Instance data feed. For more information about bucket names, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html#bucketnamingrules Rules for bucket naming> in the /Amazon S3 Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdsBucket :: Lens.Lens' CreateSpotDatafeedSubscription Lude.Text
csdsBucket = Lens.lens (bucket :: CreateSpotDatafeedSubscription -> Lude.Text) (\s a -> s {bucket = a} :: CreateSpotDatafeedSubscription)
{-# DEPRECATED csdsBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest CreateSpotDatafeedSubscription where
  type
    Rs CreateSpotDatafeedSubscription =
      CreateSpotDatafeedSubscriptionResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateSpotDatafeedSubscriptionResponse'
            Lude.<$> (x Lude..@? "spotDatafeedSubscription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSpotDatafeedSubscription where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateSpotDatafeedSubscription where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateSpotDatafeedSubscription where
  toQuery CreateSpotDatafeedSubscription' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateSpotDatafeedSubscription" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Prefix" Lude.=: prefix,
        "DryRun" Lude.=: dryRun,
        "Bucket" Lude.=: bucket
      ]

-- | Contains the output of CreateSpotDatafeedSubscription.
--
-- /See:/ 'mkCreateSpotDatafeedSubscriptionResponse' smart constructor.
data CreateSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponse'
  { spotDatafeedSubscription ::
      Lude.Maybe
        SpotDatafeedSubscription,
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

-- | Creates a value of 'CreateSpotDatafeedSubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'spotDatafeedSubscription' - The Spot Instance data feed subscription.
mkCreateSpotDatafeedSubscriptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSpotDatafeedSubscriptionResponse
mkCreateSpotDatafeedSubscriptionResponse pResponseStatus_ =
  CreateSpotDatafeedSubscriptionResponse'
    { spotDatafeedSubscription =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Spot Instance data feed subscription.
--
-- /Note:/ Consider using 'spotDatafeedSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdsrsSpotDatafeedSubscription :: Lens.Lens' CreateSpotDatafeedSubscriptionResponse (Lude.Maybe SpotDatafeedSubscription)
csdsrsSpotDatafeedSubscription = Lens.lens (spotDatafeedSubscription :: CreateSpotDatafeedSubscriptionResponse -> Lude.Maybe SpotDatafeedSubscription) (\s a -> s {spotDatafeedSubscription = a} :: CreateSpotDatafeedSubscriptionResponse)
{-# DEPRECATED csdsrsSpotDatafeedSubscription "Use generic-lens or generic-optics with 'spotDatafeedSubscription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdsrsResponseStatus :: Lens.Lens' CreateSpotDatafeedSubscriptionResponse Lude.Int
csdsrsResponseStatus = Lens.lens (responseStatus :: CreateSpotDatafeedSubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSpotDatafeedSubscriptionResponse)
{-# DEPRECATED csdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
