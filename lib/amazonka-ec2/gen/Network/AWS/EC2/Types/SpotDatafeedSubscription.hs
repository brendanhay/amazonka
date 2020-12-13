{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotDatafeedSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotDatafeedSubscription
  ( SpotDatafeedSubscription (..),

    -- * Smart constructor
    mkSpotDatafeedSubscription,

    -- * Lenses
    sdsState,
    sdsPrefix,
    sdsBucket,
    sdsOwnerId,
    sdsFault,
  )
where

import Network.AWS.EC2.Types.DatafeedSubscriptionState
import Network.AWS.EC2.Types.SpotInstanceStateFault
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the data feed for a Spot Instance.
--
-- /See:/ 'mkSpotDatafeedSubscription' smart constructor.
data SpotDatafeedSubscription = SpotDatafeedSubscription'
  { -- | The state of the Spot Instance data feed subscription.
    state :: Lude.Maybe DatafeedSubscriptionState,
    -- | The prefix for the data feed files.
    prefix :: Lude.Maybe Lude.Text,
    -- | The name of the Amazon S3 bucket where the Spot Instance data feed is located.
    bucket :: Lude.Maybe Lude.Text,
    -- | The AWS account ID of the account.
    ownerId :: Lude.Maybe Lude.Text,
    -- | The fault codes for the Spot Instance request, if any.
    fault :: Lude.Maybe SpotInstanceStateFault
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SpotDatafeedSubscription' with the minimum fields required to make a request.
--
-- * 'state' - The state of the Spot Instance data feed subscription.
-- * 'prefix' - The prefix for the data feed files.
-- * 'bucket' - The name of the Amazon S3 bucket where the Spot Instance data feed is located.
-- * 'ownerId' - The AWS account ID of the account.
-- * 'fault' - The fault codes for the Spot Instance request, if any.
mkSpotDatafeedSubscription ::
  SpotDatafeedSubscription
mkSpotDatafeedSubscription =
  SpotDatafeedSubscription'
    { state = Lude.Nothing,
      prefix = Lude.Nothing,
      bucket = Lude.Nothing,
      ownerId = Lude.Nothing,
      fault = Lude.Nothing
    }

-- | The state of the Spot Instance data feed subscription.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsState :: Lens.Lens' SpotDatafeedSubscription (Lude.Maybe DatafeedSubscriptionState)
sdsState = Lens.lens (state :: SpotDatafeedSubscription -> Lude.Maybe DatafeedSubscriptionState) (\s a -> s {state = a} :: SpotDatafeedSubscription)
{-# DEPRECATED sdsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The prefix for the data feed files.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsPrefix :: Lens.Lens' SpotDatafeedSubscription (Lude.Maybe Lude.Text)
sdsPrefix = Lens.lens (prefix :: SpotDatafeedSubscription -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: SpotDatafeedSubscription)
{-# DEPRECATED sdsPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The name of the Amazon S3 bucket where the Spot Instance data feed is located.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsBucket :: Lens.Lens' SpotDatafeedSubscription (Lude.Maybe Lude.Text)
sdsBucket = Lens.lens (bucket :: SpotDatafeedSubscription -> Lude.Maybe Lude.Text) (\s a -> s {bucket = a} :: SpotDatafeedSubscription)
{-# DEPRECATED sdsBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The AWS account ID of the account.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsOwnerId :: Lens.Lens' SpotDatafeedSubscription (Lude.Maybe Lude.Text)
sdsOwnerId = Lens.lens (ownerId :: SpotDatafeedSubscription -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: SpotDatafeedSubscription)
{-# DEPRECATED sdsOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The fault codes for the Spot Instance request, if any.
--
-- /Note:/ Consider using 'fault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsFault :: Lens.Lens' SpotDatafeedSubscription (Lude.Maybe SpotInstanceStateFault)
sdsFault = Lens.lens (fault :: SpotDatafeedSubscription -> Lude.Maybe SpotInstanceStateFault) (\s a -> s {fault = a} :: SpotDatafeedSubscription)
{-# DEPRECATED sdsFault "Use generic-lens or generic-optics with 'fault' instead." #-}

instance Lude.FromXML SpotDatafeedSubscription where
  parseXML x =
    SpotDatafeedSubscription'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "prefix")
      Lude.<*> (x Lude..@? "bucket")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "fault")
