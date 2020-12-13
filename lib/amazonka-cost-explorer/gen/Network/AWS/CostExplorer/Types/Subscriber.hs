{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Subscriber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Subscriber
  ( Subscriber (..),

    -- * Smart constructor
    mkSubscriber,

    -- * Lenses
    sStatus,
    sAddress,
    sType,
  )
where

import Network.AWS.CostExplorer.Types.SubscriberStatus
import Network.AWS.CostExplorer.Types.SubscriberType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The recipient of @AnomalySubscription@ notifications.
--
-- /See:/ 'mkSubscriber' smart constructor.
data Subscriber = Subscriber'
  { -- | Indicates if the subscriber accepts the notifications.
    status :: Lude.Maybe SubscriberStatus,
    -- | The email address or SNS Amazon Resource Name (ARN), depending on the @Type@ .
    address :: Lude.Maybe Lude.Text,
    -- | The notification delivery channel.
    type' :: Lude.Maybe SubscriberType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Subscriber' with the minimum fields required to make a request.
--
-- * 'status' - Indicates if the subscriber accepts the notifications.
-- * 'address' - The email address or SNS Amazon Resource Name (ARN), depending on the @Type@ .
-- * 'type'' - The notification delivery channel.
mkSubscriber ::
  Subscriber
mkSubscriber =
  Subscriber'
    { status = Lude.Nothing,
      address = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | Indicates if the subscriber accepts the notifications.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStatus :: Lens.Lens' Subscriber (Lude.Maybe SubscriberStatus)
sStatus = Lens.lens (status :: Subscriber -> Lude.Maybe SubscriberStatus) (\s a -> s {status = a} :: Subscriber)
{-# DEPRECATED sStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The email address or SNS Amazon Resource Name (ARN), depending on the @Type@ .
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAddress :: Lens.Lens' Subscriber (Lude.Maybe Lude.Text)
sAddress = Lens.lens (address :: Subscriber -> Lude.Maybe Lude.Text) (\s a -> s {address = a} :: Subscriber)
{-# DEPRECATED sAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | The notification delivery channel.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sType :: Lens.Lens' Subscriber (Lude.Maybe SubscriberType)
sType = Lens.lens (type' :: Subscriber -> Lude.Maybe SubscriberType) (\s a -> s {type' = a} :: Subscriber)
{-# DEPRECATED sType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON Subscriber where
  parseJSON =
    Lude.withObject
      "Subscriber"
      ( \x ->
          Subscriber'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "Address")
            Lude.<*> (x Lude..:? "Type")
      )

instance Lude.ToJSON Subscriber where
  toJSON Subscriber' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Status" Lude..=) Lude.<$> status,
            ("Address" Lude..=) Lude.<$> address,
            ("Type" Lude..=) Lude.<$> type'
          ]
      )
