{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.PartnerEventSourceAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.PartnerEventSourceAccount
  ( PartnerEventSourceAccount (..),

    -- * Smart constructor
    mkPartnerEventSourceAccount,

    -- * Lenses
    pesaCreationTime,
    pesaState,
    pesaAccount,
    pesaExpirationTime,
  )
where

import Network.AWS.CloudWatchEvents.Types.EventSourceState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The AWS account that a partner event source has been offered to.
--
-- /See:/ 'mkPartnerEventSourceAccount' smart constructor.
data PartnerEventSourceAccount = PartnerEventSourceAccount'
  { -- | The date and time the event source was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The state of the event source. If it is ACTIVE, you have already created a matching event bus for this event source, and that event bus is active. If it is PENDING, either you haven't yet created a matching event bus, or that event bus is deactivated. If it is DELETED, you have created a matching event bus, but the event source has since been deleted.
    state :: Lude.Maybe EventSourceState,
    -- | The AWS account ID that the partner event source was offered to.
    account :: Lude.Maybe Lude.Text,
    -- | The date and time that the event source will expire, if the AWS account doesn't create a matching event bus for it.
    expirationTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PartnerEventSourceAccount' with the minimum fields required to make a request.
--
-- * 'creationTime' - The date and time the event source was created.
-- * 'state' - The state of the event source. If it is ACTIVE, you have already created a matching event bus for this event source, and that event bus is active. If it is PENDING, either you haven't yet created a matching event bus, or that event bus is deactivated. If it is DELETED, you have created a matching event bus, but the event source has since been deleted.
-- * 'account' - The AWS account ID that the partner event source was offered to.
-- * 'expirationTime' - The date and time that the event source will expire, if the AWS account doesn't create a matching event bus for it.
mkPartnerEventSourceAccount ::
  PartnerEventSourceAccount
mkPartnerEventSourceAccount =
  PartnerEventSourceAccount'
    { creationTime = Lude.Nothing,
      state = Lude.Nothing,
      account = Lude.Nothing,
      expirationTime = Lude.Nothing
    }

-- | The date and time the event source was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesaCreationTime :: Lens.Lens' PartnerEventSourceAccount (Lude.Maybe Lude.Timestamp)
pesaCreationTime = Lens.lens (creationTime :: PartnerEventSourceAccount -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: PartnerEventSourceAccount)
{-# DEPRECATED pesaCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The state of the event source. If it is ACTIVE, you have already created a matching event bus for this event source, and that event bus is active. If it is PENDING, either you haven't yet created a matching event bus, or that event bus is deactivated. If it is DELETED, you have created a matching event bus, but the event source has since been deleted.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesaState :: Lens.Lens' PartnerEventSourceAccount (Lude.Maybe EventSourceState)
pesaState = Lens.lens (state :: PartnerEventSourceAccount -> Lude.Maybe EventSourceState) (\s a -> s {state = a} :: PartnerEventSourceAccount)
{-# DEPRECATED pesaState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The AWS account ID that the partner event source was offered to.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesaAccount :: Lens.Lens' PartnerEventSourceAccount (Lude.Maybe Lude.Text)
pesaAccount = Lens.lens (account :: PartnerEventSourceAccount -> Lude.Maybe Lude.Text) (\s a -> s {account = a} :: PartnerEventSourceAccount)
{-# DEPRECATED pesaAccount "Use generic-lens or generic-optics with 'account' instead." #-}

-- | The date and time that the event source will expire, if the AWS account doesn't create a matching event bus for it.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesaExpirationTime :: Lens.Lens' PartnerEventSourceAccount (Lude.Maybe Lude.Timestamp)
pesaExpirationTime = Lens.lens (expirationTime :: PartnerEventSourceAccount -> Lude.Maybe Lude.Timestamp) (\s a -> s {expirationTime = a} :: PartnerEventSourceAccount)
{-# DEPRECATED pesaExpirationTime "Use generic-lens or generic-optics with 'expirationTime' instead." #-}

instance Lude.FromJSON PartnerEventSourceAccount where
  parseJSON =
    Lude.withObject
      "PartnerEventSourceAccount"
      ( \x ->
          PartnerEventSourceAccount'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "Account")
            Lude.<*> (x Lude..:? "ExpirationTime")
      )
