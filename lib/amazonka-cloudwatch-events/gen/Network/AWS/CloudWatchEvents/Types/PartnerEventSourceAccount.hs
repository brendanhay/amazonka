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
    pesaAccount,
    pesaCreationTime,
    pesaExpirationTime,
    pesaState,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types.Account as Types
import qualified Network.AWS.CloudWatchEvents.Types.EventSourceState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The AWS account that a partner event source has been offered to.
--
-- /See:/ 'mkPartnerEventSourceAccount' smart constructor.
data PartnerEventSourceAccount = PartnerEventSourceAccount'
  { -- | The AWS account ID that the partner event source was offered to.
    account :: Core.Maybe Types.Account,
    -- | The date and time the event source was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The date and time that the event source will expire, if the AWS account doesn't create a matching event bus for it.
    expirationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The state of the event source. If it is ACTIVE, you have already created a matching event bus for this event source, and that event bus is active. If it is PENDING, either you haven't yet created a matching event bus, or that event bus is deactivated. If it is DELETED, you have created a matching event bus, but the event source has since been deleted.
    state :: Core.Maybe Types.EventSourceState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PartnerEventSourceAccount' value with any optional fields omitted.
mkPartnerEventSourceAccount ::
  PartnerEventSourceAccount
mkPartnerEventSourceAccount =
  PartnerEventSourceAccount'
    { account = Core.Nothing,
      creationTime = Core.Nothing,
      expirationTime = Core.Nothing,
      state = Core.Nothing
    }

-- | The AWS account ID that the partner event source was offered to.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesaAccount :: Lens.Lens' PartnerEventSourceAccount (Core.Maybe Types.Account)
pesaAccount = Lens.field @"account"
{-# DEPRECATED pesaAccount "Use generic-lens or generic-optics with 'account' instead." #-}

-- | The date and time the event source was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesaCreationTime :: Lens.Lens' PartnerEventSourceAccount (Core.Maybe Core.NominalDiffTime)
pesaCreationTime = Lens.field @"creationTime"
{-# DEPRECATED pesaCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The date and time that the event source will expire, if the AWS account doesn't create a matching event bus for it.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesaExpirationTime :: Lens.Lens' PartnerEventSourceAccount (Core.Maybe Core.NominalDiffTime)
pesaExpirationTime = Lens.field @"expirationTime"
{-# DEPRECATED pesaExpirationTime "Use generic-lens or generic-optics with 'expirationTime' instead." #-}

-- | The state of the event source. If it is ACTIVE, you have already created a matching event bus for this event source, and that event bus is active. If it is PENDING, either you haven't yet created a matching event bus, or that event bus is deactivated. If it is DELETED, you have created a matching event bus, but the event source has since been deleted.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesaState :: Lens.Lens' PartnerEventSourceAccount (Core.Maybe Types.EventSourceState)
pesaState = Lens.field @"state"
{-# DEPRECATED pesaState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromJSON PartnerEventSourceAccount where
  parseJSON =
    Core.withObject "PartnerEventSourceAccount" Core.$
      \x ->
        PartnerEventSourceAccount'
          Core.<$> (x Core..:? "Account")
          Core.<*> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "ExpirationTime")
          Core.<*> (x Core..:? "State")
