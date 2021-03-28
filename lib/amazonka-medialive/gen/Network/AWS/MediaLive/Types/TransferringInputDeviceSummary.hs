{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TransferringInputDeviceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.TransferringInputDeviceSummary
  ( TransferringInputDeviceSummary (..)
  -- * Smart constructor
  , mkTransferringInputDeviceSummary
  -- * Lenses
  , tidsId
  , tidsMessage
  , tidsTargetCustomerId
  , tidsTransferType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.InputDeviceTransferType as Types
import qualified Network.AWS.Prelude as Core

-- | Details about the input device that is being transferred.
--
-- /See:/ 'mkTransferringInputDeviceSummary' smart constructor.
data TransferringInputDeviceSummary = TransferringInputDeviceSummary'
  { id :: Core.Maybe Core.Text
    -- ^ The unique ID of the input device.
  , message :: Core.Maybe Core.Text
    -- ^ The optional message that the sender has attached to the transfer.
  , targetCustomerId :: Core.Maybe Core.Text
    -- ^ The AWS account ID for the recipient of the input device transfer.
  , transferType :: Core.Maybe Types.InputDeviceTransferType
    -- ^ The type (direction) of the input device transfer.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransferringInputDeviceSummary' value with any optional fields omitted.
mkTransferringInputDeviceSummary
    :: TransferringInputDeviceSummary
mkTransferringInputDeviceSummary
  = TransferringInputDeviceSummary'{id = Core.Nothing,
                                    message = Core.Nothing, targetCustomerId = Core.Nothing,
                                    transferType = Core.Nothing}

-- | The unique ID of the input device.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidsId :: Lens.Lens' TransferringInputDeviceSummary (Core.Maybe Core.Text)
tidsId = Lens.field @"id"
{-# INLINEABLE tidsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The optional message that the sender has attached to the transfer.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidsMessage :: Lens.Lens' TransferringInputDeviceSummary (Core.Maybe Core.Text)
tidsMessage = Lens.field @"message"
{-# INLINEABLE tidsMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The AWS account ID for the recipient of the input device transfer.
--
-- /Note:/ Consider using 'targetCustomerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidsTargetCustomerId :: Lens.Lens' TransferringInputDeviceSummary (Core.Maybe Core.Text)
tidsTargetCustomerId = Lens.field @"targetCustomerId"
{-# INLINEABLE tidsTargetCustomerId #-}
{-# DEPRECATED targetCustomerId "Use generic-lens or generic-optics with 'targetCustomerId' instead"  #-}

-- | The type (direction) of the input device transfer.
--
-- /Note:/ Consider using 'transferType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidsTransferType :: Lens.Lens' TransferringInputDeviceSummary (Core.Maybe Types.InputDeviceTransferType)
tidsTransferType = Lens.field @"transferType"
{-# INLINEABLE tidsTransferType #-}
{-# DEPRECATED transferType "Use generic-lens or generic-optics with 'transferType' instead"  #-}

instance Core.FromJSON TransferringInputDeviceSummary where
        parseJSON
          = Core.withObject "TransferringInputDeviceSummary" Core.$
              \ x ->
                TransferringInputDeviceSummary' Core.<$>
                  (x Core..:? "id") Core.<*> x Core..:? "message" Core.<*>
                    x Core..:? "targetCustomerId"
                    Core.<*> x Core..:? "transferType"
