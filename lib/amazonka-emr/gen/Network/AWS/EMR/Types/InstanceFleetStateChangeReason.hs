{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleetStateChangeReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.InstanceFleetStateChangeReason
  ( InstanceFleetStateChangeReason (..)
  -- * Smart constructor
  , mkInstanceFleetStateChangeReason
  -- * Lenses
  , ifscrCode
  , ifscrMessage
  ) where

import qualified Network.AWS.EMR.Types.InstanceFleetStateChangeReasonCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides status change reason details for the instance fleet.
--
-- /See:/ 'mkInstanceFleetStateChangeReason' smart constructor.
data InstanceFleetStateChangeReason = InstanceFleetStateChangeReason'
  { code :: Core.Maybe Types.InstanceFleetStateChangeReasonCode
    -- ^ A code corresponding to the reason the state change occurred.
  , message :: Core.Maybe Core.Text
    -- ^ An explanatory message.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceFleetStateChangeReason' value with any optional fields omitted.
mkInstanceFleetStateChangeReason
    :: InstanceFleetStateChangeReason
mkInstanceFleetStateChangeReason
  = InstanceFleetStateChangeReason'{code = Core.Nothing,
                                    message = Core.Nothing}

-- | A code corresponding to the reason the state change occurred.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifscrCode :: Lens.Lens' InstanceFleetStateChangeReason (Core.Maybe Types.InstanceFleetStateChangeReasonCode)
ifscrCode = Lens.field @"code"
{-# INLINEABLE ifscrCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | An explanatory message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifscrMessage :: Lens.Lens' InstanceFleetStateChangeReason (Core.Maybe Core.Text)
ifscrMessage = Lens.field @"message"
{-# INLINEABLE ifscrMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromJSON InstanceFleetStateChangeReason where
        parseJSON
          = Core.withObject "InstanceFleetStateChangeReason" Core.$
              \ x ->
                InstanceFleetStateChangeReason' Core.<$>
                  (x Core..:? "Code") Core.<*> x Core..:? "Message"
