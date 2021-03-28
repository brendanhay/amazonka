{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroupStateChangeReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.InstanceGroupStateChangeReason
  ( InstanceGroupStateChangeReason (..)
  -- * Smart constructor
  , mkInstanceGroupStateChangeReason
  -- * Lenses
  , igscrCode
  , igscrMessage
  ) where

import qualified Network.AWS.EMR.Types.InstanceGroupStateChangeReasonCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The status change reason details for the instance group.
--
-- /See:/ 'mkInstanceGroupStateChangeReason' smart constructor.
data InstanceGroupStateChangeReason = InstanceGroupStateChangeReason'
  { code :: Core.Maybe Types.InstanceGroupStateChangeReasonCode
    -- ^ The programmable code for the state change reason.
  , message :: Core.Maybe Core.Text
    -- ^ The status change reason description.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceGroupStateChangeReason' value with any optional fields omitted.
mkInstanceGroupStateChangeReason
    :: InstanceGroupStateChangeReason
mkInstanceGroupStateChangeReason
  = InstanceGroupStateChangeReason'{code = Core.Nothing,
                                    message = Core.Nothing}

-- | The programmable code for the state change reason.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igscrCode :: Lens.Lens' InstanceGroupStateChangeReason (Core.Maybe Types.InstanceGroupStateChangeReasonCode)
igscrCode = Lens.field @"code"
{-# INLINEABLE igscrCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The status change reason description.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igscrMessage :: Lens.Lens' InstanceGroupStateChangeReason (Core.Maybe Core.Text)
igscrMessage = Lens.field @"message"
{-# INLINEABLE igscrMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromJSON InstanceGroupStateChangeReason where
        parseJSON
          = Core.withObject "InstanceGroupStateChangeReason" Core.$
              \ x ->
                InstanceGroupStateChangeReason' Core.<$>
                  (x Core..:? "Code") Core.<*> x Core..:? "Message"
