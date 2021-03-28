{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceStateChangeReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.InstanceStateChangeReason
  ( InstanceStateChangeReason (..)
  -- * Smart constructor
  , mkInstanceStateChangeReason
  -- * Lenses
  , iscrCode
  , iscrMessage
  ) where

import qualified Network.AWS.EMR.Types.InstanceStateChangeReasonCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of the status change reason for the instance.
--
-- /See:/ 'mkInstanceStateChangeReason' smart constructor.
data InstanceStateChangeReason = InstanceStateChangeReason'
  { code :: Core.Maybe Types.InstanceStateChangeReasonCode
    -- ^ The programmable code for the state change reason.
  , message :: Core.Maybe Core.Text
    -- ^ The status change reason description.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceStateChangeReason' value with any optional fields omitted.
mkInstanceStateChangeReason
    :: InstanceStateChangeReason
mkInstanceStateChangeReason
  = InstanceStateChangeReason'{code = Core.Nothing,
                               message = Core.Nothing}

-- | The programmable code for the state change reason.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscrCode :: Lens.Lens' InstanceStateChangeReason (Core.Maybe Types.InstanceStateChangeReasonCode)
iscrCode = Lens.field @"code"
{-# INLINEABLE iscrCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The status change reason description.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscrMessage :: Lens.Lens' InstanceStateChangeReason (Core.Maybe Core.Text)
iscrMessage = Lens.field @"message"
{-# INLINEABLE iscrMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromJSON InstanceStateChangeReason where
        parseJSON
          = Core.withObject "InstanceStateChangeReason" Core.$
              \ x ->
                InstanceStateChangeReason' Core.<$>
                  (x Core..:? "Code") Core.<*> x Core..:? "Message"
