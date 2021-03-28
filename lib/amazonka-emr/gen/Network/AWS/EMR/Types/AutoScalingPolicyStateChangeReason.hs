{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReason
  ( AutoScalingPolicyStateChangeReason (..)
  -- * Smart constructor
  , mkAutoScalingPolicyStateChangeReason
  -- * Lenses
  , aspscrCode
  , aspscrMessage
  ) where

import qualified Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReasonCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The reason for an 'AutoScalingPolicyStatus' change.
--
-- /See:/ 'mkAutoScalingPolicyStateChangeReason' smart constructor.
data AutoScalingPolicyStateChangeReason = AutoScalingPolicyStateChangeReason'
  { code :: Core.Maybe Types.AutoScalingPolicyStateChangeReasonCode
    -- ^ The code indicating the reason for the change in status.@USER_REQUEST@ indicates that the scaling policy status was changed by a user. @PROVISION_FAILURE@ indicates that the status change was because the policy failed to provision. @CLEANUP_FAILURE@ indicates an error.
  , message :: Core.Maybe Core.Text
    -- ^ A friendly, more verbose message that accompanies an automatic scaling policy state change.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoScalingPolicyStateChangeReason' value with any optional fields omitted.
mkAutoScalingPolicyStateChangeReason
    :: AutoScalingPolicyStateChangeReason
mkAutoScalingPolicyStateChangeReason
  = AutoScalingPolicyStateChangeReason'{code = Core.Nothing,
                                        message = Core.Nothing}

-- | The code indicating the reason for the change in status.@USER_REQUEST@ indicates that the scaling policy status was changed by a user. @PROVISION_FAILURE@ indicates that the status change was because the policy failed to provision. @CLEANUP_FAILURE@ indicates an error.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspscrCode :: Lens.Lens' AutoScalingPolicyStateChangeReason (Core.Maybe Types.AutoScalingPolicyStateChangeReasonCode)
aspscrCode = Lens.field @"code"
{-# INLINEABLE aspscrCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | A friendly, more verbose message that accompanies an automatic scaling policy state change.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspscrMessage :: Lens.Lens' AutoScalingPolicyStateChangeReason (Core.Maybe Core.Text)
aspscrMessage = Lens.field @"message"
{-# INLINEABLE aspscrMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromJSON AutoScalingPolicyStateChangeReason where
        parseJSON
          = Core.withObject "AutoScalingPolicyStateChangeReason" Core.$
              \ x ->
                AutoScalingPolicyStateChangeReason' Core.<$>
                  (x Core..:? "Code") Core.<*> x Core..:? "Message"
