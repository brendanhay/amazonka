{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.SecurityGroupRemediationAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.SecurityGroupRemediationAction
  ( SecurityGroupRemediationAction (..)
  -- * Smart constructor
  , mkSecurityGroupRemediationAction
  -- * Lenses
  , sgraDescription
  , sgraIsDefaultAction
  , sgraRemediationActionType
  , sgraRemediationResult
  ) where

import qualified Network.AWS.FMS.Types.RemediationActionDescription as Types
import qualified Network.AWS.FMS.Types.RemediationActionType as Types
import qualified Network.AWS.FMS.Types.SecurityGroupRuleDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Remediation option for the rule specified in the @ViolationTarget@ .
--
-- /See:/ 'mkSecurityGroupRemediationAction' smart constructor.
data SecurityGroupRemediationAction = SecurityGroupRemediationAction'
  { description :: Core.Maybe Types.RemediationActionDescription
    -- ^ Brief description of the action that will be performed.
  , isDefaultAction :: Core.Maybe Core.Bool
    -- ^ Indicates if the current action is the default action.
  , remediationActionType :: Core.Maybe Types.RemediationActionType
    -- ^ The remediation action that will be performed.
  , remediationResult :: Core.Maybe Types.SecurityGroupRuleDescription
    -- ^ The final state of the rule specified in the @ViolationTarget@ after it is remediated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SecurityGroupRemediationAction' value with any optional fields omitted.
mkSecurityGroupRemediationAction
    :: SecurityGroupRemediationAction
mkSecurityGroupRemediationAction
  = SecurityGroupRemediationAction'{description = Core.Nothing,
                                    isDefaultAction = Core.Nothing,
                                    remediationActionType = Core.Nothing,
                                    remediationResult = Core.Nothing}

-- | Brief description of the action that will be performed.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgraDescription :: Lens.Lens' SecurityGroupRemediationAction (Core.Maybe Types.RemediationActionDescription)
sgraDescription = Lens.field @"description"
{-# INLINEABLE sgraDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Indicates if the current action is the default action.
--
-- /Note:/ Consider using 'isDefaultAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgraIsDefaultAction :: Lens.Lens' SecurityGroupRemediationAction (Core.Maybe Core.Bool)
sgraIsDefaultAction = Lens.field @"isDefaultAction"
{-# INLINEABLE sgraIsDefaultAction #-}
{-# DEPRECATED isDefaultAction "Use generic-lens or generic-optics with 'isDefaultAction' instead"  #-}

-- | The remediation action that will be performed.
--
-- /Note:/ Consider using 'remediationActionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgraRemediationActionType :: Lens.Lens' SecurityGroupRemediationAction (Core.Maybe Types.RemediationActionType)
sgraRemediationActionType = Lens.field @"remediationActionType"
{-# INLINEABLE sgraRemediationActionType #-}
{-# DEPRECATED remediationActionType "Use generic-lens or generic-optics with 'remediationActionType' instead"  #-}

-- | The final state of the rule specified in the @ViolationTarget@ after it is remediated.
--
-- /Note:/ Consider using 'remediationResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgraRemediationResult :: Lens.Lens' SecurityGroupRemediationAction (Core.Maybe Types.SecurityGroupRuleDescription)
sgraRemediationResult = Lens.field @"remediationResult"
{-# INLINEABLE sgraRemediationResult #-}
{-# DEPRECATED remediationResult "Use generic-lens or generic-optics with 'remediationResult' instead"  #-}

instance Core.FromJSON SecurityGroupRemediationAction where
        parseJSON
          = Core.withObject "SecurityGroupRemediationAction" Core.$
              \ x ->
                SecurityGroupRemediationAction' Core.<$>
                  (x Core..:? "Description") Core.<*> x Core..:? "IsDefaultAction"
                    Core.<*> x Core..:? "RemediationActionType"
                    Core.<*> x Core..:? "RemediationResult"
