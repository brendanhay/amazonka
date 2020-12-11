-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.SecurityGroupRemediationAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.SecurityGroupRemediationAction
  ( SecurityGroupRemediationAction (..),

    -- * Smart constructor
    mkSecurityGroupRemediationAction,

    -- * Lenses
    sgraIsDefaultAction,
    sgraRemediationResult,
    sgraDescription,
    sgraRemediationActionType,
  )
where

import Network.AWS.FMS.Types.RemediationActionType
import Network.AWS.FMS.Types.SecurityGroupRuleDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Remediation option for the rule specified in the @ViolationTarget@ .
--
-- /See:/ 'mkSecurityGroupRemediationAction' smart constructor.
data SecurityGroupRemediationAction = SecurityGroupRemediationAction'
  { isDefaultAction ::
      Lude.Maybe Lude.Bool,
    remediationResult ::
      Lude.Maybe
        SecurityGroupRuleDescription,
    description ::
      Lude.Maybe Lude.Text,
    remediationActionType ::
      Lude.Maybe
        RemediationActionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SecurityGroupRemediationAction' with the minimum fields required to make a request.
--
-- * 'description' - Brief description of the action that will be performed.
-- * 'isDefaultAction' - Indicates if the current action is the default action.
-- * 'remediationActionType' - The remediation action that will be performed.
-- * 'remediationResult' - The final state of the rule specified in the @ViolationTarget@ after it is remediated.
mkSecurityGroupRemediationAction ::
  SecurityGroupRemediationAction
mkSecurityGroupRemediationAction =
  SecurityGroupRemediationAction'
    { isDefaultAction = Lude.Nothing,
      remediationResult = Lude.Nothing,
      description = Lude.Nothing,
      remediationActionType = Lude.Nothing
    }

-- | Indicates if the current action is the default action.
--
-- /Note:/ Consider using 'isDefaultAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgraIsDefaultAction :: Lens.Lens' SecurityGroupRemediationAction (Lude.Maybe Lude.Bool)
sgraIsDefaultAction = Lens.lens (isDefaultAction :: SecurityGroupRemediationAction -> Lude.Maybe Lude.Bool) (\s a -> s {isDefaultAction = a} :: SecurityGroupRemediationAction)
{-# DEPRECATED sgraIsDefaultAction "Use generic-lens or generic-optics with 'isDefaultAction' instead." #-}

-- | The final state of the rule specified in the @ViolationTarget@ after it is remediated.
--
-- /Note:/ Consider using 'remediationResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgraRemediationResult :: Lens.Lens' SecurityGroupRemediationAction (Lude.Maybe SecurityGroupRuleDescription)
sgraRemediationResult = Lens.lens (remediationResult :: SecurityGroupRemediationAction -> Lude.Maybe SecurityGroupRuleDescription) (\s a -> s {remediationResult = a} :: SecurityGroupRemediationAction)
{-# DEPRECATED sgraRemediationResult "Use generic-lens or generic-optics with 'remediationResult' instead." #-}

-- | Brief description of the action that will be performed.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgraDescription :: Lens.Lens' SecurityGroupRemediationAction (Lude.Maybe Lude.Text)
sgraDescription = Lens.lens (description :: SecurityGroupRemediationAction -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: SecurityGroupRemediationAction)
{-# DEPRECATED sgraDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The remediation action that will be performed.
--
-- /Note:/ Consider using 'remediationActionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgraRemediationActionType :: Lens.Lens' SecurityGroupRemediationAction (Lude.Maybe RemediationActionType)
sgraRemediationActionType = Lens.lens (remediationActionType :: SecurityGroupRemediationAction -> Lude.Maybe RemediationActionType) (\s a -> s {remediationActionType = a} :: SecurityGroupRemediationAction)
{-# DEPRECATED sgraRemediationActionType "Use generic-lens or generic-optics with 'remediationActionType' instead." #-}

instance Lude.FromJSON SecurityGroupRemediationAction where
  parseJSON =
    Lude.withObject
      "SecurityGroupRemediationAction"
      ( \x ->
          SecurityGroupRemediationAction'
            Lude.<$> (x Lude..:? "IsDefaultAction")
            Lude.<*> (x Lude..:? "RemediationResult")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "RemediationActionType")
      )
