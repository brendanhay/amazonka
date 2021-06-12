{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.SecurityGroupRemediationAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.SecurityGroupRemediationAction where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types.RemediationActionType
import Network.AWS.FMS.Types.SecurityGroupRuleDescription
import qualified Network.AWS.Lens as Lens

-- | Remediation option for the rule specified in the @ViolationTarget@.
--
-- /See:/ 'newSecurityGroupRemediationAction' smart constructor.
data SecurityGroupRemediationAction = SecurityGroupRemediationAction'
  { -- | The remediation action that will be performed.
    remediationActionType :: Core.Maybe RemediationActionType,
    -- | The final state of the rule specified in the @ViolationTarget@ after it
    -- is remediated.
    remediationResult :: Core.Maybe SecurityGroupRuleDescription,
    -- | Indicates if the current action is the default action.
    isDefaultAction :: Core.Maybe Core.Bool,
    -- | Brief description of the action that will be performed.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SecurityGroupRemediationAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remediationActionType', 'securityGroupRemediationAction_remediationActionType' - The remediation action that will be performed.
--
-- 'remediationResult', 'securityGroupRemediationAction_remediationResult' - The final state of the rule specified in the @ViolationTarget@ after it
-- is remediated.
--
-- 'isDefaultAction', 'securityGroupRemediationAction_isDefaultAction' - Indicates if the current action is the default action.
--
-- 'description', 'securityGroupRemediationAction_description' - Brief description of the action that will be performed.
newSecurityGroupRemediationAction ::
  SecurityGroupRemediationAction
newSecurityGroupRemediationAction =
  SecurityGroupRemediationAction'
    { remediationActionType =
        Core.Nothing,
      remediationResult = Core.Nothing,
      isDefaultAction = Core.Nothing,
      description = Core.Nothing
    }

-- | The remediation action that will be performed.
securityGroupRemediationAction_remediationActionType :: Lens.Lens' SecurityGroupRemediationAction (Core.Maybe RemediationActionType)
securityGroupRemediationAction_remediationActionType = Lens.lens (\SecurityGroupRemediationAction' {remediationActionType} -> remediationActionType) (\s@SecurityGroupRemediationAction' {} a -> s {remediationActionType = a} :: SecurityGroupRemediationAction)

-- | The final state of the rule specified in the @ViolationTarget@ after it
-- is remediated.
securityGroupRemediationAction_remediationResult :: Lens.Lens' SecurityGroupRemediationAction (Core.Maybe SecurityGroupRuleDescription)
securityGroupRemediationAction_remediationResult = Lens.lens (\SecurityGroupRemediationAction' {remediationResult} -> remediationResult) (\s@SecurityGroupRemediationAction' {} a -> s {remediationResult = a} :: SecurityGroupRemediationAction)

-- | Indicates if the current action is the default action.
securityGroupRemediationAction_isDefaultAction :: Lens.Lens' SecurityGroupRemediationAction (Core.Maybe Core.Bool)
securityGroupRemediationAction_isDefaultAction = Lens.lens (\SecurityGroupRemediationAction' {isDefaultAction} -> isDefaultAction) (\s@SecurityGroupRemediationAction' {} a -> s {isDefaultAction = a} :: SecurityGroupRemediationAction)

-- | Brief description of the action that will be performed.
securityGroupRemediationAction_description :: Lens.Lens' SecurityGroupRemediationAction (Core.Maybe Core.Text)
securityGroupRemediationAction_description = Lens.lens (\SecurityGroupRemediationAction' {description} -> description) (\s@SecurityGroupRemediationAction' {} a -> s {description = a} :: SecurityGroupRemediationAction)

instance Core.FromJSON SecurityGroupRemediationAction where
  parseJSON =
    Core.withObject
      "SecurityGroupRemediationAction"
      ( \x ->
          SecurityGroupRemediationAction'
            Core.<$> (x Core..:? "RemediationActionType")
            Core.<*> (x Core..:? "RemediationResult")
            Core.<*> (x Core..:? "IsDefaultAction")
            Core.<*> (x Core..:? "Description")
      )

instance Core.Hashable SecurityGroupRemediationAction

instance Core.NFData SecurityGroupRemediationAction
