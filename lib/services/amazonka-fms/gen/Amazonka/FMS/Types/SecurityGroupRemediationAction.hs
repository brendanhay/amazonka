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
-- Module      : Amazonka.FMS.Types.SecurityGroupRemediationAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.SecurityGroupRemediationAction where

import qualified Amazonka.Core as Core
import Amazonka.FMS.Types.RemediationActionType
import Amazonka.FMS.Types.SecurityGroupRuleDescription
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Remediation option for the rule specified in the @ViolationTarget@.
--
-- /See:/ 'newSecurityGroupRemediationAction' smart constructor.
data SecurityGroupRemediationAction = SecurityGroupRemediationAction'
  { -- | Indicates if the current action is the default action.
    isDefaultAction :: Prelude.Maybe Prelude.Bool,
    -- | The final state of the rule specified in the @ViolationTarget@ after it
    -- is remediated.
    remediationResult :: Prelude.Maybe SecurityGroupRuleDescription,
    -- | Brief description of the action that will be performed.
    description :: Prelude.Maybe Prelude.Text,
    -- | The remediation action that will be performed.
    remediationActionType :: Prelude.Maybe RemediationActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityGroupRemediationAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isDefaultAction', 'securityGroupRemediationAction_isDefaultAction' - Indicates if the current action is the default action.
--
-- 'remediationResult', 'securityGroupRemediationAction_remediationResult' - The final state of the rule specified in the @ViolationTarget@ after it
-- is remediated.
--
-- 'description', 'securityGroupRemediationAction_description' - Brief description of the action that will be performed.
--
-- 'remediationActionType', 'securityGroupRemediationAction_remediationActionType' - The remediation action that will be performed.
newSecurityGroupRemediationAction ::
  SecurityGroupRemediationAction
newSecurityGroupRemediationAction =
  SecurityGroupRemediationAction'
    { isDefaultAction =
        Prelude.Nothing,
      remediationResult = Prelude.Nothing,
      description = Prelude.Nothing,
      remediationActionType = Prelude.Nothing
    }

-- | Indicates if the current action is the default action.
securityGroupRemediationAction_isDefaultAction :: Lens.Lens' SecurityGroupRemediationAction (Prelude.Maybe Prelude.Bool)
securityGroupRemediationAction_isDefaultAction = Lens.lens (\SecurityGroupRemediationAction' {isDefaultAction} -> isDefaultAction) (\s@SecurityGroupRemediationAction' {} a -> s {isDefaultAction = a} :: SecurityGroupRemediationAction)

-- | The final state of the rule specified in the @ViolationTarget@ after it
-- is remediated.
securityGroupRemediationAction_remediationResult :: Lens.Lens' SecurityGroupRemediationAction (Prelude.Maybe SecurityGroupRuleDescription)
securityGroupRemediationAction_remediationResult = Lens.lens (\SecurityGroupRemediationAction' {remediationResult} -> remediationResult) (\s@SecurityGroupRemediationAction' {} a -> s {remediationResult = a} :: SecurityGroupRemediationAction)

-- | Brief description of the action that will be performed.
securityGroupRemediationAction_description :: Lens.Lens' SecurityGroupRemediationAction (Prelude.Maybe Prelude.Text)
securityGroupRemediationAction_description = Lens.lens (\SecurityGroupRemediationAction' {description} -> description) (\s@SecurityGroupRemediationAction' {} a -> s {description = a} :: SecurityGroupRemediationAction)

-- | The remediation action that will be performed.
securityGroupRemediationAction_remediationActionType :: Lens.Lens' SecurityGroupRemediationAction (Prelude.Maybe RemediationActionType)
securityGroupRemediationAction_remediationActionType = Lens.lens (\SecurityGroupRemediationAction' {remediationActionType} -> remediationActionType) (\s@SecurityGroupRemediationAction' {} a -> s {remediationActionType = a} :: SecurityGroupRemediationAction)

instance Core.FromJSON SecurityGroupRemediationAction where
  parseJSON =
    Core.withObject
      "SecurityGroupRemediationAction"
      ( \x ->
          SecurityGroupRemediationAction'
            Prelude.<$> (x Core..:? "IsDefaultAction")
            Prelude.<*> (x Core..:? "RemediationResult")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "RemediationActionType")
      )

instance
  Prelude.Hashable
    SecurityGroupRemediationAction

instance
  Prelude.NFData
    SecurityGroupRemediationAction
