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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.SecurityGroupRemediationAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.RemediationActionType
import Amazonka.FMS.Types.SecurityGroupRuleDescription
import qualified Amazonka.Prelude as Prelude

-- | Remediation option for the rule specified in the @ViolationTarget@.
--
-- /See:/ 'newSecurityGroupRemediationAction' smart constructor.
data SecurityGroupRemediationAction = SecurityGroupRemediationAction'
  { -- | Brief description of the action that will be performed.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates if the current action is the default action.
    isDefaultAction :: Prelude.Maybe Prelude.Bool,
    -- | The remediation action that will be performed.
    remediationActionType :: Prelude.Maybe RemediationActionType,
    -- | The final state of the rule specified in the @ViolationTarget@ after it
    -- is remediated.
    remediationResult :: Prelude.Maybe SecurityGroupRuleDescription
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
-- 'description', 'securityGroupRemediationAction_description' - Brief description of the action that will be performed.
--
-- 'isDefaultAction', 'securityGroupRemediationAction_isDefaultAction' - Indicates if the current action is the default action.
--
-- 'remediationActionType', 'securityGroupRemediationAction_remediationActionType' - The remediation action that will be performed.
--
-- 'remediationResult', 'securityGroupRemediationAction_remediationResult' - The final state of the rule specified in the @ViolationTarget@ after it
-- is remediated.
newSecurityGroupRemediationAction ::
  SecurityGroupRemediationAction
newSecurityGroupRemediationAction =
  SecurityGroupRemediationAction'
    { description =
        Prelude.Nothing,
      isDefaultAction = Prelude.Nothing,
      remediationActionType = Prelude.Nothing,
      remediationResult = Prelude.Nothing
    }

-- | Brief description of the action that will be performed.
securityGroupRemediationAction_description :: Lens.Lens' SecurityGroupRemediationAction (Prelude.Maybe Prelude.Text)
securityGroupRemediationAction_description = Lens.lens (\SecurityGroupRemediationAction' {description} -> description) (\s@SecurityGroupRemediationAction' {} a -> s {description = a} :: SecurityGroupRemediationAction)

-- | Indicates if the current action is the default action.
securityGroupRemediationAction_isDefaultAction :: Lens.Lens' SecurityGroupRemediationAction (Prelude.Maybe Prelude.Bool)
securityGroupRemediationAction_isDefaultAction = Lens.lens (\SecurityGroupRemediationAction' {isDefaultAction} -> isDefaultAction) (\s@SecurityGroupRemediationAction' {} a -> s {isDefaultAction = a} :: SecurityGroupRemediationAction)

-- | The remediation action that will be performed.
securityGroupRemediationAction_remediationActionType :: Lens.Lens' SecurityGroupRemediationAction (Prelude.Maybe RemediationActionType)
securityGroupRemediationAction_remediationActionType = Lens.lens (\SecurityGroupRemediationAction' {remediationActionType} -> remediationActionType) (\s@SecurityGroupRemediationAction' {} a -> s {remediationActionType = a} :: SecurityGroupRemediationAction)

-- | The final state of the rule specified in the @ViolationTarget@ after it
-- is remediated.
securityGroupRemediationAction_remediationResult :: Lens.Lens' SecurityGroupRemediationAction (Prelude.Maybe SecurityGroupRuleDescription)
securityGroupRemediationAction_remediationResult = Lens.lens (\SecurityGroupRemediationAction' {remediationResult} -> remediationResult) (\s@SecurityGroupRemediationAction' {} a -> s {remediationResult = a} :: SecurityGroupRemediationAction)

instance Data.FromJSON SecurityGroupRemediationAction where
  parseJSON =
    Data.withObject
      "SecurityGroupRemediationAction"
      ( \x ->
          SecurityGroupRemediationAction'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "IsDefaultAction")
            Prelude.<*> (x Data..:? "RemediationActionType")
            Prelude.<*> (x Data..:? "RemediationResult")
      )

instance
  Prelude.Hashable
    SecurityGroupRemediationAction
  where
  hashWithSalt
    _salt
    SecurityGroupRemediationAction' {..} =
      _salt `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` isDefaultAction
        `Prelude.hashWithSalt` remediationActionType
        `Prelude.hashWithSalt` remediationResult

instance
  Prelude.NFData
    SecurityGroupRemediationAction
  where
  rnf SecurityGroupRemediationAction' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf isDefaultAction
      `Prelude.seq` Prelude.rnf remediationActionType
      `Prelude.seq` Prelude.rnf remediationResult
