{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.FMS.Types.RemediationActionType
import Network.AWS.FMS.Types.SecurityGroupRuleDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Remediation option for the rule specified in the @ViolationTarget@.
--
-- /See:/ 'newSecurityGroupRemediationAction' smart constructor.
data SecurityGroupRemediationAction = SecurityGroupRemediationAction'
  { -- | The remediation action that will be performed.
    remediationActionType :: Prelude.Maybe RemediationActionType,
    -- | The final state of the rule specified in the @ViolationTarget@ after it
    -- is remediated.
    remediationResult :: Prelude.Maybe SecurityGroupRuleDescription,
    -- | Indicates if the current action is the default action.
    isDefaultAction :: Prelude.Maybe Prelude.Bool,
    -- | Brief description of the action that will be performed.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      remediationResult = Prelude.Nothing,
      isDefaultAction = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The remediation action that will be performed.
securityGroupRemediationAction_remediationActionType :: Lens.Lens' SecurityGroupRemediationAction (Prelude.Maybe RemediationActionType)
securityGroupRemediationAction_remediationActionType = Lens.lens (\SecurityGroupRemediationAction' {remediationActionType} -> remediationActionType) (\s@SecurityGroupRemediationAction' {} a -> s {remediationActionType = a} :: SecurityGroupRemediationAction)

-- | The final state of the rule specified in the @ViolationTarget@ after it
-- is remediated.
securityGroupRemediationAction_remediationResult :: Lens.Lens' SecurityGroupRemediationAction (Prelude.Maybe SecurityGroupRuleDescription)
securityGroupRemediationAction_remediationResult = Lens.lens (\SecurityGroupRemediationAction' {remediationResult} -> remediationResult) (\s@SecurityGroupRemediationAction' {} a -> s {remediationResult = a} :: SecurityGroupRemediationAction)

-- | Indicates if the current action is the default action.
securityGroupRemediationAction_isDefaultAction :: Lens.Lens' SecurityGroupRemediationAction (Prelude.Maybe Prelude.Bool)
securityGroupRemediationAction_isDefaultAction = Lens.lens (\SecurityGroupRemediationAction' {isDefaultAction} -> isDefaultAction) (\s@SecurityGroupRemediationAction' {} a -> s {isDefaultAction = a} :: SecurityGroupRemediationAction)

-- | Brief description of the action that will be performed.
securityGroupRemediationAction_description :: Lens.Lens' SecurityGroupRemediationAction (Prelude.Maybe Prelude.Text)
securityGroupRemediationAction_description = Lens.lens (\SecurityGroupRemediationAction' {description} -> description) (\s@SecurityGroupRemediationAction' {} a -> s {description = a} :: SecurityGroupRemediationAction)

instance
  Prelude.FromJSON
    SecurityGroupRemediationAction
  where
  parseJSON =
    Prelude.withObject
      "SecurityGroupRemediationAction"
      ( \x ->
          SecurityGroupRemediationAction'
            Prelude.<$> (x Prelude..:? "RemediationActionType")
            Prelude.<*> (x Prelude..:? "RemediationResult")
            Prelude.<*> (x Prelude..:? "IsDefaultAction")
            Prelude.<*> (x Prelude..:? "Description")
      )

instance
  Prelude.Hashable
    SecurityGroupRemediationAction

instance
  Prelude.NFData
    SecurityGroupRemediationAction
