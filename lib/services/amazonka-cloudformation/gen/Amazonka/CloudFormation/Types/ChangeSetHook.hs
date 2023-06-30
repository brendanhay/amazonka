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
-- Module      : Amazonka.CloudFormation.Types.ChangeSetHook
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.ChangeSetHook where

import Amazonka.CloudFormation.Types.ChangeSetHookTargetDetails
import Amazonka.CloudFormation.Types.HookFailureMode
import Amazonka.CloudFormation.Types.HookInvocationPoint
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the resource, the hook, and the hook version to be invoked.
--
-- /See:/ 'newChangeSetHook' smart constructor.
data ChangeSetHook = ChangeSetHook'
  { -- | Specify the hook failure mode for non-compliant resources in the
    -- followings ways.
    --
    -- -   @FAIL@ Stops provisioning resources.
    --
    -- -   @WARN@ Allows provisioning to continue with a warning message.
    failureMode :: Prelude.Maybe HookFailureMode,
    -- | Specifies the points in provisioning logic where a hook is invoked.
    invocationPoint :: Prelude.Maybe HookInvocationPoint,
    -- | Specifies details about the target that the hook will run against.
    targetDetails :: Prelude.Maybe ChangeSetHookTargetDetails,
    -- | The version ID of the type configuration.
    typeConfigurationVersionId :: Prelude.Maybe Prelude.Text,
    -- | The unique name for your hook. Specifies a three-part namespace for your
    -- hook, with a recommended pattern of @Organization::Service::Hook@.
    --
    -- The following organization namespaces are reserved and can\'t be used in
    -- your hook type names:
    --
    -- -   @Alexa@
    --
    -- -   @AMZN@
    --
    -- -   @Amazon@
    --
    -- -   @ASK@
    --
    -- -   @AWS@
    --
    -- -   @Custom@
    --
    -- -   @Dev@
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The version ID of the type specified.
    typeVersionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeSetHook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureMode', 'changeSetHook_failureMode' - Specify the hook failure mode for non-compliant resources in the
-- followings ways.
--
-- -   @FAIL@ Stops provisioning resources.
--
-- -   @WARN@ Allows provisioning to continue with a warning message.
--
-- 'invocationPoint', 'changeSetHook_invocationPoint' - Specifies the points in provisioning logic where a hook is invoked.
--
-- 'targetDetails', 'changeSetHook_targetDetails' - Specifies details about the target that the hook will run against.
--
-- 'typeConfigurationVersionId', 'changeSetHook_typeConfigurationVersionId' - The version ID of the type configuration.
--
-- 'typeName', 'changeSetHook_typeName' - The unique name for your hook. Specifies a three-part namespace for your
-- hook, with a recommended pattern of @Organization::Service::Hook@.
--
-- The following organization namespaces are reserved and can\'t be used in
-- your hook type names:
--
-- -   @Alexa@
--
-- -   @AMZN@
--
-- -   @Amazon@
--
-- -   @ASK@
--
-- -   @AWS@
--
-- -   @Custom@
--
-- -   @Dev@
--
-- 'typeVersionId', 'changeSetHook_typeVersionId' - The version ID of the type specified.
newChangeSetHook ::
  ChangeSetHook
newChangeSetHook =
  ChangeSetHook'
    { failureMode = Prelude.Nothing,
      invocationPoint = Prelude.Nothing,
      targetDetails = Prelude.Nothing,
      typeConfigurationVersionId = Prelude.Nothing,
      typeName = Prelude.Nothing,
      typeVersionId = Prelude.Nothing
    }

-- | Specify the hook failure mode for non-compliant resources in the
-- followings ways.
--
-- -   @FAIL@ Stops provisioning resources.
--
-- -   @WARN@ Allows provisioning to continue with a warning message.
changeSetHook_failureMode :: Lens.Lens' ChangeSetHook (Prelude.Maybe HookFailureMode)
changeSetHook_failureMode = Lens.lens (\ChangeSetHook' {failureMode} -> failureMode) (\s@ChangeSetHook' {} a -> s {failureMode = a} :: ChangeSetHook)

-- | Specifies the points in provisioning logic where a hook is invoked.
changeSetHook_invocationPoint :: Lens.Lens' ChangeSetHook (Prelude.Maybe HookInvocationPoint)
changeSetHook_invocationPoint = Lens.lens (\ChangeSetHook' {invocationPoint} -> invocationPoint) (\s@ChangeSetHook' {} a -> s {invocationPoint = a} :: ChangeSetHook)

-- | Specifies details about the target that the hook will run against.
changeSetHook_targetDetails :: Lens.Lens' ChangeSetHook (Prelude.Maybe ChangeSetHookTargetDetails)
changeSetHook_targetDetails = Lens.lens (\ChangeSetHook' {targetDetails} -> targetDetails) (\s@ChangeSetHook' {} a -> s {targetDetails = a} :: ChangeSetHook)

-- | The version ID of the type configuration.
changeSetHook_typeConfigurationVersionId :: Lens.Lens' ChangeSetHook (Prelude.Maybe Prelude.Text)
changeSetHook_typeConfigurationVersionId = Lens.lens (\ChangeSetHook' {typeConfigurationVersionId} -> typeConfigurationVersionId) (\s@ChangeSetHook' {} a -> s {typeConfigurationVersionId = a} :: ChangeSetHook)

-- | The unique name for your hook. Specifies a three-part namespace for your
-- hook, with a recommended pattern of @Organization::Service::Hook@.
--
-- The following organization namespaces are reserved and can\'t be used in
-- your hook type names:
--
-- -   @Alexa@
--
-- -   @AMZN@
--
-- -   @Amazon@
--
-- -   @ASK@
--
-- -   @AWS@
--
-- -   @Custom@
--
-- -   @Dev@
changeSetHook_typeName :: Lens.Lens' ChangeSetHook (Prelude.Maybe Prelude.Text)
changeSetHook_typeName = Lens.lens (\ChangeSetHook' {typeName} -> typeName) (\s@ChangeSetHook' {} a -> s {typeName = a} :: ChangeSetHook)

-- | The version ID of the type specified.
changeSetHook_typeVersionId :: Lens.Lens' ChangeSetHook (Prelude.Maybe Prelude.Text)
changeSetHook_typeVersionId = Lens.lens (\ChangeSetHook' {typeVersionId} -> typeVersionId) (\s@ChangeSetHook' {} a -> s {typeVersionId = a} :: ChangeSetHook)

instance Data.FromXML ChangeSetHook where
  parseXML x =
    ChangeSetHook'
      Prelude.<$> (x Data..@? "FailureMode")
      Prelude.<*> (x Data..@? "InvocationPoint")
      Prelude.<*> (x Data..@? "TargetDetails")
      Prelude.<*> (x Data..@? "TypeConfigurationVersionId")
      Prelude.<*> (x Data..@? "TypeName")
      Prelude.<*> (x Data..@? "TypeVersionId")

instance Prelude.Hashable ChangeSetHook where
  hashWithSalt _salt ChangeSetHook' {..} =
    _salt
      `Prelude.hashWithSalt` failureMode
      `Prelude.hashWithSalt` invocationPoint
      `Prelude.hashWithSalt` targetDetails
      `Prelude.hashWithSalt` typeConfigurationVersionId
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` typeVersionId

instance Prelude.NFData ChangeSetHook where
  rnf ChangeSetHook' {..} =
    Prelude.rnf failureMode
      `Prelude.seq` Prelude.rnf invocationPoint
      `Prelude.seq` Prelude.rnf targetDetails
      `Prelude.seq` Prelude.rnf typeConfigurationVersionId
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf typeVersionId
