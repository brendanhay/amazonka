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
-- Module      : Amazonka.CloudFormation.Types.StackSetSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackSetSummary where

import Amazonka.CloudFormation.Types.AutoDeployment
import Amazonka.CloudFormation.Types.ManagedExecution
import Amazonka.CloudFormation.Types.PermissionModels
import Amazonka.CloudFormation.Types.StackDriftStatus
import Amazonka.CloudFormation.Types.StackSetStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The structures that contain summary information about the specified
-- stack set.
--
-- /See:/ 'newStackSetSummary' smart constructor.
data StackSetSummary = StackSetSummary'
  { -- | The name of the stack set.
    stackSetName :: Prelude.Maybe Prelude.Text,
    -- | Status of the stack set\'s actual configuration compared to its expected
    -- template and parameter configuration. A stack set is considered to have
    -- drifted if one or more of its stack instances have drifted from their
    -- expected template and parameter configuration.
    --
    -- -   @DRIFTED@: One or more of the stack instances belonging to the stack
    --     set stack differs from the expected template and parameter
    --     configuration. A stack instance is considered to have drifted if one
    --     or more of the resources in the associated stack have drifted.
    --
    -- -   @NOT_CHECKED@: CloudFormation hasn\'t checked the stack set for
    --     drift.
    --
    -- -   @IN_SYNC@: All the stack instances belonging to the stack set stack
    --     match from the expected template and parameter configuration.
    --
    -- -   @UNKNOWN@: This value is reserved for future use.
    driftStatus :: Prelude.Maybe StackDriftStatus,
    -- | The ID of the stack set.
    stackSetId :: Prelude.Maybe Prelude.Text,
    -- | The status of the stack set.
    status :: Prelude.Maybe StackSetStatus,
    -- | A description of the stack set that you specify when the stack set is
    -- created or updated.
    description :: Prelude.Maybe Prelude.Text,
    -- | [Service-managed permissions] Describes whether StackSets automatically
    -- deploys to Organizations accounts that are added to a target
    -- organizational unit (OU).
    autoDeployment :: Prelude.Maybe AutoDeployment,
    -- | Describes whether StackSets performs non-conflicting operations
    -- concurrently and queues conflicting operations.
    managedExecution :: Prelude.Maybe ManagedExecution,
    -- | Describes how the IAM roles required for stack set operations are
    -- created.
    --
    -- -   With @self-managed@ permissions, you must create the administrator
    --     and execution roles required to deploy to target accounts. For more
    --     information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions>.
    --
    -- -   With @service-managed@ permissions, StackSets automatically creates
    --     the IAM roles required to deploy to accounts managed by
    --     Organizations. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions>.
    permissionModel :: Prelude.Maybe PermissionModels,
    -- | Most recent time when CloudFormation performed a drift detection
    -- operation on the stack set. This value will be @NULL@ for any stack set
    -- on which drift detection hasn\'t yet been performed.
    lastDriftCheckTimestamp :: Prelude.Maybe Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StackSetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackSetName', 'stackSetSummary_stackSetName' - The name of the stack set.
--
-- 'driftStatus', 'stackSetSummary_driftStatus' - Status of the stack set\'s actual configuration compared to its expected
-- template and parameter configuration. A stack set is considered to have
-- drifted if one or more of its stack instances have drifted from their
-- expected template and parameter configuration.
--
-- -   @DRIFTED@: One or more of the stack instances belonging to the stack
--     set stack differs from the expected template and parameter
--     configuration. A stack instance is considered to have drifted if one
--     or more of the resources in the associated stack have drifted.
--
-- -   @NOT_CHECKED@: CloudFormation hasn\'t checked the stack set for
--     drift.
--
-- -   @IN_SYNC@: All the stack instances belonging to the stack set stack
--     match from the expected template and parameter configuration.
--
-- -   @UNKNOWN@: This value is reserved for future use.
--
-- 'stackSetId', 'stackSetSummary_stackSetId' - The ID of the stack set.
--
-- 'status', 'stackSetSummary_status' - The status of the stack set.
--
-- 'description', 'stackSetSummary_description' - A description of the stack set that you specify when the stack set is
-- created or updated.
--
-- 'autoDeployment', 'stackSetSummary_autoDeployment' - [Service-managed permissions] Describes whether StackSets automatically
-- deploys to Organizations accounts that are added to a target
-- organizational unit (OU).
--
-- 'managedExecution', 'stackSetSummary_managedExecution' - Describes whether StackSets performs non-conflicting operations
-- concurrently and queues conflicting operations.
--
-- 'permissionModel', 'stackSetSummary_permissionModel' - Describes how the IAM roles required for stack set operations are
-- created.
--
-- -   With @self-managed@ permissions, you must create the administrator
--     and execution roles required to deploy to target accounts. For more
--     information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions>.
--
-- -   With @service-managed@ permissions, StackSets automatically creates
--     the IAM roles required to deploy to accounts managed by
--     Organizations. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions>.
--
-- 'lastDriftCheckTimestamp', 'stackSetSummary_lastDriftCheckTimestamp' - Most recent time when CloudFormation performed a drift detection
-- operation on the stack set. This value will be @NULL@ for any stack set
-- on which drift detection hasn\'t yet been performed.
newStackSetSummary ::
  StackSetSummary
newStackSetSummary =
  StackSetSummary'
    { stackSetName = Prelude.Nothing,
      driftStatus = Prelude.Nothing,
      stackSetId = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      autoDeployment = Prelude.Nothing,
      managedExecution = Prelude.Nothing,
      permissionModel = Prelude.Nothing,
      lastDriftCheckTimestamp = Prelude.Nothing
    }

-- | The name of the stack set.
stackSetSummary_stackSetName :: Lens.Lens' StackSetSummary (Prelude.Maybe Prelude.Text)
stackSetSummary_stackSetName = Lens.lens (\StackSetSummary' {stackSetName} -> stackSetName) (\s@StackSetSummary' {} a -> s {stackSetName = a} :: StackSetSummary)

-- | Status of the stack set\'s actual configuration compared to its expected
-- template and parameter configuration. A stack set is considered to have
-- drifted if one or more of its stack instances have drifted from their
-- expected template and parameter configuration.
--
-- -   @DRIFTED@: One or more of the stack instances belonging to the stack
--     set stack differs from the expected template and parameter
--     configuration. A stack instance is considered to have drifted if one
--     or more of the resources in the associated stack have drifted.
--
-- -   @NOT_CHECKED@: CloudFormation hasn\'t checked the stack set for
--     drift.
--
-- -   @IN_SYNC@: All the stack instances belonging to the stack set stack
--     match from the expected template and parameter configuration.
--
-- -   @UNKNOWN@: This value is reserved for future use.
stackSetSummary_driftStatus :: Lens.Lens' StackSetSummary (Prelude.Maybe StackDriftStatus)
stackSetSummary_driftStatus = Lens.lens (\StackSetSummary' {driftStatus} -> driftStatus) (\s@StackSetSummary' {} a -> s {driftStatus = a} :: StackSetSummary)

-- | The ID of the stack set.
stackSetSummary_stackSetId :: Lens.Lens' StackSetSummary (Prelude.Maybe Prelude.Text)
stackSetSummary_stackSetId = Lens.lens (\StackSetSummary' {stackSetId} -> stackSetId) (\s@StackSetSummary' {} a -> s {stackSetId = a} :: StackSetSummary)

-- | The status of the stack set.
stackSetSummary_status :: Lens.Lens' StackSetSummary (Prelude.Maybe StackSetStatus)
stackSetSummary_status = Lens.lens (\StackSetSummary' {status} -> status) (\s@StackSetSummary' {} a -> s {status = a} :: StackSetSummary)

-- | A description of the stack set that you specify when the stack set is
-- created or updated.
stackSetSummary_description :: Lens.Lens' StackSetSummary (Prelude.Maybe Prelude.Text)
stackSetSummary_description = Lens.lens (\StackSetSummary' {description} -> description) (\s@StackSetSummary' {} a -> s {description = a} :: StackSetSummary)

-- | [Service-managed permissions] Describes whether StackSets automatically
-- deploys to Organizations accounts that are added to a target
-- organizational unit (OU).
stackSetSummary_autoDeployment :: Lens.Lens' StackSetSummary (Prelude.Maybe AutoDeployment)
stackSetSummary_autoDeployment = Lens.lens (\StackSetSummary' {autoDeployment} -> autoDeployment) (\s@StackSetSummary' {} a -> s {autoDeployment = a} :: StackSetSummary)

-- | Describes whether StackSets performs non-conflicting operations
-- concurrently and queues conflicting operations.
stackSetSummary_managedExecution :: Lens.Lens' StackSetSummary (Prelude.Maybe ManagedExecution)
stackSetSummary_managedExecution = Lens.lens (\StackSetSummary' {managedExecution} -> managedExecution) (\s@StackSetSummary' {} a -> s {managedExecution = a} :: StackSetSummary)

-- | Describes how the IAM roles required for stack set operations are
-- created.
--
-- -   With @self-managed@ permissions, you must create the administrator
--     and execution roles required to deploy to target accounts. For more
--     information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions>.
--
-- -   With @service-managed@ permissions, StackSets automatically creates
--     the IAM roles required to deploy to accounts managed by
--     Organizations. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions>.
stackSetSummary_permissionModel :: Lens.Lens' StackSetSummary (Prelude.Maybe PermissionModels)
stackSetSummary_permissionModel = Lens.lens (\StackSetSummary' {permissionModel} -> permissionModel) (\s@StackSetSummary' {} a -> s {permissionModel = a} :: StackSetSummary)

-- | Most recent time when CloudFormation performed a drift detection
-- operation on the stack set. This value will be @NULL@ for any stack set
-- on which drift detection hasn\'t yet been performed.
stackSetSummary_lastDriftCheckTimestamp :: Lens.Lens' StackSetSummary (Prelude.Maybe Prelude.UTCTime)
stackSetSummary_lastDriftCheckTimestamp = Lens.lens (\StackSetSummary' {lastDriftCheckTimestamp} -> lastDriftCheckTimestamp) (\s@StackSetSummary' {} a -> s {lastDriftCheckTimestamp = a} :: StackSetSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromXML StackSetSummary where
  parseXML x =
    StackSetSummary'
      Prelude.<$> (x Core..@? "StackSetName")
      Prelude.<*> (x Core..@? "DriftStatus")
      Prelude.<*> (x Core..@? "StackSetId")
      Prelude.<*> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "Description")
      Prelude.<*> (x Core..@? "AutoDeployment")
      Prelude.<*> (x Core..@? "ManagedExecution")
      Prelude.<*> (x Core..@? "PermissionModel")
      Prelude.<*> (x Core..@? "LastDriftCheckTimestamp")

instance Prelude.Hashable StackSetSummary where
  hashWithSalt _salt StackSetSummary' {..} =
    _salt `Prelude.hashWithSalt` stackSetName
      `Prelude.hashWithSalt` driftStatus
      `Prelude.hashWithSalt` stackSetId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` autoDeployment
      `Prelude.hashWithSalt` managedExecution
      `Prelude.hashWithSalt` permissionModel
      `Prelude.hashWithSalt` lastDriftCheckTimestamp

instance Prelude.NFData StackSetSummary where
  rnf StackSetSummary' {..} =
    Prelude.rnf stackSetName
      `Prelude.seq` Prelude.rnf driftStatus
      `Prelude.seq` Prelude.rnf stackSetId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf autoDeployment
      `Prelude.seq` Prelude.rnf managedExecution
      `Prelude.seq` Prelude.rnf permissionModel
      `Prelude.seq` Prelude.rnf lastDriftCheckTimestamp
