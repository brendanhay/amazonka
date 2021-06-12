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
-- Module      : Network.AWS.CloudFormation.Types.StackSetSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetSummary where

import Network.AWS.CloudFormation.Types.AutoDeployment
import Network.AWS.CloudFormation.Types.PermissionModels
import Network.AWS.CloudFormation.Types.StackDriftStatus
import Network.AWS.CloudFormation.Types.StackSetStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The structures that contain summary information about the specified
-- stack set.
--
-- /See:/ 'newStackSetSummary' smart constructor.
data StackSetSummary = StackSetSummary'
  { -- | The status of the stack set.
    status :: Core.Maybe StackSetStatus,
    -- | Describes how the IAM roles required for stack set operations are
    -- created.
    --
    -- -   With @self-managed@ permissions, you must create the administrator
    --     and execution roles required to deploy to target accounts. For more
    --     information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions>.
    --
    -- -   With @service-managed@ permissions, StackSets automatically creates
    --     the IAM roles required to deploy to accounts managed by AWS
    --     Organizations. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions>.
    permissionModel :: Core.Maybe PermissionModels,
    -- | Most recent time when CloudFormation performed a drift detection
    -- operation on the stack set. This value will be @NULL@ for any stack set
    -- on which drift detection has not yet been performed.
    lastDriftCheckTimestamp :: Core.Maybe Core.ISO8601,
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
    -- -   @NOT_CHECKED@: AWS CloudFormation has not checked the stack set for
    --     drift.
    --
    -- -   @IN_SYNC@: All of the stack instances belonging to the stack set
    --     stack match from the expected template and parameter configuration.
    --
    -- -   @UNKNOWN@: This value is reserved for future use.
    driftStatus :: Core.Maybe StackDriftStatus,
    -- | The ID of the stack set.
    stackSetId :: Core.Maybe Core.Text,
    -- | [Service-managed permissions] Describes whether StackSets automatically
    -- deploys to AWS Organizations accounts that are added to a target
    -- organizational unit (OU).
    autoDeployment :: Core.Maybe AutoDeployment,
    -- | A description of the stack set that you specify when the stack set is
    -- created or updated.
    description :: Core.Maybe Core.Text,
    -- | The name of the stack set.
    stackSetName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StackSetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'stackSetSummary_status' - The status of the stack set.
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
--     the IAM roles required to deploy to accounts managed by AWS
--     Organizations. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions>.
--
-- 'lastDriftCheckTimestamp', 'stackSetSummary_lastDriftCheckTimestamp' - Most recent time when CloudFormation performed a drift detection
-- operation on the stack set. This value will be @NULL@ for any stack set
-- on which drift detection has not yet been performed.
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
-- -   @NOT_CHECKED@: AWS CloudFormation has not checked the stack set for
--     drift.
--
-- -   @IN_SYNC@: All of the stack instances belonging to the stack set
--     stack match from the expected template and parameter configuration.
--
-- -   @UNKNOWN@: This value is reserved for future use.
--
-- 'stackSetId', 'stackSetSummary_stackSetId' - The ID of the stack set.
--
-- 'autoDeployment', 'stackSetSummary_autoDeployment' - [Service-managed permissions] Describes whether StackSets automatically
-- deploys to AWS Organizations accounts that are added to a target
-- organizational unit (OU).
--
-- 'description', 'stackSetSummary_description' - A description of the stack set that you specify when the stack set is
-- created or updated.
--
-- 'stackSetName', 'stackSetSummary_stackSetName' - The name of the stack set.
newStackSetSummary ::
  StackSetSummary
newStackSetSummary =
  StackSetSummary'
    { status = Core.Nothing,
      permissionModel = Core.Nothing,
      lastDriftCheckTimestamp = Core.Nothing,
      driftStatus = Core.Nothing,
      stackSetId = Core.Nothing,
      autoDeployment = Core.Nothing,
      description = Core.Nothing,
      stackSetName = Core.Nothing
    }

-- | The status of the stack set.
stackSetSummary_status :: Lens.Lens' StackSetSummary (Core.Maybe StackSetStatus)
stackSetSummary_status = Lens.lens (\StackSetSummary' {status} -> status) (\s@StackSetSummary' {} a -> s {status = a} :: StackSetSummary)

-- | Describes how the IAM roles required for stack set operations are
-- created.
--
-- -   With @self-managed@ permissions, you must create the administrator
--     and execution roles required to deploy to target accounts. For more
--     information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions>.
--
-- -   With @service-managed@ permissions, StackSets automatically creates
--     the IAM roles required to deploy to accounts managed by AWS
--     Organizations. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions>.
stackSetSummary_permissionModel :: Lens.Lens' StackSetSummary (Core.Maybe PermissionModels)
stackSetSummary_permissionModel = Lens.lens (\StackSetSummary' {permissionModel} -> permissionModel) (\s@StackSetSummary' {} a -> s {permissionModel = a} :: StackSetSummary)

-- | Most recent time when CloudFormation performed a drift detection
-- operation on the stack set. This value will be @NULL@ for any stack set
-- on which drift detection has not yet been performed.
stackSetSummary_lastDriftCheckTimestamp :: Lens.Lens' StackSetSummary (Core.Maybe Core.UTCTime)
stackSetSummary_lastDriftCheckTimestamp = Lens.lens (\StackSetSummary' {lastDriftCheckTimestamp} -> lastDriftCheckTimestamp) (\s@StackSetSummary' {} a -> s {lastDriftCheckTimestamp = a} :: StackSetSummary) Core.. Lens.mapping Core._Time

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
-- -   @NOT_CHECKED@: AWS CloudFormation has not checked the stack set for
--     drift.
--
-- -   @IN_SYNC@: All of the stack instances belonging to the stack set
--     stack match from the expected template and parameter configuration.
--
-- -   @UNKNOWN@: This value is reserved for future use.
stackSetSummary_driftStatus :: Lens.Lens' StackSetSummary (Core.Maybe StackDriftStatus)
stackSetSummary_driftStatus = Lens.lens (\StackSetSummary' {driftStatus} -> driftStatus) (\s@StackSetSummary' {} a -> s {driftStatus = a} :: StackSetSummary)

-- | The ID of the stack set.
stackSetSummary_stackSetId :: Lens.Lens' StackSetSummary (Core.Maybe Core.Text)
stackSetSummary_stackSetId = Lens.lens (\StackSetSummary' {stackSetId} -> stackSetId) (\s@StackSetSummary' {} a -> s {stackSetId = a} :: StackSetSummary)

-- | [Service-managed permissions] Describes whether StackSets automatically
-- deploys to AWS Organizations accounts that are added to a target
-- organizational unit (OU).
stackSetSummary_autoDeployment :: Lens.Lens' StackSetSummary (Core.Maybe AutoDeployment)
stackSetSummary_autoDeployment = Lens.lens (\StackSetSummary' {autoDeployment} -> autoDeployment) (\s@StackSetSummary' {} a -> s {autoDeployment = a} :: StackSetSummary)

-- | A description of the stack set that you specify when the stack set is
-- created or updated.
stackSetSummary_description :: Lens.Lens' StackSetSummary (Core.Maybe Core.Text)
stackSetSummary_description = Lens.lens (\StackSetSummary' {description} -> description) (\s@StackSetSummary' {} a -> s {description = a} :: StackSetSummary)

-- | The name of the stack set.
stackSetSummary_stackSetName :: Lens.Lens' StackSetSummary (Core.Maybe Core.Text)
stackSetSummary_stackSetName = Lens.lens (\StackSetSummary' {stackSetName} -> stackSetName) (\s@StackSetSummary' {} a -> s {stackSetName = a} :: StackSetSummary)

instance Core.FromXML StackSetSummary where
  parseXML x =
    StackSetSummary'
      Core.<$> (x Core..@? "Status")
      Core.<*> (x Core..@? "PermissionModel")
      Core.<*> (x Core..@? "LastDriftCheckTimestamp")
      Core.<*> (x Core..@? "DriftStatus")
      Core.<*> (x Core..@? "StackSetId")
      Core.<*> (x Core..@? "AutoDeployment")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "StackSetName")

instance Core.Hashable StackSetSummary

instance Core.NFData StackSetSummary
