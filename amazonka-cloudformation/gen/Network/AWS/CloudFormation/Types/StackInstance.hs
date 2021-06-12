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
-- Module      : Network.AWS.CloudFormation.Types.StackInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackInstance where

import Network.AWS.CloudFormation.Types.Parameter
import Network.AWS.CloudFormation.Types.StackDriftStatus
import Network.AWS.CloudFormation.Types.StackInstanceComprehensiveStatus
import Network.AWS.CloudFormation.Types.StackInstanceStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An AWS CloudFormation stack, in a specific account and Region, that\'s
-- part of a stack set operation. A stack instance is a reference to an
-- attempted or actual stack in a given account within a given Region. A
-- stack instance can exist without a stack—for example, if the stack
-- couldn\'t be created for some reason. A stack instance is associated
-- with only one stack set. Each stack instance contains the ID of its
-- associated stack set, as well as the ID of the actual stack and the
-- stack status.
--
-- /See:/ 'newStackInstance' smart constructor.
data StackInstance = StackInstance'
  { -- | The status of the stack instance, in terms of its synchronization with
    -- its associated stack set.
    --
    -- -   @INOPERABLE@: A @DeleteStackInstances@ operation has failed and left
    --     the stack in an unstable state. Stacks in this state are excluded
    --     from further @UpdateStackSet@ operations. You might need to perform
    --     a @DeleteStackInstances@ operation, with @RetainStacks@ set to
    --     @true@, to delete the stack instance, and then delete the stack
    --     manually.
    --
    -- -   @OUTDATED@: The stack isn\'t currently up to date with the stack set
    --     because:
    --
    --     -   The associated stack failed during a @CreateStackSet@ or
    --         @UpdateStackSet@ operation.
    --
    --     -   The stack was part of a @CreateStackSet@ or @UpdateStackSet@
    --         operation that failed or was stopped before the stack was
    --         created or updated.
    --
    -- -   @CURRENT@: The stack is currently up to date with the stack set.
    status :: Core.Maybe StackInstanceStatus,
    -- | A list of parameters from the stack set template whose values have been
    -- overridden in this stack instance.
    parameterOverrides :: Core.Maybe [Parameter],
    -- | The ID of the stack instance.
    stackId :: Core.Maybe Core.Text,
    -- | The detailed status of the stack instance.
    stackInstanceStatus :: Core.Maybe StackInstanceComprehensiveStatus,
    -- | [Service-managed permissions] The organization root ID or organizational
    -- unit (OU) IDs that you specified for
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets>.
    organizationalUnitId :: Core.Maybe Core.Text,
    -- | Most recent time when CloudFormation performed a drift detection
    -- operation on the stack instance. This value will be @NULL@ for any stack
    -- instance on which drift detection has not yet been performed.
    lastDriftCheckTimestamp :: Core.Maybe Core.ISO8601,
    -- | Status of the stack instance\'s actual configuration compared to the
    -- expected template and parameter configuration of the stack set to which
    -- it belongs.
    --
    -- -   @DRIFTED@: The stack differs from the expected template and
    --     parameter configuration of the stack set to which it belongs. A
    --     stack instance is considered to have drifted if one or more of the
    --     resources in the associated stack have drifted.
    --
    -- -   @NOT_CHECKED@: AWS CloudFormation has not checked if the stack
    --     instance differs from its expected stack set configuration.
    --
    -- -   @IN_SYNC@: The stack instance\'s actual configuration matches its
    --     expected stack set configuration.
    --
    -- -   @UNKNOWN@: This value is reserved for future use.
    driftStatus :: Core.Maybe StackDriftStatus,
    -- | [Self-managed permissions] The name of the AWS account that the stack
    -- instance is associated with.
    account :: Core.Maybe Core.Text,
    -- | The name or unique ID of the stack set that the stack instance is
    -- associated with.
    stackSetId :: Core.Maybe Core.Text,
    -- | The name of the AWS Region that the stack instance is associated with.
    region :: Core.Maybe Core.Text,
    -- | The explanation for the specific status code that is assigned to this
    -- stack instance.
    statusReason :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StackInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'stackInstance_status' - The status of the stack instance, in terms of its synchronization with
-- its associated stack set.
--
-- -   @INOPERABLE@: A @DeleteStackInstances@ operation has failed and left
--     the stack in an unstable state. Stacks in this state are excluded
--     from further @UpdateStackSet@ operations. You might need to perform
--     a @DeleteStackInstances@ operation, with @RetainStacks@ set to
--     @true@, to delete the stack instance, and then delete the stack
--     manually.
--
-- -   @OUTDATED@: The stack isn\'t currently up to date with the stack set
--     because:
--
--     -   The associated stack failed during a @CreateStackSet@ or
--         @UpdateStackSet@ operation.
--
--     -   The stack was part of a @CreateStackSet@ or @UpdateStackSet@
--         operation that failed or was stopped before the stack was
--         created or updated.
--
-- -   @CURRENT@: The stack is currently up to date with the stack set.
--
-- 'parameterOverrides', 'stackInstance_parameterOverrides' - A list of parameters from the stack set template whose values have been
-- overridden in this stack instance.
--
-- 'stackId', 'stackInstance_stackId' - The ID of the stack instance.
--
-- 'stackInstanceStatus', 'stackInstance_stackInstanceStatus' - The detailed status of the stack instance.
--
-- 'organizationalUnitId', 'stackInstance_organizationalUnitId' - [Service-managed permissions] The organization root ID or organizational
-- unit (OU) IDs that you specified for
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets>.
--
-- 'lastDriftCheckTimestamp', 'stackInstance_lastDriftCheckTimestamp' - Most recent time when CloudFormation performed a drift detection
-- operation on the stack instance. This value will be @NULL@ for any stack
-- instance on which drift detection has not yet been performed.
--
-- 'driftStatus', 'stackInstance_driftStatus' - Status of the stack instance\'s actual configuration compared to the
-- expected template and parameter configuration of the stack set to which
-- it belongs.
--
-- -   @DRIFTED@: The stack differs from the expected template and
--     parameter configuration of the stack set to which it belongs. A
--     stack instance is considered to have drifted if one or more of the
--     resources in the associated stack have drifted.
--
-- -   @NOT_CHECKED@: AWS CloudFormation has not checked if the stack
--     instance differs from its expected stack set configuration.
--
-- -   @IN_SYNC@: The stack instance\'s actual configuration matches its
--     expected stack set configuration.
--
-- -   @UNKNOWN@: This value is reserved for future use.
--
-- 'account', 'stackInstance_account' - [Self-managed permissions] The name of the AWS account that the stack
-- instance is associated with.
--
-- 'stackSetId', 'stackInstance_stackSetId' - The name or unique ID of the stack set that the stack instance is
-- associated with.
--
-- 'region', 'stackInstance_region' - The name of the AWS Region that the stack instance is associated with.
--
-- 'statusReason', 'stackInstance_statusReason' - The explanation for the specific status code that is assigned to this
-- stack instance.
newStackInstance ::
  StackInstance
newStackInstance =
  StackInstance'
    { status = Core.Nothing,
      parameterOverrides = Core.Nothing,
      stackId = Core.Nothing,
      stackInstanceStatus = Core.Nothing,
      organizationalUnitId = Core.Nothing,
      lastDriftCheckTimestamp = Core.Nothing,
      driftStatus = Core.Nothing,
      account = Core.Nothing,
      stackSetId = Core.Nothing,
      region = Core.Nothing,
      statusReason = Core.Nothing
    }

-- | The status of the stack instance, in terms of its synchronization with
-- its associated stack set.
--
-- -   @INOPERABLE@: A @DeleteStackInstances@ operation has failed and left
--     the stack in an unstable state. Stacks in this state are excluded
--     from further @UpdateStackSet@ operations. You might need to perform
--     a @DeleteStackInstances@ operation, with @RetainStacks@ set to
--     @true@, to delete the stack instance, and then delete the stack
--     manually.
--
-- -   @OUTDATED@: The stack isn\'t currently up to date with the stack set
--     because:
--
--     -   The associated stack failed during a @CreateStackSet@ or
--         @UpdateStackSet@ operation.
--
--     -   The stack was part of a @CreateStackSet@ or @UpdateStackSet@
--         operation that failed or was stopped before the stack was
--         created or updated.
--
-- -   @CURRENT@: The stack is currently up to date with the stack set.
stackInstance_status :: Lens.Lens' StackInstance (Core.Maybe StackInstanceStatus)
stackInstance_status = Lens.lens (\StackInstance' {status} -> status) (\s@StackInstance' {} a -> s {status = a} :: StackInstance)

-- | A list of parameters from the stack set template whose values have been
-- overridden in this stack instance.
stackInstance_parameterOverrides :: Lens.Lens' StackInstance (Core.Maybe [Parameter])
stackInstance_parameterOverrides = Lens.lens (\StackInstance' {parameterOverrides} -> parameterOverrides) (\s@StackInstance' {} a -> s {parameterOverrides = a} :: StackInstance) Core.. Lens.mapping Lens._Coerce

-- | The ID of the stack instance.
stackInstance_stackId :: Lens.Lens' StackInstance (Core.Maybe Core.Text)
stackInstance_stackId = Lens.lens (\StackInstance' {stackId} -> stackId) (\s@StackInstance' {} a -> s {stackId = a} :: StackInstance)

-- | The detailed status of the stack instance.
stackInstance_stackInstanceStatus :: Lens.Lens' StackInstance (Core.Maybe StackInstanceComprehensiveStatus)
stackInstance_stackInstanceStatus = Lens.lens (\StackInstance' {stackInstanceStatus} -> stackInstanceStatus) (\s@StackInstance' {} a -> s {stackInstanceStatus = a} :: StackInstance)

-- | [Service-managed permissions] The organization root ID or organizational
-- unit (OU) IDs that you specified for
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets>.
stackInstance_organizationalUnitId :: Lens.Lens' StackInstance (Core.Maybe Core.Text)
stackInstance_organizationalUnitId = Lens.lens (\StackInstance' {organizationalUnitId} -> organizationalUnitId) (\s@StackInstance' {} a -> s {organizationalUnitId = a} :: StackInstance)

-- | Most recent time when CloudFormation performed a drift detection
-- operation on the stack instance. This value will be @NULL@ for any stack
-- instance on which drift detection has not yet been performed.
stackInstance_lastDriftCheckTimestamp :: Lens.Lens' StackInstance (Core.Maybe Core.UTCTime)
stackInstance_lastDriftCheckTimestamp = Lens.lens (\StackInstance' {lastDriftCheckTimestamp} -> lastDriftCheckTimestamp) (\s@StackInstance' {} a -> s {lastDriftCheckTimestamp = a} :: StackInstance) Core.. Lens.mapping Core._Time

-- | Status of the stack instance\'s actual configuration compared to the
-- expected template and parameter configuration of the stack set to which
-- it belongs.
--
-- -   @DRIFTED@: The stack differs from the expected template and
--     parameter configuration of the stack set to which it belongs. A
--     stack instance is considered to have drifted if one or more of the
--     resources in the associated stack have drifted.
--
-- -   @NOT_CHECKED@: AWS CloudFormation has not checked if the stack
--     instance differs from its expected stack set configuration.
--
-- -   @IN_SYNC@: The stack instance\'s actual configuration matches its
--     expected stack set configuration.
--
-- -   @UNKNOWN@: This value is reserved for future use.
stackInstance_driftStatus :: Lens.Lens' StackInstance (Core.Maybe StackDriftStatus)
stackInstance_driftStatus = Lens.lens (\StackInstance' {driftStatus} -> driftStatus) (\s@StackInstance' {} a -> s {driftStatus = a} :: StackInstance)

-- | [Self-managed permissions] The name of the AWS account that the stack
-- instance is associated with.
stackInstance_account :: Lens.Lens' StackInstance (Core.Maybe Core.Text)
stackInstance_account = Lens.lens (\StackInstance' {account} -> account) (\s@StackInstance' {} a -> s {account = a} :: StackInstance)

-- | The name or unique ID of the stack set that the stack instance is
-- associated with.
stackInstance_stackSetId :: Lens.Lens' StackInstance (Core.Maybe Core.Text)
stackInstance_stackSetId = Lens.lens (\StackInstance' {stackSetId} -> stackSetId) (\s@StackInstance' {} a -> s {stackSetId = a} :: StackInstance)

-- | The name of the AWS Region that the stack instance is associated with.
stackInstance_region :: Lens.Lens' StackInstance (Core.Maybe Core.Text)
stackInstance_region = Lens.lens (\StackInstance' {region} -> region) (\s@StackInstance' {} a -> s {region = a} :: StackInstance)

-- | The explanation for the specific status code that is assigned to this
-- stack instance.
stackInstance_statusReason :: Lens.Lens' StackInstance (Core.Maybe Core.Text)
stackInstance_statusReason = Lens.lens (\StackInstance' {statusReason} -> statusReason) (\s@StackInstance' {} a -> s {statusReason = a} :: StackInstance)

instance Core.FromXML StackInstance where
  parseXML x =
    StackInstance'
      Core.<$> (x Core..@? "Status")
      Core.<*> ( x Core..@? "ParameterOverrides" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "StackId")
      Core.<*> (x Core..@? "StackInstanceStatus")
      Core.<*> (x Core..@? "OrganizationalUnitId")
      Core.<*> (x Core..@? "LastDriftCheckTimestamp")
      Core.<*> (x Core..@? "DriftStatus")
      Core.<*> (x Core..@? "Account")
      Core.<*> (x Core..@? "StackSetId")
      Core.<*> (x Core..@? "Region")
      Core.<*> (x Core..@? "StatusReason")

instance Core.Hashable StackInstance

instance Core.NFData StackInstance
