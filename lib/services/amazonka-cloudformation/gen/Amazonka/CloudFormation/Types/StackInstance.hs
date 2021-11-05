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
-- Module      : Amazonka.CloudFormation.Types.StackInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackInstance where

import Amazonka.CloudFormation.Types.Parameter
import Amazonka.CloudFormation.Types.StackDriftStatus
import Amazonka.CloudFormation.Types.StackInstanceComprehensiveStatus
import Amazonka.CloudFormation.Types.StackInstanceStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An CloudFormation stack, in a specific account and Region, that\'s part
-- of a stack set operation. A stack instance is a reference to an
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
    status :: Prelude.Maybe StackInstanceStatus,
    -- | Most recent time when CloudFormation performed a drift detection
    -- operation on the stack instance. This value will be @NULL@ for any stack
    -- instance on which drift detection has not yet been performed.
    lastDriftCheckTimestamp :: Prelude.Maybe Core.ISO8601,
    -- | [Self-managed permissions] The name of the Amazon Web Services account
    -- that the stack instance is associated with.
    account :: Prelude.Maybe Prelude.Text,
    -- | Status of the stack instance\'s actual configuration compared to the
    -- expected template and parameter configuration of the stack set to which
    -- it belongs.
    --
    -- -   @DRIFTED@: The stack differs from the expected template and
    --     parameter configuration of the stack set to which it belongs. A
    --     stack instance is considered to have drifted if one or more of the
    --     resources in the associated stack have drifted.
    --
    -- -   @NOT_CHECKED@: CloudFormation has not checked if the stack instance
    --     differs from its expected stack set configuration.
    --
    -- -   @IN_SYNC@: The stack instance\'s actual configuration matches its
    --     expected stack set configuration.
    --
    -- -   @UNKNOWN@: This value is reserved for future use.
    driftStatus :: Prelude.Maybe StackDriftStatus,
    -- | [Service-managed permissions] The organization root ID or organizational
    -- unit (OU) IDs that you specified for
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets>.
    organizationalUnitId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Web Services Region that the stack instance is
    -- associated with.
    region :: Prelude.Maybe Prelude.Text,
    -- | The explanation for the specific status code that is assigned to this
    -- stack instance.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The ID of the stack instance.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The detailed status of the stack instance.
    stackInstanceStatus :: Prelude.Maybe StackInstanceComprehensiveStatus,
    -- | A list of parameters from the stack set template whose values have been
    -- overridden in this stack instance.
    parameterOverrides :: Prelude.Maybe [Parameter],
    -- | The name or unique ID of the stack set that the stack instance is
    -- associated with.
    stackSetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'lastDriftCheckTimestamp', 'stackInstance_lastDriftCheckTimestamp' - Most recent time when CloudFormation performed a drift detection
-- operation on the stack instance. This value will be @NULL@ for any stack
-- instance on which drift detection has not yet been performed.
--
-- 'account', 'stackInstance_account' - [Self-managed permissions] The name of the Amazon Web Services account
-- that the stack instance is associated with.
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
-- -   @NOT_CHECKED@: CloudFormation has not checked if the stack instance
--     differs from its expected stack set configuration.
--
-- -   @IN_SYNC@: The stack instance\'s actual configuration matches its
--     expected stack set configuration.
--
-- -   @UNKNOWN@: This value is reserved for future use.
--
-- 'organizationalUnitId', 'stackInstance_organizationalUnitId' - [Service-managed permissions] The organization root ID or organizational
-- unit (OU) IDs that you specified for
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets>.
--
-- 'region', 'stackInstance_region' - The name of the Amazon Web Services Region that the stack instance is
-- associated with.
--
-- 'statusReason', 'stackInstance_statusReason' - The explanation for the specific status code that is assigned to this
-- stack instance.
--
-- 'stackId', 'stackInstance_stackId' - The ID of the stack instance.
--
-- 'stackInstanceStatus', 'stackInstance_stackInstanceStatus' - The detailed status of the stack instance.
--
-- 'parameterOverrides', 'stackInstance_parameterOverrides' - A list of parameters from the stack set template whose values have been
-- overridden in this stack instance.
--
-- 'stackSetId', 'stackInstance_stackSetId' - The name or unique ID of the stack set that the stack instance is
-- associated with.
newStackInstance ::
  StackInstance
newStackInstance =
  StackInstance'
    { status = Prelude.Nothing,
      lastDriftCheckTimestamp = Prelude.Nothing,
      account = Prelude.Nothing,
      driftStatus = Prelude.Nothing,
      organizationalUnitId = Prelude.Nothing,
      region = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      stackId = Prelude.Nothing,
      stackInstanceStatus = Prelude.Nothing,
      parameterOverrides = Prelude.Nothing,
      stackSetId = Prelude.Nothing
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
stackInstance_status :: Lens.Lens' StackInstance (Prelude.Maybe StackInstanceStatus)
stackInstance_status = Lens.lens (\StackInstance' {status} -> status) (\s@StackInstance' {} a -> s {status = a} :: StackInstance)

-- | Most recent time when CloudFormation performed a drift detection
-- operation on the stack instance. This value will be @NULL@ for any stack
-- instance on which drift detection has not yet been performed.
stackInstance_lastDriftCheckTimestamp :: Lens.Lens' StackInstance (Prelude.Maybe Prelude.UTCTime)
stackInstance_lastDriftCheckTimestamp = Lens.lens (\StackInstance' {lastDriftCheckTimestamp} -> lastDriftCheckTimestamp) (\s@StackInstance' {} a -> s {lastDriftCheckTimestamp = a} :: StackInstance) Prelude.. Lens.mapping Core._Time

-- | [Self-managed permissions] The name of the Amazon Web Services account
-- that the stack instance is associated with.
stackInstance_account :: Lens.Lens' StackInstance (Prelude.Maybe Prelude.Text)
stackInstance_account = Lens.lens (\StackInstance' {account} -> account) (\s@StackInstance' {} a -> s {account = a} :: StackInstance)

-- | Status of the stack instance\'s actual configuration compared to the
-- expected template and parameter configuration of the stack set to which
-- it belongs.
--
-- -   @DRIFTED@: The stack differs from the expected template and
--     parameter configuration of the stack set to which it belongs. A
--     stack instance is considered to have drifted if one or more of the
--     resources in the associated stack have drifted.
--
-- -   @NOT_CHECKED@: CloudFormation has not checked if the stack instance
--     differs from its expected stack set configuration.
--
-- -   @IN_SYNC@: The stack instance\'s actual configuration matches its
--     expected stack set configuration.
--
-- -   @UNKNOWN@: This value is reserved for future use.
stackInstance_driftStatus :: Lens.Lens' StackInstance (Prelude.Maybe StackDriftStatus)
stackInstance_driftStatus = Lens.lens (\StackInstance' {driftStatus} -> driftStatus) (\s@StackInstance' {} a -> s {driftStatus = a} :: StackInstance)

-- | [Service-managed permissions] The organization root ID or organizational
-- unit (OU) IDs that you specified for
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets>.
stackInstance_organizationalUnitId :: Lens.Lens' StackInstance (Prelude.Maybe Prelude.Text)
stackInstance_organizationalUnitId = Lens.lens (\StackInstance' {organizationalUnitId} -> organizationalUnitId) (\s@StackInstance' {} a -> s {organizationalUnitId = a} :: StackInstance)

-- | The name of the Amazon Web Services Region that the stack instance is
-- associated with.
stackInstance_region :: Lens.Lens' StackInstance (Prelude.Maybe Prelude.Text)
stackInstance_region = Lens.lens (\StackInstance' {region} -> region) (\s@StackInstance' {} a -> s {region = a} :: StackInstance)

-- | The explanation for the specific status code that is assigned to this
-- stack instance.
stackInstance_statusReason :: Lens.Lens' StackInstance (Prelude.Maybe Prelude.Text)
stackInstance_statusReason = Lens.lens (\StackInstance' {statusReason} -> statusReason) (\s@StackInstance' {} a -> s {statusReason = a} :: StackInstance)

-- | The ID of the stack instance.
stackInstance_stackId :: Lens.Lens' StackInstance (Prelude.Maybe Prelude.Text)
stackInstance_stackId = Lens.lens (\StackInstance' {stackId} -> stackId) (\s@StackInstance' {} a -> s {stackId = a} :: StackInstance)

-- | The detailed status of the stack instance.
stackInstance_stackInstanceStatus :: Lens.Lens' StackInstance (Prelude.Maybe StackInstanceComprehensiveStatus)
stackInstance_stackInstanceStatus = Lens.lens (\StackInstance' {stackInstanceStatus} -> stackInstanceStatus) (\s@StackInstance' {} a -> s {stackInstanceStatus = a} :: StackInstance)

-- | A list of parameters from the stack set template whose values have been
-- overridden in this stack instance.
stackInstance_parameterOverrides :: Lens.Lens' StackInstance (Prelude.Maybe [Parameter])
stackInstance_parameterOverrides = Lens.lens (\StackInstance' {parameterOverrides} -> parameterOverrides) (\s@StackInstance' {} a -> s {parameterOverrides = a} :: StackInstance) Prelude.. Lens.mapping Lens.coerced

-- | The name or unique ID of the stack set that the stack instance is
-- associated with.
stackInstance_stackSetId :: Lens.Lens' StackInstance (Prelude.Maybe Prelude.Text)
stackInstance_stackSetId = Lens.lens (\StackInstance' {stackSetId} -> stackSetId) (\s@StackInstance' {} a -> s {stackSetId = a} :: StackInstance)

instance Core.FromXML StackInstance where
  parseXML x =
    StackInstance'
      Prelude.<$> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "LastDriftCheckTimestamp")
      Prelude.<*> (x Core..@? "Account")
      Prelude.<*> (x Core..@? "DriftStatus")
      Prelude.<*> (x Core..@? "OrganizationalUnitId")
      Prelude.<*> (x Core..@? "Region")
      Prelude.<*> (x Core..@? "StatusReason")
      Prelude.<*> (x Core..@? "StackId")
      Prelude.<*> (x Core..@? "StackInstanceStatus")
      Prelude.<*> ( x Core..@? "ParameterOverrides"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "StackSetId")

instance Prelude.Hashable StackInstance

instance Prelude.NFData StackInstance
