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
-- Module      : Network.AWS.ServiceCatalog.Types.StackInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.StackInstance where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ServiceCatalog.Types.StackInstanceStatus

-- | An AWS CloudFormation stack, in a specific account and region, that\'s
-- part of a stack set operation. A stack instance is a reference to an
-- attempted or actual stack in a given account within a given region. A
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
    --     a @DeleteStackInstances@ operation, with @RetainStacks@ set to true,
    --     to delete the stack instance, and then delete the stack manually.
    --
    -- -   @OUTDATED@: The stack isn\'t currently up to date with the stack set
    --     because either the associated stack failed during a @CreateStackSet@
    --     or @UpdateStackSet@ operation, or the stack was part of a
    --     @CreateStackSet@ or @UpdateStackSet@ operation that failed or was
    --     stopped before the stack was created or updated.
    --
    -- -   @CURRENT@: The stack is currently up to date with the stack set.
    stackInstanceStatus :: Core.Maybe StackInstanceStatus,
    -- | The name of the AWS account that the stack instance is associated with.
    account :: Core.Maybe Core.Text,
    -- | The name of the AWS region that the stack instance is associated with.
    region :: Core.Maybe Core.Text
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
-- 'stackInstanceStatus', 'stackInstance_stackInstanceStatus' - The status of the stack instance, in terms of its synchronization with
-- its associated stack set.
--
-- -   @INOPERABLE@: A @DeleteStackInstances@ operation has failed and left
--     the stack in an unstable state. Stacks in this state are excluded
--     from further @UpdateStackSet@ operations. You might need to perform
--     a @DeleteStackInstances@ operation, with @RetainStacks@ set to true,
--     to delete the stack instance, and then delete the stack manually.
--
-- -   @OUTDATED@: The stack isn\'t currently up to date with the stack set
--     because either the associated stack failed during a @CreateStackSet@
--     or @UpdateStackSet@ operation, or the stack was part of a
--     @CreateStackSet@ or @UpdateStackSet@ operation that failed or was
--     stopped before the stack was created or updated.
--
-- -   @CURRENT@: The stack is currently up to date with the stack set.
--
-- 'account', 'stackInstance_account' - The name of the AWS account that the stack instance is associated with.
--
-- 'region', 'stackInstance_region' - The name of the AWS region that the stack instance is associated with.
newStackInstance ::
  StackInstance
newStackInstance =
  StackInstance'
    { stackInstanceStatus = Core.Nothing,
      account = Core.Nothing,
      region = Core.Nothing
    }

-- | The status of the stack instance, in terms of its synchronization with
-- its associated stack set.
--
-- -   @INOPERABLE@: A @DeleteStackInstances@ operation has failed and left
--     the stack in an unstable state. Stacks in this state are excluded
--     from further @UpdateStackSet@ operations. You might need to perform
--     a @DeleteStackInstances@ operation, with @RetainStacks@ set to true,
--     to delete the stack instance, and then delete the stack manually.
--
-- -   @OUTDATED@: The stack isn\'t currently up to date with the stack set
--     because either the associated stack failed during a @CreateStackSet@
--     or @UpdateStackSet@ operation, or the stack was part of a
--     @CreateStackSet@ or @UpdateStackSet@ operation that failed or was
--     stopped before the stack was created or updated.
--
-- -   @CURRENT@: The stack is currently up to date with the stack set.
stackInstance_stackInstanceStatus :: Lens.Lens' StackInstance (Core.Maybe StackInstanceStatus)
stackInstance_stackInstanceStatus = Lens.lens (\StackInstance' {stackInstanceStatus} -> stackInstanceStatus) (\s@StackInstance' {} a -> s {stackInstanceStatus = a} :: StackInstance)

-- | The name of the AWS account that the stack instance is associated with.
stackInstance_account :: Lens.Lens' StackInstance (Core.Maybe Core.Text)
stackInstance_account = Lens.lens (\StackInstance' {account} -> account) (\s@StackInstance' {} a -> s {account = a} :: StackInstance)

-- | The name of the AWS region that the stack instance is associated with.
stackInstance_region :: Lens.Lens' StackInstance (Core.Maybe Core.Text)
stackInstance_region = Lens.lens (\StackInstance' {region} -> region) (\s@StackInstance' {} a -> s {region = a} :: StackInstance)

instance Core.FromJSON StackInstance where
  parseJSON =
    Core.withObject
      "StackInstance"
      ( \x ->
          StackInstance'
            Core.<$> (x Core..:? "StackInstanceStatus")
            Core.<*> (x Core..:? "Account")
            Core.<*> (x Core..:? "Region")
      )

instance Core.Hashable StackInstance

instance Core.NFData StackInstance
