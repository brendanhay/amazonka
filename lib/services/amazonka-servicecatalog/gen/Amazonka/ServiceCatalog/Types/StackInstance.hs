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
-- Module      : Amazonka.ServiceCatalog.Types.StackInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.StackInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.StackInstanceStatus

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
  { -- | The name of the Amazon Web Services account that the stack instance is
    -- associated with.
    account :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Web Services Region that the stack instance is
    -- associated with.
    region :: Prelude.Maybe Prelude.Text,
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
    stackInstanceStatus :: Prelude.Maybe StackInstanceStatus
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
-- 'account', 'stackInstance_account' - The name of the Amazon Web Services account that the stack instance is
-- associated with.
--
-- 'region', 'stackInstance_region' - The name of the Amazon Web Services Region that the stack instance is
-- associated with.
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
newStackInstance ::
  StackInstance
newStackInstance =
  StackInstance'
    { account = Prelude.Nothing,
      region = Prelude.Nothing,
      stackInstanceStatus = Prelude.Nothing
    }

-- | The name of the Amazon Web Services account that the stack instance is
-- associated with.
stackInstance_account :: Lens.Lens' StackInstance (Prelude.Maybe Prelude.Text)
stackInstance_account = Lens.lens (\StackInstance' {account} -> account) (\s@StackInstance' {} a -> s {account = a} :: StackInstance)

-- | The name of the Amazon Web Services Region that the stack instance is
-- associated with.
stackInstance_region :: Lens.Lens' StackInstance (Prelude.Maybe Prelude.Text)
stackInstance_region = Lens.lens (\StackInstance' {region} -> region) (\s@StackInstance' {} a -> s {region = a} :: StackInstance)

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
stackInstance_stackInstanceStatus :: Lens.Lens' StackInstance (Prelude.Maybe StackInstanceStatus)
stackInstance_stackInstanceStatus = Lens.lens (\StackInstance' {stackInstanceStatus} -> stackInstanceStatus) (\s@StackInstance' {} a -> s {stackInstanceStatus = a} :: StackInstance)

instance Data.FromJSON StackInstance where
  parseJSON =
    Data.withObject
      "StackInstance"
      ( \x ->
          StackInstance'
            Prelude.<$> (x Data..:? "Account")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..:? "StackInstanceStatus")
      )

instance Prelude.Hashable StackInstance where
  hashWithSalt _salt StackInstance' {..} =
    _salt
      `Prelude.hashWithSalt` account
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` stackInstanceStatus

instance Prelude.NFData StackInstance where
  rnf StackInstance' {..} =
    Prelude.rnf account
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf stackInstanceStatus
