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
-- Module      : Amazonka.CloudFormation.Types.ManagedExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.ManagedExecution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes whether StackSets performs non-conflicting operations
-- concurrently and queues conflicting operations.
--
-- /See:/ 'newManagedExecution' smart constructor.
data ManagedExecution = ManagedExecution'
  { -- | When @true@, StackSets performs non-conflicting operations concurrently
    -- and queues conflicting operations. After conflicting operations finish,
    -- StackSets starts queued operations in request order.
    --
    -- If there are already running or queued operations, StackSets queues all
    -- incoming operations even if they are non-conflicting.
    --
    -- You can\'t modify your stack set\'s execution configuration while there
    -- are running or queued operations for that stack set.
    --
    -- When @false@ (default), StackSets performs one operation at a time in
    -- request order.
    active :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagedExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'active', 'managedExecution_active' - When @true@, StackSets performs non-conflicting operations concurrently
-- and queues conflicting operations. After conflicting operations finish,
-- StackSets starts queued operations in request order.
--
-- If there are already running or queued operations, StackSets queues all
-- incoming operations even if they are non-conflicting.
--
-- You can\'t modify your stack set\'s execution configuration while there
-- are running or queued operations for that stack set.
--
-- When @false@ (default), StackSets performs one operation at a time in
-- request order.
newManagedExecution ::
  ManagedExecution
newManagedExecution =
  ManagedExecution' {active = Prelude.Nothing}

-- | When @true@, StackSets performs non-conflicting operations concurrently
-- and queues conflicting operations. After conflicting operations finish,
-- StackSets starts queued operations in request order.
--
-- If there are already running or queued operations, StackSets queues all
-- incoming operations even if they are non-conflicting.
--
-- You can\'t modify your stack set\'s execution configuration while there
-- are running or queued operations for that stack set.
--
-- When @false@ (default), StackSets performs one operation at a time in
-- request order.
managedExecution_active :: Lens.Lens' ManagedExecution (Prelude.Maybe Prelude.Bool)
managedExecution_active = Lens.lens (\ManagedExecution' {active} -> active) (\s@ManagedExecution' {} a -> s {active = a} :: ManagedExecution)

instance Core.FromXML ManagedExecution where
  parseXML x =
    ManagedExecution' Prelude.<$> (x Core..@? "Active")

instance Prelude.Hashable ManagedExecution where
  hashWithSalt _salt ManagedExecution' {..} =
    _salt `Prelude.hashWithSalt` active

instance Prelude.NFData ManagedExecution where
  rnf ManagedExecution' {..} = Prelude.rnf active

instance Core.ToQuery ManagedExecution where
  toQuery ManagedExecution' {..} =
    Prelude.mconcat ["Active" Core.=: active]
