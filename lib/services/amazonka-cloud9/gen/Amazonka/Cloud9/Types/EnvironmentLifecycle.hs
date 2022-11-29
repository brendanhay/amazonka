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
-- Module      : Amazonka.Cloud9.Types.EnvironmentLifecycle
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Cloud9.Types.EnvironmentLifecycle where

import Amazonka.Cloud9.Types.EnvironmentLifecycleStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the current creation or deletion lifecycle state of an
-- Cloud9 development environment.
--
-- /See:/ 'newEnvironmentLifecycle' smart constructor.
data EnvironmentLifecycle = EnvironmentLifecycle'
  { -- | The current creation or deletion lifecycle state of the environment.
    --
    -- -   @CREATING@: The environment is in the process of being created.
    --
    -- -   @CREATED@: The environment was successfully created.
    --
    -- -   @CREATE_FAILED@: The environment failed to be created.
    --
    -- -   @DELETING@: The environment is in the process of being deleted.
    --
    -- -   @DELETE_FAILED@: The environment failed to delete.
    status :: Prelude.Maybe EnvironmentLifecycleStatus,
    -- | Any informational message about the lifecycle state of the environment.
    reason :: Prelude.Maybe Prelude.Text,
    -- | If the environment failed to delete, the Amazon Resource Name (ARN) of
    -- the related Amazon Web Services resource.
    failureResource :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentLifecycle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'environmentLifecycle_status' - The current creation or deletion lifecycle state of the environment.
--
-- -   @CREATING@: The environment is in the process of being created.
--
-- -   @CREATED@: The environment was successfully created.
--
-- -   @CREATE_FAILED@: The environment failed to be created.
--
-- -   @DELETING@: The environment is in the process of being deleted.
--
-- -   @DELETE_FAILED@: The environment failed to delete.
--
-- 'reason', 'environmentLifecycle_reason' - Any informational message about the lifecycle state of the environment.
--
-- 'failureResource', 'environmentLifecycle_failureResource' - If the environment failed to delete, the Amazon Resource Name (ARN) of
-- the related Amazon Web Services resource.
newEnvironmentLifecycle ::
  EnvironmentLifecycle
newEnvironmentLifecycle =
  EnvironmentLifecycle'
    { status = Prelude.Nothing,
      reason = Prelude.Nothing,
      failureResource = Prelude.Nothing
    }

-- | The current creation or deletion lifecycle state of the environment.
--
-- -   @CREATING@: The environment is in the process of being created.
--
-- -   @CREATED@: The environment was successfully created.
--
-- -   @CREATE_FAILED@: The environment failed to be created.
--
-- -   @DELETING@: The environment is in the process of being deleted.
--
-- -   @DELETE_FAILED@: The environment failed to delete.
environmentLifecycle_status :: Lens.Lens' EnvironmentLifecycle (Prelude.Maybe EnvironmentLifecycleStatus)
environmentLifecycle_status = Lens.lens (\EnvironmentLifecycle' {status} -> status) (\s@EnvironmentLifecycle' {} a -> s {status = a} :: EnvironmentLifecycle)

-- | Any informational message about the lifecycle state of the environment.
environmentLifecycle_reason :: Lens.Lens' EnvironmentLifecycle (Prelude.Maybe Prelude.Text)
environmentLifecycle_reason = Lens.lens (\EnvironmentLifecycle' {reason} -> reason) (\s@EnvironmentLifecycle' {} a -> s {reason = a} :: EnvironmentLifecycle)

-- | If the environment failed to delete, the Amazon Resource Name (ARN) of
-- the related Amazon Web Services resource.
environmentLifecycle_failureResource :: Lens.Lens' EnvironmentLifecycle (Prelude.Maybe Prelude.Text)
environmentLifecycle_failureResource = Lens.lens (\EnvironmentLifecycle' {failureResource} -> failureResource) (\s@EnvironmentLifecycle' {} a -> s {failureResource = a} :: EnvironmentLifecycle)

instance Core.FromJSON EnvironmentLifecycle where
  parseJSON =
    Core.withObject
      "EnvironmentLifecycle"
      ( \x ->
          EnvironmentLifecycle'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "reason")
            Prelude.<*> (x Core..:? "failureResource")
      )

instance Prelude.Hashable EnvironmentLifecycle where
  hashWithSalt _salt EnvironmentLifecycle' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` failureResource

instance Prelude.NFData EnvironmentLifecycle where
  rnf EnvironmentLifecycle' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf failureResource
