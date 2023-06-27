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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Cloud9.Types.EnvironmentLifecycle where

import Amazonka.Cloud9.Types.EnvironmentLifecycleStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the current creation or deletion lifecycle state of an
-- Cloud9 development environment.
--
-- /See:/ 'newEnvironmentLifecycle' smart constructor.
data EnvironmentLifecycle = EnvironmentLifecycle'
  { -- | If the environment failed to delete, the Amazon Resource Name (ARN) of
    -- the related Amazon Web Services resource.
    failureResource :: Prelude.Maybe Prelude.Text,
    -- | Any informational message about the lifecycle state of the environment.
    reason :: Prelude.Maybe Prelude.Text,
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
    status :: Prelude.Maybe EnvironmentLifecycleStatus
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
-- 'failureResource', 'environmentLifecycle_failureResource' - If the environment failed to delete, the Amazon Resource Name (ARN) of
-- the related Amazon Web Services resource.
--
-- 'reason', 'environmentLifecycle_reason' - Any informational message about the lifecycle state of the environment.
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
newEnvironmentLifecycle ::
  EnvironmentLifecycle
newEnvironmentLifecycle =
  EnvironmentLifecycle'
    { failureResource =
        Prelude.Nothing,
      reason = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | If the environment failed to delete, the Amazon Resource Name (ARN) of
-- the related Amazon Web Services resource.
environmentLifecycle_failureResource :: Lens.Lens' EnvironmentLifecycle (Prelude.Maybe Prelude.Text)
environmentLifecycle_failureResource = Lens.lens (\EnvironmentLifecycle' {failureResource} -> failureResource) (\s@EnvironmentLifecycle' {} a -> s {failureResource = a} :: EnvironmentLifecycle)

-- | Any informational message about the lifecycle state of the environment.
environmentLifecycle_reason :: Lens.Lens' EnvironmentLifecycle (Prelude.Maybe Prelude.Text)
environmentLifecycle_reason = Lens.lens (\EnvironmentLifecycle' {reason} -> reason) (\s@EnvironmentLifecycle' {} a -> s {reason = a} :: EnvironmentLifecycle)

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

instance Data.FromJSON EnvironmentLifecycle where
  parseJSON =
    Data.withObject
      "EnvironmentLifecycle"
      ( \x ->
          EnvironmentLifecycle'
            Prelude.<$> (x Data..:? "failureResource")
            Prelude.<*> (x Data..:? "reason")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable EnvironmentLifecycle where
  hashWithSalt _salt EnvironmentLifecycle' {..} =
    _salt
      `Prelude.hashWithSalt` failureResource
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` status

instance Prelude.NFData EnvironmentLifecycle where
  rnf EnvironmentLifecycle' {..} =
    Prelude.rnf failureResource
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf status
