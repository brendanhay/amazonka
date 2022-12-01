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
-- Module      : Amazonka.DrS.Types.RecoveryInstanceDataReplicationInitiationStep
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.RecoveryInstanceDataReplicationInitiationStep where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DrS.Types.RecoveryInstanceDataReplicationInitiationStepName
import Amazonka.DrS.Types.RecoveryInstanceDataReplicationInitiationStepStatus
import qualified Amazonka.Prelude as Prelude

-- | Data replication initiation step.
--
-- /See:/ 'newRecoveryInstanceDataReplicationInitiationStep' smart constructor.
data RecoveryInstanceDataReplicationInitiationStep = RecoveryInstanceDataReplicationInitiationStep'
  { -- | The name of the step.
    name :: Prelude.Maybe RecoveryInstanceDataReplicationInitiationStepName,
    -- | The status of the step.
    status :: Prelude.Maybe RecoveryInstanceDataReplicationInitiationStepStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecoveryInstanceDataReplicationInitiationStep' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'recoveryInstanceDataReplicationInitiationStep_name' - The name of the step.
--
-- 'status', 'recoveryInstanceDataReplicationInitiationStep_status' - The status of the step.
newRecoveryInstanceDataReplicationInitiationStep ::
  RecoveryInstanceDataReplicationInitiationStep
newRecoveryInstanceDataReplicationInitiationStep =
  RecoveryInstanceDataReplicationInitiationStep'
    { name =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The name of the step.
recoveryInstanceDataReplicationInitiationStep_name :: Lens.Lens' RecoveryInstanceDataReplicationInitiationStep (Prelude.Maybe RecoveryInstanceDataReplicationInitiationStepName)
recoveryInstanceDataReplicationInitiationStep_name = Lens.lens (\RecoveryInstanceDataReplicationInitiationStep' {name} -> name) (\s@RecoveryInstanceDataReplicationInitiationStep' {} a -> s {name = a} :: RecoveryInstanceDataReplicationInitiationStep)

-- | The status of the step.
recoveryInstanceDataReplicationInitiationStep_status :: Lens.Lens' RecoveryInstanceDataReplicationInitiationStep (Prelude.Maybe RecoveryInstanceDataReplicationInitiationStepStatus)
recoveryInstanceDataReplicationInitiationStep_status = Lens.lens (\RecoveryInstanceDataReplicationInitiationStep' {status} -> status) (\s@RecoveryInstanceDataReplicationInitiationStep' {} a -> s {status = a} :: RecoveryInstanceDataReplicationInitiationStep)

instance
  Core.FromJSON
    RecoveryInstanceDataReplicationInitiationStep
  where
  parseJSON =
    Core.withObject
      "RecoveryInstanceDataReplicationInitiationStep"
      ( \x ->
          RecoveryInstanceDataReplicationInitiationStep'
            Prelude.<$> (x Core..:? "name")
              Prelude.<*> (x Core..:? "status")
      )

instance
  Prelude.Hashable
    RecoveryInstanceDataReplicationInitiationStep
  where
  hashWithSalt
    _salt
    RecoveryInstanceDataReplicationInitiationStep' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    RecoveryInstanceDataReplicationInitiationStep
  where
  rnf
    RecoveryInstanceDataReplicationInitiationStep' {..} =
      Prelude.rnf name `Prelude.seq` Prelude.rnf status
