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
-- Module      : Amazonka.DrS.Types.DataReplicationInitiationStep
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.DataReplicationInitiationStep where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DrS.Types.DataReplicationInitiationStepName
import Amazonka.DrS.Types.DataReplicationInitiationStepStatus
import qualified Amazonka.Prelude as Prelude

-- | Data replication initiation step.
--
-- /See:/ 'newDataReplicationInitiationStep' smart constructor.
data DataReplicationInitiationStep = DataReplicationInitiationStep'
  { -- | The name of the step.
    name :: Prelude.Maybe DataReplicationInitiationStepName,
    -- | The status of the step.
    status :: Prelude.Maybe DataReplicationInitiationStepStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataReplicationInitiationStep' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'dataReplicationInitiationStep_name' - The name of the step.
--
-- 'status', 'dataReplicationInitiationStep_status' - The status of the step.
newDataReplicationInitiationStep ::
  DataReplicationInitiationStep
newDataReplicationInitiationStep =
  DataReplicationInitiationStep'
    { name =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The name of the step.
dataReplicationInitiationStep_name :: Lens.Lens' DataReplicationInitiationStep (Prelude.Maybe DataReplicationInitiationStepName)
dataReplicationInitiationStep_name = Lens.lens (\DataReplicationInitiationStep' {name} -> name) (\s@DataReplicationInitiationStep' {} a -> s {name = a} :: DataReplicationInitiationStep)

-- | The status of the step.
dataReplicationInitiationStep_status :: Lens.Lens' DataReplicationInitiationStep (Prelude.Maybe DataReplicationInitiationStepStatus)
dataReplicationInitiationStep_status = Lens.lens (\DataReplicationInitiationStep' {status} -> status) (\s@DataReplicationInitiationStep' {} a -> s {status = a} :: DataReplicationInitiationStep)

instance Core.FromJSON DataReplicationInitiationStep where
  parseJSON =
    Core.withObject
      "DataReplicationInitiationStep"
      ( \x ->
          DataReplicationInitiationStep'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "status")
      )

instance
  Prelude.Hashable
    DataReplicationInitiationStep
  where
  hashWithSalt _salt DataReplicationInitiationStep' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData DataReplicationInitiationStep where
  rnf DataReplicationInitiationStep' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf status
