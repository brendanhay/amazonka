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
-- Module      : Amazonka.SageMaker.Types.MonitoringInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.BatchTransformInput
import Amazonka.SageMaker.Types.EndpointInput

-- | The inputs for a monitoring job.
--
-- /See:/ 'newMonitoringInput' smart constructor.
data MonitoringInput = MonitoringInput'
  { -- | Input object for the batch transform job.
    batchTransformInput :: Prelude.Maybe BatchTransformInput,
    -- | The endpoint for a monitoring job.
    endpointInput :: Prelude.Maybe EndpointInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchTransformInput', 'monitoringInput_batchTransformInput' - Input object for the batch transform job.
--
-- 'endpointInput', 'monitoringInput_endpointInput' - The endpoint for a monitoring job.
newMonitoringInput ::
  MonitoringInput
newMonitoringInput =
  MonitoringInput'
    { batchTransformInput =
        Prelude.Nothing,
      endpointInput = Prelude.Nothing
    }

-- | Input object for the batch transform job.
monitoringInput_batchTransformInput :: Lens.Lens' MonitoringInput (Prelude.Maybe BatchTransformInput)
monitoringInput_batchTransformInput = Lens.lens (\MonitoringInput' {batchTransformInput} -> batchTransformInput) (\s@MonitoringInput' {} a -> s {batchTransformInput = a} :: MonitoringInput)

-- | The endpoint for a monitoring job.
monitoringInput_endpointInput :: Lens.Lens' MonitoringInput (Prelude.Maybe EndpointInput)
monitoringInput_endpointInput = Lens.lens (\MonitoringInput' {endpointInput} -> endpointInput) (\s@MonitoringInput' {} a -> s {endpointInput = a} :: MonitoringInput)

instance Data.FromJSON MonitoringInput where
  parseJSON =
    Data.withObject
      "MonitoringInput"
      ( \x ->
          MonitoringInput'
            Prelude.<$> (x Data..:? "BatchTransformInput")
            Prelude.<*> (x Data..:? "EndpointInput")
      )

instance Prelude.Hashable MonitoringInput where
  hashWithSalt _salt MonitoringInput' {..} =
    _salt
      `Prelude.hashWithSalt` batchTransformInput
      `Prelude.hashWithSalt` endpointInput

instance Prelude.NFData MonitoringInput where
  rnf MonitoringInput' {..} =
    Prelude.rnf batchTransformInput `Prelude.seq`
      Prelude.rnf endpointInput

instance Data.ToJSON MonitoringInput where
  toJSON MonitoringInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BatchTransformInput" Data..=)
              Prelude.<$> batchTransformInput,
            ("EndpointInput" Data..=) Prelude.<$> endpointInput
          ]
      )
