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
-- Module      : Amazonka.SageMaker.Types.DataQualityJobInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DataQualityJobInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.BatchTransformInput
import Amazonka.SageMaker.Types.EndpointInput

-- | The input for the data quality monitoring job. Currently endpoints are
-- supported for input.
--
-- /See:/ 'newDataQualityJobInput' smart constructor.
data DataQualityJobInput = DataQualityJobInput'
  { -- | Input object for the batch transform job.
    batchTransformInput :: Prelude.Maybe BatchTransformInput,
    endpointInput :: Prelude.Maybe EndpointInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataQualityJobInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchTransformInput', 'dataQualityJobInput_batchTransformInput' - Input object for the batch transform job.
--
-- 'endpointInput', 'dataQualityJobInput_endpointInput' - Undocumented member.
newDataQualityJobInput ::
  DataQualityJobInput
newDataQualityJobInput =
  DataQualityJobInput'
    { batchTransformInput =
        Prelude.Nothing,
      endpointInput = Prelude.Nothing
    }

-- | Input object for the batch transform job.
dataQualityJobInput_batchTransformInput :: Lens.Lens' DataQualityJobInput (Prelude.Maybe BatchTransformInput)
dataQualityJobInput_batchTransformInput = Lens.lens (\DataQualityJobInput' {batchTransformInput} -> batchTransformInput) (\s@DataQualityJobInput' {} a -> s {batchTransformInput = a} :: DataQualityJobInput)

-- | Undocumented member.
dataQualityJobInput_endpointInput :: Lens.Lens' DataQualityJobInput (Prelude.Maybe EndpointInput)
dataQualityJobInput_endpointInput = Lens.lens (\DataQualityJobInput' {endpointInput} -> endpointInput) (\s@DataQualityJobInput' {} a -> s {endpointInput = a} :: DataQualityJobInput)

instance Data.FromJSON DataQualityJobInput where
  parseJSON =
    Data.withObject
      "DataQualityJobInput"
      ( \x ->
          DataQualityJobInput'
            Prelude.<$> (x Data..:? "BatchTransformInput")
            Prelude.<*> (x Data..:? "EndpointInput")
      )

instance Prelude.Hashable DataQualityJobInput where
  hashWithSalt _salt DataQualityJobInput' {..} =
    _salt `Prelude.hashWithSalt` batchTransformInput
      `Prelude.hashWithSalt` endpointInput

instance Prelude.NFData DataQualityJobInput where
  rnf DataQualityJobInput' {..} =
    Prelude.rnf batchTransformInput
      `Prelude.seq` Prelude.rnf endpointInput

instance Data.ToJSON DataQualityJobInput where
  toJSON DataQualityJobInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BatchTransformInput" Data..=)
              Prelude.<$> batchTransformInput,
            ("EndpointInput" Data..=) Prelude.<$> endpointInput
          ]
      )
