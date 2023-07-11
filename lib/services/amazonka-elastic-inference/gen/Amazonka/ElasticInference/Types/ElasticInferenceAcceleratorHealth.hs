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
-- Module      : Amazonka.ElasticInference.Types.ElasticInferenceAcceleratorHealth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticInference.Types.ElasticInferenceAcceleratorHealth where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The health details of an Elastic Inference Accelerator.
--
-- /See:/ 'newElasticInferenceAcceleratorHealth' smart constructor.
data ElasticInferenceAcceleratorHealth = ElasticInferenceAcceleratorHealth'
  { -- | The health status of the Elastic Inference Accelerator.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ElasticInferenceAcceleratorHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'elasticInferenceAcceleratorHealth_status' - The health status of the Elastic Inference Accelerator.
newElasticInferenceAcceleratorHealth ::
  ElasticInferenceAcceleratorHealth
newElasticInferenceAcceleratorHealth =
  ElasticInferenceAcceleratorHealth'
    { status =
        Prelude.Nothing
    }

-- | The health status of the Elastic Inference Accelerator.
elasticInferenceAcceleratorHealth_status :: Lens.Lens' ElasticInferenceAcceleratorHealth (Prelude.Maybe Prelude.Text)
elasticInferenceAcceleratorHealth_status = Lens.lens (\ElasticInferenceAcceleratorHealth' {status} -> status) (\s@ElasticInferenceAcceleratorHealth' {} a -> s {status = a} :: ElasticInferenceAcceleratorHealth)

instance
  Data.FromJSON
    ElasticInferenceAcceleratorHealth
  where
  parseJSON =
    Data.withObject
      "ElasticInferenceAcceleratorHealth"
      ( \x ->
          ElasticInferenceAcceleratorHealth'
            Prelude.<$> (x Data..:? "status")
      )

instance
  Prelude.Hashable
    ElasticInferenceAcceleratorHealth
  where
  hashWithSalt
    _salt
    ElasticInferenceAcceleratorHealth' {..} =
      _salt `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    ElasticInferenceAcceleratorHealth
  where
  rnf ElasticInferenceAcceleratorHealth' {..} =
    Prelude.rnf status
