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
-- Module      : Amazonka.SageMaker.Types.ProductionVariantServerlessConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProductionVariantServerlessConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the serverless configuration for an endpoint variant.
--
-- /See:/ 'newProductionVariantServerlessConfig' smart constructor.
data ProductionVariantServerlessConfig = ProductionVariantServerlessConfig'
  { -- | The memory size of your serverless endpoint. Valid values are in 1 GB
    -- increments: 1024 MB, 2048 MB, 3072 MB, 4096 MB, 5120 MB, or 6144 MB.
    memorySizeInMB :: Prelude.Natural,
    -- | The maximum number of concurrent invocations your serverless endpoint
    -- can process.
    maxConcurrency :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProductionVariantServerlessConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memorySizeInMB', 'productionVariantServerlessConfig_memorySizeInMB' - The memory size of your serverless endpoint. Valid values are in 1 GB
-- increments: 1024 MB, 2048 MB, 3072 MB, 4096 MB, 5120 MB, or 6144 MB.
--
-- 'maxConcurrency', 'productionVariantServerlessConfig_maxConcurrency' - The maximum number of concurrent invocations your serverless endpoint
-- can process.
newProductionVariantServerlessConfig ::
  -- | 'memorySizeInMB'
  Prelude.Natural ->
  -- | 'maxConcurrency'
  Prelude.Natural ->
  ProductionVariantServerlessConfig
newProductionVariantServerlessConfig
  pMemorySizeInMB_
  pMaxConcurrency_ =
    ProductionVariantServerlessConfig'
      { memorySizeInMB =
          pMemorySizeInMB_,
        maxConcurrency = pMaxConcurrency_
      }

-- | The memory size of your serverless endpoint. Valid values are in 1 GB
-- increments: 1024 MB, 2048 MB, 3072 MB, 4096 MB, 5120 MB, or 6144 MB.
productionVariantServerlessConfig_memorySizeInMB :: Lens.Lens' ProductionVariantServerlessConfig Prelude.Natural
productionVariantServerlessConfig_memorySizeInMB = Lens.lens (\ProductionVariantServerlessConfig' {memorySizeInMB} -> memorySizeInMB) (\s@ProductionVariantServerlessConfig' {} a -> s {memorySizeInMB = a} :: ProductionVariantServerlessConfig)

-- | The maximum number of concurrent invocations your serverless endpoint
-- can process.
productionVariantServerlessConfig_maxConcurrency :: Lens.Lens' ProductionVariantServerlessConfig Prelude.Natural
productionVariantServerlessConfig_maxConcurrency = Lens.lens (\ProductionVariantServerlessConfig' {maxConcurrency} -> maxConcurrency) (\s@ProductionVariantServerlessConfig' {} a -> s {maxConcurrency = a} :: ProductionVariantServerlessConfig)

instance
  Data.FromJSON
    ProductionVariantServerlessConfig
  where
  parseJSON =
    Data.withObject
      "ProductionVariantServerlessConfig"
      ( \x ->
          ProductionVariantServerlessConfig'
            Prelude.<$> (x Data..: "MemorySizeInMB")
            Prelude.<*> (x Data..: "MaxConcurrency")
      )

instance
  Prelude.Hashable
    ProductionVariantServerlessConfig
  where
  hashWithSalt
    _salt
    ProductionVariantServerlessConfig' {..} =
      _salt
        `Prelude.hashWithSalt` memorySizeInMB
        `Prelude.hashWithSalt` maxConcurrency

instance
  Prelude.NFData
    ProductionVariantServerlessConfig
  where
  rnf ProductionVariantServerlessConfig' {..} =
    Prelude.rnf memorySizeInMB `Prelude.seq`
      Prelude.rnf maxConcurrency

instance
  Data.ToJSON
    ProductionVariantServerlessConfig
  where
  toJSON ProductionVariantServerlessConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("MemorySizeInMB" Data..= memorySizeInMB),
            Prelude.Just
              ("MaxConcurrency" Data..= maxConcurrency)
          ]
      )
