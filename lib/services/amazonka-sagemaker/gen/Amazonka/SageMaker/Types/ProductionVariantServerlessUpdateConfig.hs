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
-- Module      : Amazonka.SageMaker.Types.ProductionVariantServerlessUpdateConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProductionVariantServerlessUpdateConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the serverless update concurrency configuration for an
-- endpoint variant.
--
-- /See:/ 'newProductionVariantServerlessUpdateConfig' smart constructor.
data ProductionVariantServerlessUpdateConfig = ProductionVariantServerlessUpdateConfig'
  { -- | The updated maximum number of concurrent invocations your serverless
    -- endpoint can process.
    maxConcurrency :: Prelude.Maybe Prelude.Natural,
    -- | The updated amount of provisioned concurrency to allocate for the
    -- serverless endpoint. Should be less than or equal to @MaxConcurrency@.
    provisionedConcurrency :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProductionVariantServerlessUpdateConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxConcurrency', 'productionVariantServerlessUpdateConfig_maxConcurrency' - The updated maximum number of concurrent invocations your serverless
-- endpoint can process.
--
-- 'provisionedConcurrency', 'productionVariantServerlessUpdateConfig_provisionedConcurrency' - The updated amount of provisioned concurrency to allocate for the
-- serverless endpoint. Should be less than or equal to @MaxConcurrency@.
newProductionVariantServerlessUpdateConfig ::
  ProductionVariantServerlessUpdateConfig
newProductionVariantServerlessUpdateConfig =
  ProductionVariantServerlessUpdateConfig'
    { maxConcurrency =
        Prelude.Nothing,
      provisionedConcurrency =
        Prelude.Nothing
    }

-- | The updated maximum number of concurrent invocations your serverless
-- endpoint can process.
productionVariantServerlessUpdateConfig_maxConcurrency :: Lens.Lens' ProductionVariantServerlessUpdateConfig (Prelude.Maybe Prelude.Natural)
productionVariantServerlessUpdateConfig_maxConcurrency = Lens.lens (\ProductionVariantServerlessUpdateConfig' {maxConcurrency} -> maxConcurrency) (\s@ProductionVariantServerlessUpdateConfig' {} a -> s {maxConcurrency = a} :: ProductionVariantServerlessUpdateConfig)

-- | The updated amount of provisioned concurrency to allocate for the
-- serverless endpoint. Should be less than or equal to @MaxConcurrency@.
productionVariantServerlessUpdateConfig_provisionedConcurrency :: Lens.Lens' ProductionVariantServerlessUpdateConfig (Prelude.Maybe Prelude.Natural)
productionVariantServerlessUpdateConfig_provisionedConcurrency = Lens.lens (\ProductionVariantServerlessUpdateConfig' {provisionedConcurrency} -> provisionedConcurrency) (\s@ProductionVariantServerlessUpdateConfig' {} a -> s {provisionedConcurrency = a} :: ProductionVariantServerlessUpdateConfig)

instance
  Prelude.Hashable
    ProductionVariantServerlessUpdateConfig
  where
  hashWithSalt
    _salt
    ProductionVariantServerlessUpdateConfig' {..} =
      _salt
        `Prelude.hashWithSalt` maxConcurrency
        `Prelude.hashWithSalt` provisionedConcurrency

instance
  Prelude.NFData
    ProductionVariantServerlessUpdateConfig
  where
  rnf ProductionVariantServerlessUpdateConfig' {..} =
    Prelude.rnf maxConcurrency
      `Prelude.seq` Prelude.rnf provisionedConcurrency

instance
  Data.ToJSON
    ProductionVariantServerlessUpdateConfig
  where
  toJSON ProductionVariantServerlessUpdateConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxConcurrency" Data..=)
              Prelude.<$> maxConcurrency,
            ("ProvisionedConcurrency" Data..=)
              Prelude.<$> provisionedConcurrency
          ]
      )
