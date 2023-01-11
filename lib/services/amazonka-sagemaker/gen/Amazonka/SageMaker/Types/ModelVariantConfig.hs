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
-- Module      : Amazonka.SageMaker.Types.ModelVariantConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelVariantConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ModelInfrastructureConfig

-- | Contains information about the deployment options of a model.
--
-- /See:/ 'newModelVariantConfig' smart constructor.
data ModelVariantConfig = ModelVariantConfig'
  { -- | The name of the Amazon SageMaker Model entity.
    modelName :: Prelude.Text,
    -- | The name of the variant.
    variantName :: Prelude.Text,
    -- | The configuration for the infrastructure that the model will be deployed
    -- to.
    infrastructureConfig :: ModelInfrastructureConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelVariantConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelName', 'modelVariantConfig_modelName' - The name of the Amazon SageMaker Model entity.
--
-- 'variantName', 'modelVariantConfig_variantName' - The name of the variant.
--
-- 'infrastructureConfig', 'modelVariantConfig_infrastructureConfig' - The configuration for the infrastructure that the model will be deployed
-- to.
newModelVariantConfig ::
  -- | 'modelName'
  Prelude.Text ->
  -- | 'variantName'
  Prelude.Text ->
  -- | 'infrastructureConfig'
  ModelInfrastructureConfig ->
  ModelVariantConfig
newModelVariantConfig
  pModelName_
  pVariantName_
  pInfrastructureConfig_ =
    ModelVariantConfig'
      { modelName = pModelName_,
        variantName = pVariantName_,
        infrastructureConfig = pInfrastructureConfig_
      }

-- | The name of the Amazon SageMaker Model entity.
modelVariantConfig_modelName :: Lens.Lens' ModelVariantConfig Prelude.Text
modelVariantConfig_modelName = Lens.lens (\ModelVariantConfig' {modelName} -> modelName) (\s@ModelVariantConfig' {} a -> s {modelName = a} :: ModelVariantConfig)

-- | The name of the variant.
modelVariantConfig_variantName :: Lens.Lens' ModelVariantConfig Prelude.Text
modelVariantConfig_variantName = Lens.lens (\ModelVariantConfig' {variantName} -> variantName) (\s@ModelVariantConfig' {} a -> s {variantName = a} :: ModelVariantConfig)

-- | The configuration for the infrastructure that the model will be deployed
-- to.
modelVariantConfig_infrastructureConfig :: Lens.Lens' ModelVariantConfig ModelInfrastructureConfig
modelVariantConfig_infrastructureConfig = Lens.lens (\ModelVariantConfig' {infrastructureConfig} -> infrastructureConfig) (\s@ModelVariantConfig' {} a -> s {infrastructureConfig = a} :: ModelVariantConfig)

instance Prelude.Hashable ModelVariantConfig where
  hashWithSalt _salt ModelVariantConfig' {..} =
    _salt `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` variantName
      `Prelude.hashWithSalt` infrastructureConfig

instance Prelude.NFData ModelVariantConfig where
  rnf ModelVariantConfig' {..} =
    Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf variantName
      `Prelude.seq` Prelude.rnf infrastructureConfig

instance Data.ToJSON ModelVariantConfig where
  toJSON ModelVariantConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ModelName" Data..= modelName),
            Prelude.Just ("VariantName" Data..= variantName),
            Prelude.Just
              ( "InfrastructureConfig"
                  Data..= infrastructureConfig
              )
          ]
      )
