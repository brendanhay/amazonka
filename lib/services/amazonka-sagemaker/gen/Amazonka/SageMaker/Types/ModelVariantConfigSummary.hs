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
-- Module      : Amazonka.SageMaker.Types.ModelVariantConfigSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelVariantConfigSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ModelInfrastructureConfig
import Amazonka.SageMaker.Types.ModelVariantStatus

-- | Summary of the deployment configuration of a model.
--
-- /See:/ 'newModelVariantConfigSummary' smart constructor.
data ModelVariantConfigSummary = ModelVariantConfigSummary'
  { -- | The name of the model.
    modelName :: Prelude.Text,
    -- | The name of the variant.
    variantName :: Prelude.Text,
    -- | The configuration of the infrastructure that the model has been deployed
    -- to.
    infrastructureConfig :: ModelInfrastructureConfig,
    -- | The status of the deployment.
    status :: ModelVariantStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelVariantConfigSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelName', 'modelVariantConfigSummary_modelName' - The name of the model.
--
-- 'variantName', 'modelVariantConfigSummary_variantName' - The name of the variant.
--
-- 'infrastructureConfig', 'modelVariantConfigSummary_infrastructureConfig' - The configuration of the infrastructure that the model has been deployed
-- to.
--
-- 'status', 'modelVariantConfigSummary_status' - The status of the deployment.
newModelVariantConfigSummary ::
  -- | 'modelName'
  Prelude.Text ->
  -- | 'variantName'
  Prelude.Text ->
  -- | 'infrastructureConfig'
  ModelInfrastructureConfig ->
  -- | 'status'
  ModelVariantStatus ->
  ModelVariantConfigSummary
newModelVariantConfigSummary
  pModelName_
  pVariantName_
  pInfrastructureConfig_
  pStatus_ =
    ModelVariantConfigSummary'
      { modelName = pModelName_,
        variantName = pVariantName_,
        infrastructureConfig = pInfrastructureConfig_,
        status = pStatus_
      }

-- | The name of the model.
modelVariantConfigSummary_modelName :: Lens.Lens' ModelVariantConfigSummary Prelude.Text
modelVariantConfigSummary_modelName = Lens.lens (\ModelVariantConfigSummary' {modelName} -> modelName) (\s@ModelVariantConfigSummary' {} a -> s {modelName = a} :: ModelVariantConfigSummary)

-- | The name of the variant.
modelVariantConfigSummary_variantName :: Lens.Lens' ModelVariantConfigSummary Prelude.Text
modelVariantConfigSummary_variantName = Lens.lens (\ModelVariantConfigSummary' {variantName} -> variantName) (\s@ModelVariantConfigSummary' {} a -> s {variantName = a} :: ModelVariantConfigSummary)

-- | The configuration of the infrastructure that the model has been deployed
-- to.
modelVariantConfigSummary_infrastructureConfig :: Lens.Lens' ModelVariantConfigSummary ModelInfrastructureConfig
modelVariantConfigSummary_infrastructureConfig = Lens.lens (\ModelVariantConfigSummary' {infrastructureConfig} -> infrastructureConfig) (\s@ModelVariantConfigSummary' {} a -> s {infrastructureConfig = a} :: ModelVariantConfigSummary)

-- | The status of the deployment.
modelVariantConfigSummary_status :: Lens.Lens' ModelVariantConfigSummary ModelVariantStatus
modelVariantConfigSummary_status = Lens.lens (\ModelVariantConfigSummary' {status} -> status) (\s@ModelVariantConfigSummary' {} a -> s {status = a} :: ModelVariantConfigSummary)

instance Data.FromJSON ModelVariantConfigSummary where
  parseJSON =
    Data.withObject
      "ModelVariantConfigSummary"
      ( \x ->
          ModelVariantConfigSummary'
            Prelude.<$> (x Data..: "ModelName")
            Prelude.<*> (x Data..: "VariantName")
            Prelude.<*> (x Data..: "InfrastructureConfig")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable ModelVariantConfigSummary where
  hashWithSalt _salt ModelVariantConfigSummary' {..} =
    _salt `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` variantName
      `Prelude.hashWithSalt` infrastructureConfig
      `Prelude.hashWithSalt` status

instance Prelude.NFData ModelVariantConfigSummary where
  rnf ModelVariantConfigSummary' {..} =
    Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf variantName
      `Prelude.seq` Prelude.rnf infrastructureConfig
      `Prelude.seq` Prelude.rnf status
