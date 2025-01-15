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
-- Module      : Amazonka.SageMaker.Types.ModelInfrastructureConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelInfrastructureConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ModelInfrastructureType
import Amazonka.SageMaker.Types.RealTimeInferenceConfig

-- | The configuration for the infrastructure that the model will be deployed
-- to.
--
-- /See:/ 'newModelInfrastructureConfig' smart constructor.
data ModelInfrastructureConfig = ModelInfrastructureConfig'
  { -- | The inference option to which to deploy your model. Possible values are
    -- the following:
    --
    -- -   @RealTime@: Deploy to real-time inference.
    infrastructureType :: ModelInfrastructureType,
    -- | The infrastructure configuration for deploying the model to real-time
    -- inference.
    realTimeInferenceConfig :: RealTimeInferenceConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelInfrastructureConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'infrastructureType', 'modelInfrastructureConfig_infrastructureType' - The inference option to which to deploy your model. Possible values are
-- the following:
--
-- -   @RealTime@: Deploy to real-time inference.
--
-- 'realTimeInferenceConfig', 'modelInfrastructureConfig_realTimeInferenceConfig' - The infrastructure configuration for deploying the model to real-time
-- inference.
newModelInfrastructureConfig ::
  -- | 'infrastructureType'
  ModelInfrastructureType ->
  -- | 'realTimeInferenceConfig'
  RealTimeInferenceConfig ->
  ModelInfrastructureConfig
newModelInfrastructureConfig
  pInfrastructureType_
  pRealTimeInferenceConfig_ =
    ModelInfrastructureConfig'
      { infrastructureType =
          pInfrastructureType_,
        realTimeInferenceConfig =
          pRealTimeInferenceConfig_
      }

-- | The inference option to which to deploy your model. Possible values are
-- the following:
--
-- -   @RealTime@: Deploy to real-time inference.
modelInfrastructureConfig_infrastructureType :: Lens.Lens' ModelInfrastructureConfig ModelInfrastructureType
modelInfrastructureConfig_infrastructureType = Lens.lens (\ModelInfrastructureConfig' {infrastructureType} -> infrastructureType) (\s@ModelInfrastructureConfig' {} a -> s {infrastructureType = a} :: ModelInfrastructureConfig)

-- | The infrastructure configuration for deploying the model to real-time
-- inference.
modelInfrastructureConfig_realTimeInferenceConfig :: Lens.Lens' ModelInfrastructureConfig RealTimeInferenceConfig
modelInfrastructureConfig_realTimeInferenceConfig = Lens.lens (\ModelInfrastructureConfig' {realTimeInferenceConfig} -> realTimeInferenceConfig) (\s@ModelInfrastructureConfig' {} a -> s {realTimeInferenceConfig = a} :: ModelInfrastructureConfig)

instance Data.FromJSON ModelInfrastructureConfig where
  parseJSON =
    Data.withObject
      "ModelInfrastructureConfig"
      ( \x ->
          ModelInfrastructureConfig'
            Prelude.<$> (x Data..: "InfrastructureType")
            Prelude.<*> (x Data..: "RealTimeInferenceConfig")
      )

instance Prelude.Hashable ModelInfrastructureConfig where
  hashWithSalt _salt ModelInfrastructureConfig' {..} =
    _salt
      `Prelude.hashWithSalt` infrastructureType
      `Prelude.hashWithSalt` realTimeInferenceConfig

instance Prelude.NFData ModelInfrastructureConfig where
  rnf ModelInfrastructureConfig' {..} =
    Prelude.rnf infrastructureType `Prelude.seq`
      Prelude.rnf realTimeInferenceConfig

instance Data.ToJSON ModelInfrastructureConfig where
  toJSON ModelInfrastructureConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("InfrastructureType" Data..= infrastructureType),
            Prelude.Just
              ( "RealTimeInferenceConfig"
                  Data..= realTimeInferenceConfig
              )
          ]
      )
