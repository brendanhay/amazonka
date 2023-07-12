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
-- Module      : Amazonka.FraudDetector.Types.ModelVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.ModelVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.ModelTypeEnum
import qualified Amazonka.Prelude as Prelude

-- | The model version.
--
-- /See:/ 'newModelVersion' smart constructor.
data ModelVersion = ModelVersion'
  { -- | The model version ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The model ID.
    modelId :: Prelude.Text,
    -- | The model type.
    modelType :: ModelTypeEnum,
    -- | The model version number.
    modelVersionNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'modelVersion_arn' - The model version ARN.
--
-- 'modelId', 'modelVersion_modelId' - The model ID.
--
-- 'modelType', 'modelVersion_modelType' - The model type.
--
-- 'modelVersionNumber', 'modelVersion_modelVersionNumber' - The model version number.
newModelVersion ::
  -- | 'modelId'
  Prelude.Text ->
  -- | 'modelType'
  ModelTypeEnum ->
  -- | 'modelVersionNumber'
  Prelude.Text ->
  ModelVersion
newModelVersion
  pModelId_
  pModelType_
  pModelVersionNumber_ =
    ModelVersion'
      { arn = Prelude.Nothing,
        modelId = pModelId_,
        modelType = pModelType_,
        modelVersionNumber = pModelVersionNumber_
      }

-- | The model version ARN.
modelVersion_arn :: Lens.Lens' ModelVersion (Prelude.Maybe Prelude.Text)
modelVersion_arn = Lens.lens (\ModelVersion' {arn} -> arn) (\s@ModelVersion' {} a -> s {arn = a} :: ModelVersion)

-- | The model ID.
modelVersion_modelId :: Lens.Lens' ModelVersion Prelude.Text
modelVersion_modelId = Lens.lens (\ModelVersion' {modelId} -> modelId) (\s@ModelVersion' {} a -> s {modelId = a} :: ModelVersion)

-- | The model type.
modelVersion_modelType :: Lens.Lens' ModelVersion ModelTypeEnum
modelVersion_modelType = Lens.lens (\ModelVersion' {modelType} -> modelType) (\s@ModelVersion' {} a -> s {modelType = a} :: ModelVersion)

-- | The model version number.
modelVersion_modelVersionNumber :: Lens.Lens' ModelVersion Prelude.Text
modelVersion_modelVersionNumber = Lens.lens (\ModelVersion' {modelVersionNumber} -> modelVersionNumber) (\s@ModelVersion' {} a -> s {modelVersionNumber = a} :: ModelVersion)

instance Data.FromJSON ModelVersion where
  parseJSON =
    Data.withObject
      "ModelVersion"
      ( \x ->
          ModelVersion'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..: "modelId")
            Prelude.<*> (x Data..: "modelType")
            Prelude.<*> (x Data..: "modelVersionNumber")
      )

instance Prelude.Hashable ModelVersion where
  hashWithSalt _salt ModelVersion' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` modelId
      `Prelude.hashWithSalt` modelType
      `Prelude.hashWithSalt` modelVersionNumber

instance Prelude.NFData ModelVersion where
  rnf ModelVersion' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf modelType
      `Prelude.seq` Prelude.rnf modelVersionNumber

instance Data.ToJSON ModelVersion where
  toJSON ModelVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("arn" Data..=) Prelude.<$> arn,
            Prelude.Just ("modelId" Data..= modelId),
            Prelude.Just ("modelType" Data..= modelType),
            Prelude.Just
              ("modelVersionNumber" Data..= modelVersionNumber)
          ]
      )
