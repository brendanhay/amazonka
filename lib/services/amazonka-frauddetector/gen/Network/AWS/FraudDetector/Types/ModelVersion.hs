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
-- Module      : Network.AWS.FraudDetector.Types.ModelVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FraudDetector.Types.ModelVersion where

import qualified Network.AWS.Core as Core
import Network.AWS.FraudDetector.Types.ModelTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON ModelVersion where
  parseJSON =
    Core.withObject
      "ModelVersion"
      ( \x ->
          ModelVersion'
            Prelude.<$> (x Core..:? "arn")
            Prelude.<*> (x Core..: "modelId")
            Prelude.<*> (x Core..: "modelType")
            Prelude.<*> (x Core..: "modelVersionNumber")
      )

instance Prelude.Hashable ModelVersion

instance Prelude.NFData ModelVersion

instance Core.ToJSON ModelVersion where
  toJSON ModelVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("arn" Core..=) Prelude.<$> arn,
            Prelude.Just ("modelId" Core..= modelId),
            Prelude.Just ("modelType" Core..= modelType),
            Prelude.Just
              ("modelVersionNumber" Core..= modelVersionNumber)
          ]
      )
