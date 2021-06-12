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
-- Module      : Network.AWS.SageMaker.Types.EdgeModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EdgeModel where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The model on the edge device.
--
-- /See:/ 'newEdgeModel' smart constructor.
data EdgeModel = EdgeModel'
  { -- | The timestamp of the last inference that was made.
    latestInference :: Core.Maybe Core.POSIX,
    -- | The timestamp of the last data sample taken.
    latestSampleTime :: Core.Maybe Core.POSIX,
    -- | The name of the model.
    modelName :: Core.Text,
    -- | The model version.
    modelVersion :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EdgeModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latestInference', 'edgeModel_latestInference' - The timestamp of the last inference that was made.
--
-- 'latestSampleTime', 'edgeModel_latestSampleTime' - The timestamp of the last data sample taken.
--
-- 'modelName', 'edgeModel_modelName' - The name of the model.
--
-- 'modelVersion', 'edgeModel_modelVersion' - The model version.
newEdgeModel ::
  -- | 'modelName'
  Core.Text ->
  -- | 'modelVersion'
  Core.Text ->
  EdgeModel
newEdgeModel pModelName_ pModelVersion_ =
  EdgeModel'
    { latestInference = Core.Nothing,
      latestSampleTime = Core.Nothing,
      modelName = pModelName_,
      modelVersion = pModelVersion_
    }

-- | The timestamp of the last inference that was made.
edgeModel_latestInference :: Lens.Lens' EdgeModel (Core.Maybe Core.UTCTime)
edgeModel_latestInference = Lens.lens (\EdgeModel' {latestInference} -> latestInference) (\s@EdgeModel' {} a -> s {latestInference = a} :: EdgeModel) Core.. Lens.mapping Core._Time

-- | The timestamp of the last data sample taken.
edgeModel_latestSampleTime :: Lens.Lens' EdgeModel (Core.Maybe Core.UTCTime)
edgeModel_latestSampleTime = Lens.lens (\EdgeModel' {latestSampleTime} -> latestSampleTime) (\s@EdgeModel' {} a -> s {latestSampleTime = a} :: EdgeModel) Core.. Lens.mapping Core._Time

-- | The name of the model.
edgeModel_modelName :: Lens.Lens' EdgeModel Core.Text
edgeModel_modelName = Lens.lens (\EdgeModel' {modelName} -> modelName) (\s@EdgeModel' {} a -> s {modelName = a} :: EdgeModel)

-- | The model version.
edgeModel_modelVersion :: Lens.Lens' EdgeModel Core.Text
edgeModel_modelVersion = Lens.lens (\EdgeModel' {modelVersion} -> modelVersion) (\s@EdgeModel' {} a -> s {modelVersion = a} :: EdgeModel)

instance Core.FromJSON EdgeModel where
  parseJSON =
    Core.withObject
      "EdgeModel"
      ( \x ->
          EdgeModel'
            Core.<$> (x Core..:? "LatestInference")
            Core.<*> (x Core..:? "LatestSampleTime")
            Core.<*> (x Core..: "ModelName")
            Core.<*> (x Core..: "ModelVersion")
      )

instance Core.Hashable EdgeModel

instance Core.NFData EdgeModel
