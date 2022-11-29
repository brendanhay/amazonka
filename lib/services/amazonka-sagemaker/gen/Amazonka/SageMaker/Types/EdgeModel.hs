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
-- Module      : Amazonka.SageMaker.Types.EdgeModel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.EdgeModel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The model on the edge device.
--
-- /See:/ 'newEdgeModel' smart constructor.
data EdgeModel = EdgeModel'
  { -- | The timestamp of the last data sample taken.
    latestSampleTime :: Prelude.Maybe Core.POSIX,
    -- | The timestamp of the last inference that was made.
    latestInference :: Prelude.Maybe Core.POSIX,
    -- | The name of the model.
    modelName :: Prelude.Text,
    -- | The model version.
    modelVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EdgeModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latestSampleTime', 'edgeModel_latestSampleTime' - The timestamp of the last data sample taken.
--
-- 'latestInference', 'edgeModel_latestInference' - The timestamp of the last inference that was made.
--
-- 'modelName', 'edgeModel_modelName' - The name of the model.
--
-- 'modelVersion', 'edgeModel_modelVersion' - The model version.
newEdgeModel ::
  -- | 'modelName'
  Prelude.Text ->
  -- | 'modelVersion'
  Prelude.Text ->
  EdgeModel
newEdgeModel pModelName_ pModelVersion_ =
  EdgeModel'
    { latestSampleTime = Prelude.Nothing,
      latestInference = Prelude.Nothing,
      modelName = pModelName_,
      modelVersion = pModelVersion_
    }

-- | The timestamp of the last data sample taken.
edgeModel_latestSampleTime :: Lens.Lens' EdgeModel (Prelude.Maybe Prelude.UTCTime)
edgeModel_latestSampleTime = Lens.lens (\EdgeModel' {latestSampleTime} -> latestSampleTime) (\s@EdgeModel' {} a -> s {latestSampleTime = a} :: EdgeModel) Prelude.. Lens.mapping Core._Time

-- | The timestamp of the last inference that was made.
edgeModel_latestInference :: Lens.Lens' EdgeModel (Prelude.Maybe Prelude.UTCTime)
edgeModel_latestInference = Lens.lens (\EdgeModel' {latestInference} -> latestInference) (\s@EdgeModel' {} a -> s {latestInference = a} :: EdgeModel) Prelude.. Lens.mapping Core._Time

-- | The name of the model.
edgeModel_modelName :: Lens.Lens' EdgeModel Prelude.Text
edgeModel_modelName = Lens.lens (\EdgeModel' {modelName} -> modelName) (\s@EdgeModel' {} a -> s {modelName = a} :: EdgeModel)

-- | The model version.
edgeModel_modelVersion :: Lens.Lens' EdgeModel Prelude.Text
edgeModel_modelVersion = Lens.lens (\EdgeModel' {modelVersion} -> modelVersion) (\s@EdgeModel' {} a -> s {modelVersion = a} :: EdgeModel)

instance Core.FromJSON EdgeModel where
  parseJSON =
    Core.withObject
      "EdgeModel"
      ( \x ->
          EdgeModel'
            Prelude.<$> (x Core..:? "LatestSampleTime")
            Prelude.<*> (x Core..:? "LatestInference")
            Prelude.<*> (x Core..: "ModelName")
            Prelude.<*> (x Core..: "ModelVersion")
      )

instance Prelude.Hashable EdgeModel where
  hashWithSalt _salt EdgeModel' {..} =
    _salt `Prelude.hashWithSalt` latestSampleTime
      `Prelude.hashWithSalt` latestInference
      `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` modelVersion

instance Prelude.NFData EdgeModel where
  rnf EdgeModel' {..} =
    Prelude.rnf latestSampleTime
      `Prelude.seq` Prelude.rnf latestInference
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf modelVersion
