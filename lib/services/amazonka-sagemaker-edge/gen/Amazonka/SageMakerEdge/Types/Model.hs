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
-- Module      : Amazonka.SageMakerEdge.Types.Model
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerEdge.Types.Model where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerEdge.Types.EdgeMetric

-- | Information about a model deployed on an edge device that is registered
-- with SageMaker Edge Manager.
--
-- /See:/ 'newModel' smart constructor.
data Model = Model'
  { -- | The timestamp of the last data sample taken.
    latestSampleTime :: Prelude.Maybe Data.POSIX,
    -- | The version of the model.
    modelVersion :: Prelude.Maybe Prelude.Text,
    -- | Information required for model metrics.
    modelMetrics :: Prelude.Maybe [EdgeMetric],
    -- | The name of the model.
    modelName :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the last inference that was made.
    latestInference :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Model' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latestSampleTime', 'model_latestSampleTime' - The timestamp of the last data sample taken.
--
-- 'modelVersion', 'model_modelVersion' - The version of the model.
--
-- 'modelMetrics', 'model_modelMetrics' - Information required for model metrics.
--
-- 'modelName', 'model_modelName' - The name of the model.
--
-- 'latestInference', 'model_latestInference' - The timestamp of the last inference that was made.
newModel ::
  Model
newModel =
  Model'
    { latestSampleTime = Prelude.Nothing,
      modelVersion = Prelude.Nothing,
      modelMetrics = Prelude.Nothing,
      modelName = Prelude.Nothing,
      latestInference = Prelude.Nothing
    }

-- | The timestamp of the last data sample taken.
model_latestSampleTime :: Lens.Lens' Model (Prelude.Maybe Prelude.UTCTime)
model_latestSampleTime = Lens.lens (\Model' {latestSampleTime} -> latestSampleTime) (\s@Model' {} a -> s {latestSampleTime = a} :: Model) Prelude.. Lens.mapping Data._Time

-- | The version of the model.
model_modelVersion :: Lens.Lens' Model (Prelude.Maybe Prelude.Text)
model_modelVersion = Lens.lens (\Model' {modelVersion} -> modelVersion) (\s@Model' {} a -> s {modelVersion = a} :: Model)

-- | Information required for model metrics.
model_modelMetrics :: Lens.Lens' Model (Prelude.Maybe [EdgeMetric])
model_modelMetrics = Lens.lens (\Model' {modelMetrics} -> modelMetrics) (\s@Model' {} a -> s {modelMetrics = a} :: Model) Prelude.. Lens.mapping Lens.coerced

-- | The name of the model.
model_modelName :: Lens.Lens' Model (Prelude.Maybe Prelude.Text)
model_modelName = Lens.lens (\Model' {modelName} -> modelName) (\s@Model' {} a -> s {modelName = a} :: Model)

-- | The timestamp of the last inference that was made.
model_latestInference :: Lens.Lens' Model (Prelude.Maybe Prelude.UTCTime)
model_latestInference = Lens.lens (\Model' {latestInference} -> latestInference) (\s@Model' {} a -> s {latestInference = a} :: Model) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable Model where
  hashWithSalt _salt Model' {..} =
    _salt `Prelude.hashWithSalt` latestSampleTime
      `Prelude.hashWithSalt` modelVersion
      `Prelude.hashWithSalt` modelMetrics
      `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` latestInference

instance Prelude.NFData Model where
  rnf Model' {..} =
    Prelude.rnf latestSampleTime
      `Prelude.seq` Prelude.rnf modelVersion
      `Prelude.seq` Prelude.rnf modelMetrics
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf latestInference

instance Data.ToJSON Model where
  toJSON Model' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LatestSampleTime" Data..=)
              Prelude.<$> latestSampleTime,
            ("ModelVersion" Data..=) Prelude.<$> modelVersion,
            ("ModelMetrics" Data..=) Prelude.<$> modelMetrics,
            ("ModelName" Data..=) Prelude.<$> modelName,
            ("LatestInference" Data..=)
              Prelude.<$> latestInference
          ]
      )
