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
-- Module      : Amazonka.SageMaker.Types.ModelDashboardModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelDashboardModel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.Model
import Amazonka.SageMaker.Types.ModelDashboardEndpoint
import Amazonka.SageMaker.Types.ModelDashboardModelCard
import Amazonka.SageMaker.Types.ModelDashboardMonitoringSchedule
import Amazonka.SageMaker.Types.TransformJob

-- | A model displayed in the Amazon SageMaker Model Dashboard.
--
-- /See:/ 'newModelDashboardModel' smart constructor.
data ModelDashboardModel = ModelDashboardModel'
  { -- | The endpoints that host a model.
    endpoints :: Prelude.Maybe [ModelDashboardEndpoint],
    lastBatchTransformJob :: Prelude.Maybe TransformJob,
    -- | A model displayed in the Model Dashboard.
    model :: Prelude.Maybe Model,
    -- | The model card for a model.
    modelCard :: Prelude.Maybe ModelDashboardModelCard,
    -- | The monitoring schedules for a model.
    monitoringSchedules :: Prelude.Maybe [ModelDashboardMonitoringSchedule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelDashboardModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoints', 'modelDashboardModel_endpoints' - The endpoints that host a model.
--
-- 'lastBatchTransformJob', 'modelDashboardModel_lastBatchTransformJob' - Undocumented member.
--
-- 'model', 'modelDashboardModel_model' - A model displayed in the Model Dashboard.
--
-- 'modelCard', 'modelDashboardModel_modelCard' - The model card for a model.
--
-- 'monitoringSchedules', 'modelDashboardModel_monitoringSchedules' - The monitoring schedules for a model.
newModelDashboardModel ::
  ModelDashboardModel
newModelDashboardModel =
  ModelDashboardModel'
    { endpoints = Prelude.Nothing,
      lastBatchTransformJob = Prelude.Nothing,
      model = Prelude.Nothing,
      modelCard = Prelude.Nothing,
      monitoringSchedules = Prelude.Nothing
    }

-- | The endpoints that host a model.
modelDashboardModel_endpoints :: Lens.Lens' ModelDashboardModel (Prelude.Maybe [ModelDashboardEndpoint])
modelDashboardModel_endpoints = Lens.lens (\ModelDashboardModel' {endpoints} -> endpoints) (\s@ModelDashboardModel' {} a -> s {endpoints = a} :: ModelDashboardModel) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
modelDashboardModel_lastBatchTransformJob :: Lens.Lens' ModelDashboardModel (Prelude.Maybe TransformJob)
modelDashboardModel_lastBatchTransformJob = Lens.lens (\ModelDashboardModel' {lastBatchTransformJob} -> lastBatchTransformJob) (\s@ModelDashboardModel' {} a -> s {lastBatchTransformJob = a} :: ModelDashboardModel)

-- | A model displayed in the Model Dashboard.
modelDashboardModel_model :: Lens.Lens' ModelDashboardModel (Prelude.Maybe Model)
modelDashboardModel_model = Lens.lens (\ModelDashboardModel' {model} -> model) (\s@ModelDashboardModel' {} a -> s {model = a} :: ModelDashboardModel)

-- | The model card for a model.
modelDashboardModel_modelCard :: Lens.Lens' ModelDashboardModel (Prelude.Maybe ModelDashboardModelCard)
modelDashboardModel_modelCard = Lens.lens (\ModelDashboardModel' {modelCard} -> modelCard) (\s@ModelDashboardModel' {} a -> s {modelCard = a} :: ModelDashboardModel)

-- | The monitoring schedules for a model.
modelDashboardModel_monitoringSchedules :: Lens.Lens' ModelDashboardModel (Prelude.Maybe [ModelDashboardMonitoringSchedule])
modelDashboardModel_monitoringSchedules = Lens.lens (\ModelDashboardModel' {monitoringSchedules} -> monitoringSchedules) (\s@ModelDashboardModel' {} a -> s {monitoringSchedules = a} :: ModelDashboardModel) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ModelDashboardModel where
  parseJSON =
    Data.withObject
      "ModelDashboardModel"
      ( \x ->
          ModelDashboardModel'
            Prelude.<$> (x Data..:? "Endpoints" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LastBatchTransformJob")
            Prelude.<*> (x Data..:? "Model")
            Prelude.<*> (x Data..:? "ModelCard")
            Prelude.<*> ( x Data..:? "MonitoringSchedules"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ModelDashboardModel where
  hashWithSalt _salt ModelDashboardModel' {..} =
    _salt `Prelude.hashWithSalt` endpoints
      `Prelude.hashWithSalt` lastBatchTransformJob
      `Prelude.hashWithSalt` model
      `Prelude.hashWithSalt` modelCard
      `Prelude.hashWithSalt` monitoringSchedules

instance Prelude.NFData ModelDashboardModel where
  rnf ModelDashboardModel' {..} =
    Prelude.rnf endpoints
      `Prelude.seq` Prelude.rnf lastBatchTransformJob
      `Prelude.seq` Prelude.rnf model
      `Prelude.seq` Prelude.rnf modelCard
      `Prelude.seq` Prelude.rnf monitoringSchedules
