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
-- Module      : Amazonka.LookoutEquipment.Types.ModelSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.ModelSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types.ModelStatus
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the specified ML model, including dataset and
-- model names and ARNs, as well as status.
--
-- /See:/ 'newModelSummary' smart constructor.
data ModelSummary = ModelSummary'
  { -- | The time at which the specific model was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the dataset used to create the model.
    datasetArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the dataset being used for the ML model.
    datasetName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the ML model.
    modelArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the ML model.
    modelName :: Prelude.Maybe Prelude.Text,
    -- | Indicates the status of the ML model.
    status :: Prelude.Maybe ModelStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'modelSummary_createdAt' - The time at which the specific model was created.
--
-- 'datasetArn', 'modelSummary_datasetArn' - The Amazon Resource Name (ARN) of the dataset used to create the model.
--
-- 'datasetName', 'modelSummary_datasetName' - The name of the dataset being used for the ML model.
--
-- 'modelArn', 'modelSummary_modelArn' - The Amazon Resource Name (ARN) of the ML model.
--
-- 'modelName', 'modelSummary_modelName' - The name of the ML model.
--
-- 'status', 'modelSummary_status' - Indicates the status of the ML model.
newModelSummary ::
  ModelSummary
newModelSummary =
  ModelSummary'
    { createdAt = Prelude.Nothing,
      datasetArn = Prelude.Nothing,
      datasetName = Prelude.Nothing,
      modelArn = Prelude.Nothing,
      modelName = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The time at which the specific model was created.
modelSummary_createdAt :: Lens.Lens' ModelSummary (Prelude.Maybe Prelude.UTCTime)
modelSummary_createdAt = Lens.lens (\ModelSummary' {createdAt} -> createdAt) (\s@ModelSummary' {} a -> s {createdAt = a} :: ModelSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the dataset used to create the model.
modelSummary_datasetArn :: Lens.Lens' ModelSummary (Prelude.Maybe Prelude.Text)
modelSummary_datasetArn = Lens.lens (\ModelSummary' {datasetArn} -> datasetArn) (\s@ModelSummary' {} a -> s {datasetArn = a} :: ModelSummary)

-- | The name of the dataset being used for the ML model.
modelSummary_datasetName :: Lens.Lens' ModelSummary (Prelude.Maybe Prelude.Text)
modelSummary_datasetName = Lens.lens (\ModelSummary' {datasetName} -> datasetName) (\s@ModelSummary' {} a -> s {datasetName = a} :: ModelSummary)

-- | The Amazon Resource Name (ARN) of the ML model.
modelSummary_modelArn :: Lens.Lens' ModelSummary (Prelude.Maybe Prelude.Text)
modelSummary_modelArn = Lens.lens (\ModelSummary' {modelArn} -> modelArn) (\s@ModelSummary' {} a -> s {modelArn = a} :: ModelSummary)

-- | The name of the ML model.
modelSummary_modelName :: Lens.Lens' ModelSummary (Prelude.Maybe Prelude.Text)
modelSummary_modelName = Lens.lens (\ModelSummary' {modelName} -> modelName) (\s@ModelSummary' {} a -> s {modelName = a} :: ModelSummary)

-- | Indicates the status of the ML model.
modelSummary_status :: Lens.Lens' ModelSummary (Prelude.Maybe ModelStatus)
modelSummary_status = Lens.lens (\ModelSummary' {status} -> status) (\s@ModelSummary' {} a -> s {status = a} :: ModelSummary)

instance Data.FromJSON ModelSummary where
  parseJSON =
    Data.withObject
      "ModelSummary"
      ( \x ->
          ModelSummary'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "DatasetArn")
            Prelude.<*> (x Data..:? "DatasetName")
            Prelude.<*> (x Data..:? "ModelArn")
            Prelude.<*> (x Data..:? "ModelName")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable ModelSummary where
  hashWithSalt _salt ModelSummary' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` datasetArn
      `Prelude.hashWithSalt` datasetName
      `Prelude.hashWithSalt` modelArn
      `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` status

instance Prelude.NFData ModelSummary where
  rnf ModelSummary' {..} =
    Prelude.rnf createdAt `Prelude.seq`
      Prelude.rnf datasetArn `Prelude.seq`
        Prelude.rnf datasetName `Prelude.seq`
          Prelude.rnf modelArn `Prelude.seq`
            Prelude.rnf modelName `Prelude.seq`
              Prelude.rnf status
