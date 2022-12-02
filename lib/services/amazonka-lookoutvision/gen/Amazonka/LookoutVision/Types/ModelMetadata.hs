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
-- Module      : Amazonka.LookoutVision.Types.ModelMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.ModelMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types.ModelPerformance
import Amazonka.LookoutVision.Types.ModelStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon Lookout for Vision model.
--
-- /See:/ 'newModelMetadata' smart constructor.
data ModelMetadata = ModelMetadata'
  { -- | The version of the model.
    modelVersion :: Prelude.Maybe Prelude.Text,
    -- | The status of the model.
    status :: Prelude.Maybe ModelStatus,
    -- | The description for the model.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unix timestamp for the date and time that the model was created.
    creationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | Performance metrics for the model. Not available until training has
    -- successfully completed.
    performance :: Prelude.Maybe ModelPerformance,
    -- | The Amazon Resource Name (ARN) of the model.
    modelArn :: Prelude.Maybe Prelude.Text,
    -- | The status message for the model.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelVersion', 'modelMetadata_modelVersion' - The version of the model.
--
-- 'status', 'modelMetadata_status' - The status of the model.
--
-- 'description', 'modelMetadata_description' - The description for the model.
--
-- 'creationTimestamp', 'modelMetadata_creationTimestamp' - The unix timestamp for the date and time that the model was created.
--
-- 'performance', 'modelMetadata_performance' - Performance metrics for the model. Not available until training has
-- successfully completed.
--
-- 'modelArn', 'modelMetadata_modelArn' - The Amazon Resource Name (ARN) of the model.
--
-- 'statusMessage', 'modelMetadata_statusMessage' - The status message for the model.
newModelMetadata ::
  ModelMetadata
newModelMetadata =
  ModelMetadata'
    { modelVersion = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      performance = Prelude.Nothing,
      modelArn = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The version of the model.
modelMetadata_modelVersion :: Lens.Lens' ModelMetadata (Prelude.Maybe Prelude.Text)
modelMetadata_modelVersion = Lens.lens (\ModelMetadata' {modelVersion} -> modelVersion) (\s@ModelMetadata' {} a -> s {modelVersion = a} :: ModelMetadata)

-- | The status of the model.
modelMetadata_status :: Lens.Lens' ModelMetadata (Prelude.Maybe ModelStatus)
modelMetadata_status = Lens.lens (\ModelMetadata' {status} -> status) (\s@ModelMetadata' {} a -> s {status = a} :: ModelMetadata)

-- | The description for the model.
modelMetadata_description :: Lens.Lens' ModelMetadata (Prelude.Maybe Prelude.Text)
modelMetadata_description = Lens.lens (\ModelMetadata' {description} -> description) (\s@ModelMetadata' {} a -> s {description = a} :: ModelMetadata)

-- | The unix timestamp for the date and time that the model was created.
modelMetadata_creationTimestamp :: Lens.Lens' ModelMetadata (Prelude.Maybe Prelude.UTCTime)
modelMetadata_creationTimestamp = Lens.lens (\ModelMetadata' {creationTimestamp} -> creationTimestamp) (\s@ModelMetadata' {} a -> s {creationTimestamp = a} :: ModelMetadata) Prelude.. Lens.mapping Data._Time

-- | Performance metrics for the model. Not available until training has
-- successfully completed.
modelMetadata_performance :: Lens.Lens' ModelMetadata (Prelude.Maybe ModelPerformance)
modelMetadata_performance = Lens.lens (\ModelMetadata' {performance} -> performance) (\s@ModelMetadata' {} a -> s {performance = a} :: ModelMetadata)

-- | The Amazon Resource Name (ARN) of the model.
modelMetadata_modelArn :: Lens.Lens' ModelMetadata (Prelude.Maybe Prelude.Text)
modelMetadata_modelArn = Lens.lens (\ModelMetadata' {modelArn} -> modelArn) (\s@ModelMetadata' {} a -> s {modelArn = a} :: ModelMetadata)

-- | The status message for the model.
modelMetadata_statusMessage :: Lens.Lens' ModelMetadata (Prelude.Maybe Prelude.Text)
modelMetadata_statusMessage = Lens.lens (\ModelMetadata' {statusMessage} -> statusMessage) (\s@ModelMetadata' {} a -> s {statusMessage = a} :: ModelMetadata)

instance Data.FromJSON ModelMetadata where
  parseJSON =
    Data.withObject
      "ModelMetadata"
      ( \x ->
          ModelMetadata'
            Prelude.<$> (x Data..:? "ModelVersion")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "CreationTimestamp")
            Prelude.<*> (x Data..:? "Performance")
            Prelude.<*> (x Data..:? "ModelArn")
            Prelude.<*> (x Data..:? "StatusMessage")
      )

instance Prelude.Hashable ModelMetadata where
  hashWithSalt _salt ModelMetadata' {..} =
    _salt `Prelude.hashWithSalt` modelVersion
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` performance
      `Prelude.hashWithSalt` modelArn
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData ModelMetadata where
  rnf ModelMetadata' {..} =
    Prelude.rnf modelVersion
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf performance
      `Prelude.seq` Prelude.rnf modelArn
      `Prelude.seq` Prelude.rnf statusMessage
