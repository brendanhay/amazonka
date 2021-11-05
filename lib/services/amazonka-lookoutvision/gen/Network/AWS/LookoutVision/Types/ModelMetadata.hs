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
-- Module      : Network.AWS.LookoutVision.Types.ModelMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LookoutVision.Types.ModelMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LookoutVision.Types.ModelPerformance
import Network.AWS.LookoutVision.Types.ModelStatus
import qualified Network.AWS.Prelude as Prelude

-- | Describes an Amazon Lookout for Vision model.
--
-- /See:/ 'newModelMetadata' smart constructor.
data ModelMetadata = ModelMetadata'
  { -- | The status of the model.
    status :: Prelude.Maybe ModelStatus,
    -- | The Amazon Resource Name (ARN) of the model.
    modelArn :: Prelude.Maybe Prelude.Text,
    -- | Performance metrics for the model. Not available until training has
    -- successfully completed.
    performance :: Prelude.Maybe ModelPerformance,
    -- | The status message for the model.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The unix timestamp for the date and time that the model was created.
    creationTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The version of the model.
    modelVersion :: Prelude.Maybe Prelude.Text,
    -- | The description for the model.
    description :: Prelude.Maybe Prelude.Text
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
-- 'status', 'modelMetadata_status' - The status of the model.
--
-- 'modelArn', 'modelMetadata_modelArn' - The Amazon Resource Name (ARN) of the model.
--
-- 'performance', 'modelMetadata_performance' - Performance metrics for the model. Not available until training has
-- successfully completed.
--
-- 'statusMessage', 'modelMetadata_statusMessage' - The status message for the model.
--
-- 'creationTimestamp', 'modelMetadata_creationTimestamp' - The unix timestamp for the date and time that the model was created.
--
-- 'modelVersion', 'modelMetadata_modelVersion' - The version of the model.
--
-- 'description', 'modelMetadata_description' - The description for the model.
newModelMetadata ::
  ModelMetadata
newModelMetadata =
  ModelMetadata'
    { status = Prelude.Nothing,
      modelArn = Prelude.Nothing,
      performance = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      modelVersion = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The status of the model.
modelMetadata_status :: Lens.Lens' ModelMetadata (Prelude.Maybe ModelStatus)
modelMetadata_status = Lens.lens (\ModelMetadata' {status} -> status) (\s@ModelMetadata' {} a -> s {status = a} :: ModelMetadata)

-- | The Amazon Resource Name (ARN) of the model.
modelMetadata_modelArn :: Lens.Lens' ModelMetadata (Prelude.Maybe Prelude.Text)
modelMetadata_modelArn = Lens.lens (\ModelMetadata' {modelArn} -> modelArn) (\s@ModelMetadata' {} a -> s {modelArn = a} :: ModelMetadata)

-- | Performance metrics for the model. Not available until training has
-- successfully completed.
modelMetadata_performance :: Lens.Lens' ModelMetadata (Prelude.Maybe ModelPerformance)
modelMetadata_performance = Lens.lens (\ModelMetadata' {performance} -> performance) (\s@ModelMetadata' {} a -> s {performance = a} :: ModelMetadata)

-- | The status message for the model.
modelMetadata_statusMessage :: Lens.Lens' ModelMetadata (Prelude.Maybe Prelude.Text)
modelMetadata_statusMessage = Lens.lens (\ModelMetadata' {statusMessage} -> statusMessage) (\s@ModelMetadata' {} a -> s {statusMessage = a} :: ModelMetadata)

-- | The unix timestamp for the date and time that the model was created.
modelMetadata_creationTimestamp :: Lens.Lens' ModelMetadata (Prelude.Maybe Prelude.UTCTime)
modelMetadata_creationTimestamp = Lens.lens (\ModelMetadata' {creationTimestamp} -> creationTimestamp) (\s@ModelMetadata' {} a -> s {creationTimestamp = a} :: ModelMetadata) Prelude.. Lens.mapping Core._Time

-- | The version of the model.
modelMetadata_modelVersion :: Lens.Lens' ModelMetadata (Prelude.Maybe Prelude.Text)
modelMetadata_modelVersion = Lens.lens (\ModelMetadata' {modelVersion} -> modelVersion) (\s@ModelMetadata' {} a -> s {modelVersion = a} :: ModelMetadata)

-- | The description for the model.
modelMetadata_description :: Lens.Lens' ModelMetadata (Prelude.Maybe Prelude.Text)
modelMetadata_description = Lens.lens (\ModelMetadata' {description} -> description) (\s@ModelMetadata' {} a -> s {description = a} :: ModelMetadata)

instance Core.FromJSON ModelMetadata where
  parseJSON =
    Core.withObject
      "ModelMetadata"
      ( \x ->
          ModelMetadata'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ModelArn")
            Prelude.<*> (x Core..:? "Performance")
            Prelude.<*> (x Core..:? "StatusMessage")
            Prelude.<*> (x Core..:? "CreationTimestamp")
            Prelude.<*> (x Core..:? "ModelVersion")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable ModelMetadata

instance Prelude.NFData ModelMetadata
