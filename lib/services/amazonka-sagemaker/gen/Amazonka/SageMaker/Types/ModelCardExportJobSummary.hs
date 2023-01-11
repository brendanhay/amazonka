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
-- Module      : Amazonka.SageMaker.Types.ModelCardExportJobSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelCardExportJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ModelCardExportJobStatus

-- | The summary of the Amazon SageMaker Model Card export job.
--
-- /See:/ 'newModelCardExportJobSummary' smart constructor.
data ModelCardExportJobSummary = ModelCardExportJobSummary'
  { -- | The name of the model card export job.
    modelCardExportJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the model card export job.
    modelCardExportJobArn :: Prelude.Text,
    -- | The completion status of the model card export job.
    status :: ModelCardExportJobStatus,
    -- | The name of the model card that the export job exports.
    modelCardName :: Prelude.Text,
    -- | The version of the model card that the export job exports.
    modelCardVersion :: Prelude.Int,
    -- | The date and time that the model card export job was created.
    createdAt :: Data.POSIX,
    -- | The date and time that the model card export job was last modified..
    lastModifiedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelCardExportJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelCardExportJobName', 'modelCardExportJobSummary_modelCardExportJobName' - The name of the model card export job.
--
-- 'modelCardExportJobArn', 'modelCardExportJobSummary_modelCardExportJobArn' - The Amazon Resource Name (ARN) of the model card export job.
--
-- 'status', 'modelCardExportJobSummary_status' - The completion status of the model card export job.
--
-- 'modelCardName', 'modelCardExportJobSummary_modelCardName' - The name of the model card that the export job exports.
--
-- 'modelCardVersion', 'modelCardExportJobSummary_modelCardVersion' - The version of the model card that the export job exports.
--
-- 'createdAt', 'modelCardExportJobSummary_createdAt' - The date and time that the model card export job was created.
--
-- 'lastModifiedAt', 'modelCardExportJobSummary_lastModifiedAt' - The date and time that the model card export job was last modified..
newModelCardExportJobSummary ::
  -- | 'modelCardExportJobName'
  Prelude.Text ->
  -- | 'modelCardExportJobArn'
  Prelude.Text ->
  -- | 'status'
  ModelCardExportJobStatus ->
  -- | 'modelCardName'
  Prelude.Text ->
  -- | 'modelCardVersion'
  Prelude.Int ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastModifiedAt'
  Prelude.UTCTime ->
  ModelCardExportJobSummary
newModelCardExportJobSummary
  pModelCardExportJobName_
  pModelCardExportJobArn_
  pStatus_
  pModelCardName_
  pModelCardVersion_
  pCreatedAt_
  pLastModifiedAt_ =
    ModelCardExportJobSummary'
      { modelCardExportJobName =
          pModelCardExportJobName_,
        modelCardExportJobArn = pModelCardExportJobArn_,
        status = pStatus_,
        modelCardName = pModelCardName_,
        modelCardVersion = pModelCardVersion_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        lastModifiedAt =
          Data._Time Lens.# pLastModifiedAt_
      }

-- | The name of the model card export job.
modelCardExportJobSummary_modelCardExportJobName :: Lens.Lens' ModelCardExportJobSummary Prelude.Text
modelCardExportJobSummary_modelCardExportJobName = Lens.lens (\ModelCardExportJobSummary' {modelCardExportJobName} -> modelCardExportJobName) (\s@ModelCardExportJobSummary' {} a -> s {modelCardExportJobName = a} :: ModelCardExportJobSummary)

-- | The Amazon Resource Name (ARN) of the model card export job.
modelCardExportJobSummary_modelCardExportJobArn :: Lens.Lens' ModelCardExportJobSummary Prelude.Text
modelCardExportJobSummary_modelCardExportJobArn = Lens.lens (\ModelCardExportJobSummary' {modelCardExportJobArn} -> modelCardExportJobArn) (\s@ModelCardExportJobSummary' {} a -> s {modelCardExportJobArn = a} :: ModelCardExportJobSummary)

-- | The completion status of the model card export job.
modelCardExportJobSummary_status :: Lens.Lens' ModelCardExportJobSummary ModelCardExportJobStatus
modelCardExportJobSummary_status = Lens.lens (\ModelCardExportJobSummary' {status} -> status) (\s@ModelCardExportJobSummary' {} a -> s {status = a} :: ModelCardExportJobSummary)

-- | The name of the model card that the export job exports.
modelCardExportJobSummary_modelCardName :: Lens.Lens' ModelCardExportJobSummary Prelude.Text
modelCardExportJobSummary_modelCardName = Lens.lens (\ModelCardExportJobSummary' {modelCardName} -> modelCardName) (\s@ModelCardExportJobSummary' {} a -> s {modelCardName = a} :: ModelCardExportJobSummary)

-- | The version of the model card that the export job exports.
modelCardExportJobSummary_modelCardVersion :: Lens.Lens' ModelCardExportJobSummary Prelude.Int
modelCardExportJobSummary_modelCardVersion = Lens.lens (\ModelCardExportJobSummary' {modelCardVersion} -> modelCardVersion) (\s@ModelCardExportJobSummary' {} a -> s {modelCardVersion = a} :: ModelCardExportJobSummary)

-- | The date and time that the model card export job was created.
modelCardExportJobSummary_createdAt :: Lens.Lens' ModelCardExportJobSummary Prelude.UTCTime
modelCardExportJobSummary_createdAt = Lens.lens (\ModelCardExportJobSummary' {createdAt} -> createdAt) (\s@ModelCardExportJobSummary' {} a -> s {createdAt = a} :: ModelCardExportJobSummary) Prelude.. Data._Time

-- | The date and time that the model card export job was last modified..
modelCardExportJobSummary_lastModifiedAt :: Lens.Lens' ModelCardExportJobSummary Prelude.UTCTime
modelCardExportJobSummary_lastModifiedAt = Lens.lens (\ModelCardExportJobSummary' {lastModifiedAt} -> lastModifiedAt) (\s@ModelCardExportJobSummary' {} a -> s {lastModifiedAt = a} :: ModelCardExportJobSummary) Prelude.. Data._Time

instance Data.FromJSON ModelCardExportJobSummary where
  parseJSON =
    Data.withObject
      "ModelCardExportJobSummary"
      ( \x ->
          ModelCardExportJobSummary'
            Prelude.<$> (x Data..: "ModelCardExportJobName")
            Prelude.<*> (x Data..: "ModelCardExportJobArn")
            Prelude.<*> (x Data..: "Status")
            Prelude.<*> (x Data..: "ModelCardName")
            Prelude.<*> (x Data..: "ModelCardVersion")
            Prelude.<*> (x Data..: "CreatedAt")
            Prelude.<*> (x Data..: "LastModifiedAt")
      )

instance Prelude.Hashable ModelCardExportJobSummary where
  hashWithSalt _salt ModelCardExportJobSummary' {..} =
    _salt `Prelude.hashWithSalt` modelCardExportJobName
      `Prelude.hashWithSalt` modelCardExportJobArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` modelCardName
      `Prelude.hashWithSalt` modelCardVersion
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastModifiedAt

instance Prelude.NFData ModelCardExportJobSummary where
  rnf ModelCardExportJobSummary' {..} =
    Prelude.rnf modelCardExportJobName
      `Prelude.seq` Prelude.rnf modelCardExportJobArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf modelCardName
      `Prelude.seq` Prelude.rnf modelCardVersion
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastModifiedAt
