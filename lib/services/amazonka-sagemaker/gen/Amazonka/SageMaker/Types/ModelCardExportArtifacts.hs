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
-- Module      : Amazonka.SageMaker.Types.ModelCardExportArtifacts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelCardExportArtifacts where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The artifacts of the model card export job.
--
-- /See:/ 'newModelCardExportArtifacts' smart constructor.
data ModelCardExportArtifacts = ModelCardExportArtifacts'
  { -- | The Amazon S3 URI of the exported model artifacts.
    s3ExportArtifacts :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelCardExportArtifacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3ExportArtifacts', 'modelCardExportArtifacts_s3ExportArtifacts' - The Amazon S3 URI of the exported model artifacts.
newModelCardExportArtifacts ::
  -- | 's3ExportArtifacts'
  Prelude.Text ->
  ModelCardExportArtifacts
newModelCardExportArtifacts pS3ExportArtifacts_ =
  ModelCardExportArtifacts'
    { s3ExportArtifacts =
        pS3ExportArtifacts_
    }

-- | The Amazon S3 URI of the exported model artifacts.
modelCardExportArtifacts_s3ExportArtifacts :: Lens.Lens' ModelCardExportArtifacts Prelude.Text
modelCardExportArtifacts_s3ExportArtifacts = Lens.lens (\ModelCardExportArtifacts' {s3ExportArtifacts} -> s3ExportArtifacts) (\s@ModelCardExportArtifacts' {} a -> s {s3ExportArtifacts = a} :: ModelCardExportArtifacts)

instance Data.FromJSON ModelCardExportArtifacts where
  parseJSON =
    Data.withObject
      "ModelCardExportArtifacts"
      ( \x ->
          ModelCardExportArtifacts'
            Prelude.<$> (x Data..: "S3ExportArtifacts")
      )

instance Prelude.Hashable ModelCardExportArtifacts where
  hashWithSalt _salt ModelCardExportArtifacts' {..} =
    _salt `Prelude.hashWithSalt` s3ExportArtifacts

instance Prelude.NFData ModelCardExportArtifacts where
  rnf ModelCardExportArtifacts' {..} =
    Prelude.rnf s3ExportArtifacts
