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
-- Module      : Amazonka.SageMaker.Types.ModelCardExportOutputConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelCardExportOutputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configure the export output details for an Amazon SageMaker Model Card.
--
-- /See:/ 'newModelCardExportOutputConfig' smart constructor.
data ModelCardExportOutputConfig = ModelCardExportOutputConfig'
  { -- | The Amazon S3 output path to export your model card PDF.
    s3OutputPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelCardExportOutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3OutputPath', 'modelCardExportOutputConfig_s3OutputPath' - The Amazon S3 output path to export your model card PDF.
newModelCardExportOutputConfig ::
  -- | 's3OutputPath'
  Prelude.Text ->
  ModelCardExportOutputConfig
newModelCardExportOutputConfig pS3OutputPath_ =
  ModelCardExportOutputConfig'
    { s3OutputPath =
        pS3OutputPath_
    }

-- | The Amazon S3 output path to export your model card PDF.
modelCardExportOutputConfig_s3OutputPath :: Lens.Lens' ModelCardExportOutputConfig Prelude.Text
modelCardExportOutputConfig_s3OutputPath = Lens.lens (\ModelCardExportOutputConfig' {s3OutputPath} -> s3OutputPath) (\s@ModelCardExportOutputConfig' {} a -> s {s3OutputPath = a} :: ModelCardExportOutputConfig)

instance Data.FromJSON ModelCardExportOutputConfig where
  parseJSON =
    Data.withObject
      "ModelCardExportOutputConfig"
      ( \x ->
          ModelCardExportOutputConfig'
            Prelude.<$> (x Data..: "S3OutputPath")
      )

instance Prelude.Hashable ModelCardExportOutputConfig where
  hashWithSalt _salt ModelCardExportOutputConfig' {..} =
    _salt `Prelude.hashWithSalt` s3OutputPath

instance Prelude.NFData ModelCardExportOutputConfig where
  rnf ModelCardExportOutputConfig' {..} =
    Prelude.rnf s3OutputPath

instance Data.ToJSON ModelCardExportOutputConfig where
  toJSON ModelCardExportOutputConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("S3OutputPath" Data..= s3OutputPath)]
      )
