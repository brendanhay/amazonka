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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.ExportS3DataInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.ExportS3DataInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newExportS3DataInput' smart constructor.
data ExportS3DataInput = ExportS3DataInput'
  { -- | The Amazon Key Management Service (KMS) key ID for server-side
    -- encryption.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The URL to the Amazon S3 data input.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportS3DataInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'exportS3DataInput_kmsKeyId' - The Amazon Key Management Service (KMS) key ID for server-side
-- encryption.
--
-- 's3Uri', 'exportS3DataInput_s3Uri' - The URL to the Amazon S3 data input.
newExportS3DataInput ::
  -- | 's3Uri'
  Prelude.Text ->
  ExportS3DataInput
newExportS3DataInput pS3Uri_ =
  ExportS3DataInput'
    { kmsKeyId = Prelude.Nothing,
      s3Uri = pS3Uri_
    }

-- | The Amazon Key Management Service (KMS) key ID for server-side
-- encryption.
exportS3DataInput_kmsKeyId :: Lens.Lens' ExportS3DataInput (Prelude.Maybe Prelude.Text)
exportS3DataInput_kmsKeyId = Lens.lens (\ExportS3DataInput' {kmsKeyId} -> kmsKeyId) (\s@ExportS3DataInput' {} a -> s {kmsKeyId = a} :: ExportS3DataInput)

-- | The URL to the Amazon S3 data input.
exportS3DataInput_s3Uri :: Lens.Lens' ExportS3DataInput Prelude.Text
exportS3DataInput_s3Uri = Lens.lens (\ExportS3DataInput' {s3Uri} -> s3Uri) (\s@ExportS3DataInput' {} a -> s {s3Uri = a} :: ExportS3DataInput)

instance Data.FromJSON ExportS3DataInput where
  parseJSON =
    Data.withObject
      "ExportS3DataInput"
      ( \x ->
          ExportS3DataInput'
            Prelude.<$> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..: "S3Uri")
      )

instance Prelude.Hashable ExportS3DataInput where
  hashWithSalt _salt ExportS3DataInput' {..} =
    _salt `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData ExportS3DataInput where
  rnf ExportS3DataInput' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf s3Uri

instance Data.ToJSON ExportS3DataInput where
  toJSON ExportS3DataInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            Prelude.Just ("S3Uri" Data..= s3Uri)
          ]
      )
