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
-- Module      : Amazonka.Lambda.Types.LayerVersionContentInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.LayerVersionContentInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A ZIP archive that contains the contents of an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html Lambda layer>.
-- You can specify either an Amazon S3 location, or upload a layer archive
-- directly.
--
-- /See:/ 'newLayerVersionContentInput' smart constructor.
data LayerVersionContentInput = LayerVersionContentInput'
  { -- | The Amazon S3 bucket of the layer archive.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 key of the layer archive.
    s3Key :: Prelude.Maybe Prelude.Text,
    -- | For versioned objects, the version of the layer archive object to use.
    s3ObjectVersion :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded contents of the layer archive. Amazon Web Services
    -- SDK and Amazon Web Services CLI clients handle the encoding for you.
    zipFile :: Prelude.Maybe (Data.Sensitive Data.Base64)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LayerVersionContentInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Bucket', 'layerVersionContentInput_s3Bucket' - The Amazon S3 bucket of the layer archive.
--
-- 's3Key', 'layerVersionContentInput_s3Key' - The Amazon S3 key of the layer archive.
--
-- 's3ObjectVersion', 'layerVersionContentInput_s3ObjectVersion' - For versioned objects, the version of the layer archive object to use.
--
-- 'zipFile', 'layerVersionContentInput_zipFile' - The base64-encoded contents of the layer archive. Amazon Web Services
-- SDK and Amazon Web Services CLI clients handle the encoding for you.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newLayerVersionContentInput ::
  LayerVersionContentInput
newLayerVersionContentInput =
  LayerVersionContentInput'
    { s3Bucket =
        Prelude.Nothing,
      s3Key = Prelude.Nothing,
      s3ObjectVersion = Prelude.Nothing,
      zipFile = Prelude.Nothing
    }

-- | The Amazon S3 bucket of the layer archive.
layerVersionContentInput_s3Bucket :: Lens.Lens' LayerVersionContentInput (Prelude.Maybe Prelude.Text)
layerVersionContentInput_s3Bucket = Lens.lens (\LayerVersionContentInput' {s3Bucket} -> s3Bucket) (\s@LayerVersionContentInput' {} a -> s {s3Bucket = a} :: LayerVersionContentInput)

-- | The Amazon S3 key of the layer archive.
layerVersionContentInput_s3Key :: Lens.Lens' LayerVersionContentInput (Prelude.Maybe Prelude.Text)
layerVersionContentInput_s3Key = Lens.lens (\LayerVersionContentInput' {s3Key} -> s3Key) (\s@LayerVersionContentInput' {} a -> s {s3Key = a} :: LayerVersionContentInput)

-- | For versioned objects, the version of the layer archive object to use.
layerVersionContentInput_s3ObjectVersion :: Lens.Lens' LayerVersionContentInput (Prelude.Maybe Prelude.Text)
layerVersionContentInput_s3ObjectVersion = Lens.lens (\LayerVersionContentInput' {s3ObjectVersion} -> s3ObjectVersion) (\s@LayerVersionContentInput' {} a -> s {s3ObjectVersion = a} :: LayerVersionContentInput)

-- | The base64-encoded contents of the layer archive. Amazon Web Services
-- SDK and Amazon Web Services CLI clients handle the encoding for you.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
layerVersionContentInput_zipFile :: Lens.Lens' LayerVersionContentInput (Prelude.Maybe Prelude.ByteString)
layerVersionContentInput_zipFile = Lens.lens (\LayerVersionContentInput' {zipFile} -> zipFile) (\s@LayerVersionContentInput' {} a -> s {zipFile = a} :: LayerVersionContentInput) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Data._Base64)

instance Prelude.Hashable LayerVersionContentInput where
  hashWithSalt _salt LayerVersionContentInput' {..} =
    _salt `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3Key
      `Prelude.hashWithSalt` s3ObjectVersion
      `Prelude.hashWithSalt` zipFile

instance Prelude.NFData LayerVersionContentInput where
  rnf LayerVersionContentInput' {..} =
    Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3Key
      `Prelude.seq` Prelude.rnf s3ObjectVersion
      `Prelude.seq` Prelude.rnf zipFile

instance Data.ToJSON LayerVersionContentInput where
  toJSON LayerVersionContentInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3Bucket" Data..=) Prelude.<$> s3Bucket,
            ("S3Key" Data..=) Prelude.<$> s3Key,
            ("S3ObjectVersion" Data..=)
              Prelude.<$> s3ObjectVersion,
            ("ZipFile" Data..=) Prelude.<$> zipFile
          ]
      )
