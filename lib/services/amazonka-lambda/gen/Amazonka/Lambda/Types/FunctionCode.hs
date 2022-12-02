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
-- Module      : Amazonka.Lambda.Types.FunctionCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.FunctionCode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The code for the Lambda function. You can specify either an object in
-- Amazon S3, upload a .zip file archive deployment package directly, or
-- specify the URI of a container image.
--
-- /See:/ 'newFunctionCode' smart constructor.
data FunctionCode = FunctionCode'
  { -- | An Amazon S3 bucket in the same Amazon Web Services Region as your
    -- function. The bucket can be in a different Amazon Web Services account.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | URI of a
    -- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-images.html container image>
    -- in the Amazon ECR registry.
    imageUri :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 key of the deployment package.
    s3Key :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded contents of the deployment package. Amazon Web
    -- Services SDK and Amazon Web Services CLI clients handle the encoding for
    -- you.
    zipFile :: Prelude.Maybe (Data.Sensitive Data.Base64),
    -- | For versioned objects, the version of the deployment package object to
    -- use.
    s3ObjectVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunctionCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Bucket', 'functionCode_s3Bucket' - An Amazon S3 bucket in the same Amazon Web Services Region as your
-- function. The bucket can be in a different Amazon Web Services account.
--
-- 'imageUri', 'functionCode_imageUri' - URI of a
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-images.html container image>
-- in the Amazon ECR registry.
--
-- 's3Key', 'functionCode_s3Key' - The Amazon S3 key of the deployment package.
--
-- 'zipFile', 'functionCode_zipFile' - The base64-encoded contents of the deployment package. Amazon Web
-- Services SDK and Amazon Web Services CLI clients handle the encoding for
-- you.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 's3ObjectVersion', 'functionCode_s3ObjectVersion' - For versioned objects, the version of the deployment package object to
-- use.
newFunctionCode ::
  FunctionCode
newFunctionCode =
  FunctionCode'
    { s3Bucket = Prelude.Nothing,
      imageUri = Prelude.Nothing,
      s3Key = Prelude.Nothing,
      zipFile = Prelude.Nothing,
      s3ObjectVersion = Prelude.Nothing
    }

-- | An Amazon S3 bucket in the same Amazon Web Services Region as your
-- function. The bucket can be in a different Amazon Web Services account.
functionCode_s3Bucket :: Lens.Lens' FunctionCode (Prelude.Maybe Prelude.Text)
functionCode_s3Bucket = Lens.lens (\FunctionCode' {s3Bucket} -> s3Bucket) (\s@FunctionCode' {} a -> s {s3Bucket = a} :: FunctionCode)

-- | URI of a
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-images.html container image>
-- in the Amazon ECR registry.
functionCode_imageUri :: Lens.Lens' FunctionCode (Prelude.Maybe Prelude.Text)
functionCode_imageUri = Lens.lens (\FunctionCode' {imageUri} -> imageUri) (\s@FunctionCode' {} a -> s {imageUri = a} :: FunctionCode)

-- | The Amazon S3 key of the deployment package.
functionCode_s3Key :: Lens.Lens' FunctionCode (Prelude.Maybe Prelude.Text)
functionCode_s3Key = Lens.lens (\FunctionCode' {s3Key} -> s3Key) (\s@FunctionCode' {} a -> s {s3Key = a} :: FunctionCode)

-- | The base64-encoded contents of the deployment package. Amazon Web
-- Services SDK and Amazon Web Services CLI clients handle the encoding for
-- you.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
functionCode_zipFile :: Lens.Lens' FunctionCode (Prelude.Maybe Prelude.ByteString)
functionCode_zipFile = Lens.lens (\FunctionCode' {zipFile} -> zipFile) (\s@FunctionCode' {} a -> s {zipFile = a} :: FunctionCode) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Data._Base64)

-- | For versioned objects, the version of the deployment package object to
-- use.
functionCode_s3ObjectVersion :: Lens.Lens' FunctionCode (Prelude.Maybe Prelude.Text)
functionCode_s3ObjectVersion = Lens.lens (\FunctionCode' {s3ObjectVersion} -> s3ObjectVersion) (\s@FunctionCode' {} a -> s {s3ObjectVersion = a} :: FunctionCode)

instance Prelude.Hashable FunctionCode where
  hashWithSalt _salt FunctionCode' {..} =
    _salt `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` imageUri
      `Prelude.hashWithSalt` s3Key
      `Prelude.hashWithSalt` zipFile
      `Prelude.hashWithSalt` s3ObjectVersion

instance Prelude.NFData FunctionCode where
  rnf FunctionCode' {..} =
    Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf imageUri
      `Prelude.seq` Prelude.rnf s3Key
      `Prelude.seq` Prelude.rnf zipFile
      `Prelude.seq` Prelude.rnf s3ObjectVersion

instance Data.ToJSON FunctionCode where
  toJSON FunctionCode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3Bucket" Data..=) Prelude.<$> s3Bucket,
            ("ImageUri" Data..=) Prelude.<$> imageUri,
            ("S3Key" Data..=) Prelude.<$> s3Key,
            ("ZipFile" Data..=) Prelude.<$> zipFile,
            ("S3ObjectVersion" Data..=)
              Prelude.<$> s3ObjectVersion
          ]
      )
