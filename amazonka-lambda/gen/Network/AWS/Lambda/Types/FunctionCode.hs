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
-- Module      : Network.AWS.Lambda.Types.FunctionCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.FunctionCode where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The code for the Lambda function. You can specify either an object in
-- Amazon S3, upload a .zip file archive deployment package directly, or
-- specify the URI of a container image.
--
-- /See:/ 'newFunctionCode' smart constructor.
data FunctionCode = FunctionCode'
  { -- | URI of a container image in the Amazon ECR registry.
    imageUri :: Core.Maybe Core.Text,
    -- | An Amazon S3 bucket in the same AWS Region as your function. The bucket
    -- can be in a different AWS account.
    s3Bucket :: Core.Maybe Core.Text,
    -- | The base64-encoded contents of the deployment package. AWS SDK and AWS
    -- CLI clients handle the encoding for you.
    zipFile :: Core.Maybe (Core.Sensitive Core.Base64),
    -- | For versioned objects, the version of the deployment package object to
    -- use.
    s3ObjectVersion :: Core.Maybe Core.Text,
    -- | The Amazon S3 key of the deployment package.
    s3Key :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'FunctionCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageUri', 'functionCode_imageUri' - URI of a container image in the Amazon ECR registry.
--
-- 's3Bucket', 'functionCode_s3Bucket' - An Amazon S3 bucket in the same AWS Region as your function. The bucket
-- can be in a different AWS account.
--
-- 'zipFile', 'functionCode_zipFile' - The base64-encoded contents of the deployment package. AWS SDK and AWS
-- CLI clients handle the encoding for you.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 's3ObjectVersion', 'functionCode_s3ObjectVersion' - For versioned objects, the version of the deployment package object to
-- use.
--
-- 's3Key', 'functionCode_s3Key' - The Amazon S3 key of the deployment package.
newFunctionCode ::
  FunctionCode
newFunctionCode =
  FunctionCode'
    { imageUri = Core.Nothing,
      s3Bucket = Core.Nothing,
      zipFile = Core.Nothing,
      s3ObjectVersion = Core.Nothing,
      s3Key = Core.Nothing
    }

-- | URI of a container image in the Amazon ECR registry.
functionCode_imageUri :: Lens.Lens' FunctionCode (Core.Maybe Core.Text)
functionCode_imageUri = Lens.lens (\FunctionCode' {imageUri} -> imageUri) (\s@FunctionCode' {} a -> s {imageUri = a} :: FunctionCode)

-- | An Amazon S3 bucket in the same AWS Region as your function. The bucket
-- can be in a different AWS account.
functionCode_s3Bucket :: Lens.Lens' FunctionCode (Core.Maybe Core.Text)
functionCode_s3Bucket = Lens.lens (\FunctionCode' {s3Bucket} -> s3Bucket) (\s@FunctionCode' {} a -> s {s3Bucket = a} :: FunctionCode)

-- | The base64-encoded contents of the deployment package. AWS SDK and AWS
-- CLI clients handle the encoding for you.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
functionCode_zipFile :: Lens.Lens' FunctionCode (Core.Maybe Core.ByteString)
functionCode_zipFile = Lens.lens (\FunctionCode' {zipFile} -> zipFile) (\s@FunctionCode' {} a -> s {zipFile = a} :: FunctionCode) Core.. Lens.mapping (Core._Sensitive Core.. Core._Base64)

-- | For versioned objects, the version of the deployment package object to
-- use.
functionCode_s3ObjectVersion :: Lens.Lens' FunctionCode (Core.Maybe Core.Text)
functionCode_s3ObjectVersion = Lens.lens (\FunctionCode' {s3ObjectVersion} -> s3ObjectVersion) (\s@FunctionCode' {} a -> s {s3ObjectVersion = a} :: FunctionCode)

-- | The Amazon S3 key of the deployment package.
functionCode_s3Key :: Lens.Lens' FunctionCode (Core.Maybe Core.Text)
functionCode_s3Key = Lens.lens (\FunctionCode' {s3Key} -> s3Key) (\s@FunctionCode' {} a -> s {s3Key = a} :: FunctionCode)

instance Core.Hashable FunctionCode

instance Core.NFData FunctionCode

instance Core.ToJSON FunctionCode where
  toJSON FunctionCode' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ImageUri" Core..=) Core.<$> imageUri,
            ("S3Bucket" Core..=) Core.<$> s3Bucket,
            ("ZipFile" Core..=) Core.<$> zipFile,
            ("S3ObjectVersion" Core..=) Core.<$> s3ObjectVersion,
            ("S3Key" Core..=) Core.<$> s3Key
          ]
      )
