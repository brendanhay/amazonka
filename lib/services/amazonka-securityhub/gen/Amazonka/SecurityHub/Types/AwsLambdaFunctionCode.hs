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
-- Module      : Amazonka.SecurityHub.Types.AwsLambdaFunctionCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsLambdaFunctionCode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The code for the Lambda function. You can specify either an object in
-- Amazon S3, or upload a deployment package directly.
--
-- /See:/ 'newAwsLambdaFunctionCode' smart constructor.
data AwsLambdaFunctionCode = AwsLambdaFunctionCode'
  { -- | An Amazon S3 bucket in the same Amazon Web Services Region as your
    -- function. The bucket can be in a different Amazon Web Services account.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 key of the deployment package.
    s3Key :: Prelude.Maybe Prelude.Text,
    -- | For versioned objects, the version of the deployment package object to
    -- use.
    s3ObjectVersion :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded contents of the deployment package. Amazon Web
    -- Services SDK and Amazon Web Services CLI clients handle the encoding for
    -- you.
    zipFile :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsLambdaFunctionCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Bucket', 'awsLambdaFunctionCode_s3Bucket' - An Amazon S3 bucket in the same Amazon Web Services Region as your
-- function. The bucket can be in a different Amazon Web Services account.
--
-- 's3Key', 'awsLambdaFunctionCode_s3Key' - The Amazon S3 key of the deployment package.
--
-- 's3ObjectVersion', 'awsLambdaFunctionCode_s3ObjectVersion' - For versioned objects, the version of the deployment package object to
-- use.
--
-- 'zipFile', 'awsLambdaFunctionCode_zipFile' - The base64-encoded contents of the deployment package. Amazon Web
-- Services SDK and Amazon Web Services CLI clients handle the encoding for
-- you.
newAwsLambdaFunctionCode ::
  AwsLambdaFunctionCode
newAwsLambdaFunctionCode =
  AwsLambdaFunctionCode'
    { s3Bucket = Prelude.Nothing,
      s3Key = Prelude.Nothing,
      s3ObjectVersion = Prelude.Nothing,
      zipFile = Prelude.Nothing
    }

-- | An Amazon S3 bucket in the same Amazon Web Services Region as your
-- function. The bucket can be in a different Amazon Web Services account.
awsLambdaFunctionCode_s3Bucket :: Lens.Lens' AwsLambdaFunctionCode (Prelude.Maybe Prelude.Text)
awsLambdaFunctionCode_s3Bucket = Lens.lens (\AwsLambdaFunctionCode' {s3Bucket} -> s3Bucket) (\s@AwsLambdaFunctionCode' {} a -> s {s3Bucket = a} :: AwsLambdaFunctionCode)

-- | The Amazon S3 key of the deployment package.
awsLambdaFunctionCode_s3Key :: Lens.Lens' AwsLambdaFunctionCode (Prelude.Maybe Prelude.Text)
awsLambdaFunctionCode_s3Key = Lens.lens (\AwsLambdaFunctionCode' {s3Key} -> s3Key) (\s@AwsLambdaFunctionCode' {} a -> s {s3Key = a} :: AwsLambdaFunctionCode)

-- | For versioned objects, the version of the deployment package object to
-- use.
awsLambdaFunctionCode_s3ObjectVersion :: Lens.Lens' AwsLambdaFunctionCode (Prelude.Maybe Prelude.Text)
awsLambdaFunctionCode_s3ObjectVersion = Lens.lens (\AwsLambdaFunctionCode' {s3ObjectVersion} -> s3ObjectVersion) (\s@AwsLambdaFunctionCode' {} a -> s {s3ObjectVersion = a} :: AwsLambdaFunctionCode)

-- | The base64-encoded contents of the deployment package. Amazon Web
-- Services SDK and Amazon Web Services CLI clients handle the encoding for
-- you.
awsLambdaFunctionCode_zipFile :: Lens.Lens' AwsLambdaFunctionCode (Prelude.Maybe Prelude.Text)
awsLambdaFunctionCode_zipFile = Lens.lens (\AwsLambdaFunctionCode' {zipFile} -> zipFile) (\s@AwsLambdaFunctionCode' {} a -> s {zipFile = a} :: AwsLambdaFunctionCode)

instance Data.FromJSON AwsLambdaFunctionCode where
  parseJSON =
    Data.withObject
      "AwsLambdaFunctionCode"
      ( \x ->
          AwsLambdaFunctionCode'
            Prelude.<$> (x Data..:? "S3Bucket")
            Prelude.<*> (x Data..:? "S3Key")
            Prelude.<*> (x Data..:? "S3ObjectVersion")
            Prelude.<*> (x Data..:? "ZipFile")
      )

instance Prelude.Hashable AwsLambdaFunctionCode where
  hashWithSalt _salt AwsLambdaFunctionCode' {..} =
    _salt
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3Key
      `Prelude.hashWithSalt` s3ObjectVersion
      `Prelude.hashWithSalt` zipFile

instance Prelude.NFData AwsLambdaFunctionCode where
  rnf AwsLambdaFunctionCode' {..} =
    Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3Key
      `Prelude.seq` Prelude.rnf s3ObjectVersion
      `Prelude.seq` Prelude.rnf zipFile

instance Data.ToJSON AwsLambdaFunctionCode where
  toJSON AwsLambdaFunctionCode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3Bucket" Data..=) Prelude.<$> s3Bucket,
            ("S3Key" Data..=) Prelude.<$> s3Key,
            ("S3ObjectVersion" Data..=)
              Prelude.<$> s3ObjectVersion,
            ("ZipFile" Data..=) Prelude.<$> zipFile
          ]
      )
