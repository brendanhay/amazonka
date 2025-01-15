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
-- Module      : Amazonka.Inspector2.Types.AwsLambdaFunctionDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AwsLambdaFunctionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.Architecture
import Amazonka.Inspector2.Types.LambdaVpcConfig
import Amazonka.Inspector2.Types.PackageType
import Amazonka.Inspector2.Types.Runtime
import qualified Amazonka.Prelude as Prelude

-- | A summary of information about the AWS Lambda function.
--
-- /See:/ 'newAwsLambdaFunctionDetails' smart constructor.
data AwsLambdaFunctionDetails = AwsLambdaFunctionDetails'
  { -- | The instruction set architecture that the AWS Lambda function supports.
    -- Architecture is a string array with one of the valid values. The default
    -- architecture value is @x86_64@.
    architectures :: Prelude.Maybe (Prelude.NonEmpty Architecture),
    -- | The date and time that a user last updated the configuration, in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>
    lastModifiedAt :: Prelude.Maybe Data.POSIX,
    -- | The AWS Lambda function\'s
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers>.
    -- A Lambda function can have up to five layers.
    layers :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The type of deployment package. Set to @Image@ for container image and
    -- set @Zip@ for .zip file archive.
    packageType :: Prelude.Maybe PackageType,
    -- | The AWS Lambda function\'s networking configuration.
    vpcConfig :: Prelude.Maybe LambdaVpcConfig,
    -- | The SHA256 hash of the AWS Lambda function\'s deployment package.
    codeSha256 :: Prelude.Text,
    -- | The AWS Lambda function\'s execution role.
    executionRoleArn :: Prelude.Text,
    -- | The name of the AWS Lambda function.
    functionName :: Prelude.Text,
    -- | The runtime environment for the AWS Lambda function.
    runtime :: Runtime,
    -- | The version of the AWS Lambda function.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsLambdaFunctionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'architectures', 'awsLambdaFunctionDetails_architectures' - The instruction set architecture that the AWS Lambda function supports.
-- Architecture is a string array with one of the valid values. The default
-- architecture value is @x86_64@.
--
-- 'lastModifiedAt', 'awsLambdaFunctionDetails_lastModifiedAt' - The date and time that a user last updated the configuration, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>
--
-- 'layers', 'awsLambdaFunctionDetails_layers' - The AWS Lambda function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers>.
-- A Lambda function can have up to five layers.
--
-- 'packageType', 'awsLambdaFunctionDetails_packageType' - The type of deployment package. Set to @Image@ for container image and
-- set @Zip@ for .zip file archive.
--
-- 'vpcConfig', 'awsLambdaFunctionDetails_vpcConfig' - The AWS Lambda function\'s networking configuration.
--
-- 'codeSha256', 'awsLambdaFunctionDetails_codeSha256' - The SHA256 hash of the AWS Lambda function\'s deployment package.
--
-- 'executionRoleArn', 'awsLambdaFunctionDetails_executionRoleArn' - The AWS Lambda function\'s execution role.
--
-- 'functionName', 'awsLambdaFunctionDetails_functionName' - The name of the AWS Lambda function.
--
-- 'runtime', 'awsLambdaFunctionDetails_runtime' - The runtime environment for the AWS Lambda function.
--
-- 'version', 'awsLambdaFunctionDetails_version' - The version of the AWS Lambda function.
newAwsLambdaFunctionDetails ::
  -- | 'codeSha256'
  Prelude.Text ->
  -- | 'executionRoleArn'
  Prelude.Text ->
  -- | 'functionName'
  Prelude.Text ->
  -- | 'runtime'
  Runtime ->
  -- | 'version'
  Prelude.Text ->
  AwsLambdaFunctionDetails
newAwsLambdaFunctionDetails
  pCodeSha256_
  pExecutionRoleArn_
  pFunctionName_
  pRuntime_
  pVersion_ =
    AwsLambdaFunctionDetails'
      { architectures =
          Prelude.Nothing,
        lastModifiedAt = Prelude.Nothing,
        layers = Prelude.Nothing,
        packageType = Prelude.Nothing,
        vpcConfig = Prelude.Nothing,
        codeSha256 = pCodeSha256_,
        executionRoleArn = pExecutionRoleArn_,
        functionName = pFunctionName_,
        runtime = pRuntime_,
        version = pVersion_
      }

-- | The instruction set architecture that the AWS Lambda function supports.
-- Architecture is a string array with one of the valid values. The default
-- architecture value is @x86_64@.
awsLambdaFunctionDetails_architectures :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe (Prelude.NonEmpty Architecture))
awsLambdaFunctionDetails_architectures = Lens.lens (\AwsLambdaFunctionDetails' {architectures} -> architectures) (\s@AwsLambdaFunctionDetails' {} a -> s {architectures = a} :: AwsLambdaFunctionDetails) Prelude.. Lens.mapping Lens.coerced

-- | The date and time that a user last updated the configuration, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>
awsLambdaFunctionDetails_lastModifiedAt :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.UTCTime)
awsLambdaFunctionDetails_lastModifiedAt = Lens.lens (\AwsLambdaFunctionDetails' {lastModifiedAt} -> lastModifiedAt) (\s@AwsLambdaFunctionDetails' {} a -> s {lastModifiedAt = a} :: AwsLambdaFunctionDetails) Prelude.. Lens.mapping Data._Time

-- | The AWS Lambda function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers>.
-- A Lambda function can have up to five layers.
awsLambdaFunctionDetails_layers :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
awsLambdaFunctionDetails_layers = Lens.lens (\AwsLambdaFunctionDetails' {layers} -> layers) (\s@AwsLambdaFunctionDetails' {} a -> s {layers = a} :: AwsLambdaFunctionDetails) Prelude.. Lens.mapping Lens.coerced

-- | The type of deployment package. Set to @Image@ for container image and
-- set @Zip@ for .zip file archive.
awsLambdaFunctionDetails_packageType :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe PackageType)
awsLambdaFunctionDetails_packageType = Lens.lens (\AwsLambdaFunctionDetails' {packageType} -> packageType) (\s@AwsLambdaFunctionDetails' {} a -> s {packageType = a} :: AwsLambdaFunctionDetails)

-- | The AWS Lambda function\'s networking configuration.
awsLambdaFunctionDetails_vpcConfig :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe LambdaVpcConfig)
awsLambdaFunctionDetails_vpcConfig = Lens.lens (\AwsLambdaFunctionDetails' {vpcConfig} -> vpcConfig) (\s@AwsLambdaFunctionDetails' {} a -> s {vpcConfig = a} :: AwsLambdaFunctionDetails)

-- | The SHA256 hash of the AWS Lambda function\'s deployment package.
awsLambdaFunctionDetails_codeSha256 :: Lens.Lens' AwsLambdaFunctionDetails Prelude.Text
awsLambdaFunctionDetails_codeSha256 = Lens.lens (\AwsLambdaFunctionDetails' {codeSha256} -> codeSha256) (\s@AwsLambdaFunctionDetails' {} a -> s {codeSha256 = a} :: AwsLambdaFunctionDetails)

-- | The AWS Lambda function\'s execution role.
awsLambdaFunctionDetails_executionRoleArn :: Lens.Lens' AwsLambdaFunctionDetails Prelude.Text
awsLambdaFunctionDetails_executionRoleArn = Lens.lens (\AwsLambdaFunctionDetails' {executionRoleArn} -> executionRoleArn) (\s@AwsLambdaFunctionDetails' {} a -> s {executionRoleArn = a} :: AwsLambdaFunctionDetails)

-- | The name of the AWS Lambda function.
awsLambdaFunctionDetails_functionName :: Lens.Lens' AwsLambdaFunctionDetails Prelude.Text
awsLambdaFunctionDetails_functionName = Lens.lens (\AwsLambdaFunctionDetails' {functionName} -> functionName) (\s@AwsLambdaFunctionDetails' {} a -> s {functionName = a} :: AwsLambdaFunctionDetails)

-- | The runtime environment for the AWS Lambda function.
awsLambdaFunctionDetails_runtime :: Lens.Lens' AwsLambdaFunctionDetails Runtime
awsLambdaFunctionDetails_runtime = Lens.lens (\AwsLambdaFunctionDetails' {runtime} -> runtime) (\s@AwsLambdaFunctionDetails' {} a -> s {runtime = a} :: AwsLambdaFunctionDetails)

-- | The version of the AWS Lambda function.
awsLambdaFunctionDetails_version :: Lens.Lens' AwsLambdaFunctionDetails Prelude.Text
awsLambdaFunctionDetails_version = Lens.lens (\AwsLambdaFunctionDetails' {version} -> version) (\s@AwsLambdaFunctionDetails' {} a -> s {version = a} :: AwsLambdaFunctionDetails)

instance Data.FromJSON AwsLambdaFunctionDetails where
  parseJSON =
    Data.withObject
      "AwsLambdaFunctionDetails"
      ( \x ->
          AwsLambdaFunctionDetails'
            Prelude.<$> (x Data..:? "architectures")
            Prelude.<*> (x Data..:? "lastModifiedAt")
            Prelude.<*> (x Data..:? "layers")
            Prelude.<*> (x Data..:? "packageType")
            Prelude.<*> (x Data..:? "vpcConfig")
            Prelude.<*> (x Data..: "codeSha256")
            Prelude.<*> (x Data..: "executionRoleArn")
            Prelude.<*> (x Data..: "functionName")
            Prelude.<*> (x Data..: "runtime")
            Prelude.<*> (x Data..: "version")
      )

instance Prelude.Hashable AwsLambdaFunctionDetails where
  hashWithSalt _salt AwsLambdaFunctionDetails' {..} =
    _salt
      `Prelude.hashWithSalt` architectures
      `Prelude.hashWithSalt` lastModifiedAt
      `Prelude.hashWithSalt` layers
      `Prelude.hashWithSalt` packageType
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` codeSha256
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` functionName
      `Prelude.hashWithSalt` runtime
      `Prelude.hashWithSalt` version

instance Prelude.NFData AwsLambdaFunctionDetails where
  rnf AwsLambdaFunctionDetails' {..} =
    Prelude.rnf architectures `Prelude.seq`
      Prelude.rnf lastModifiedAt `Prelude.seq`
        Prelude.rnf layers `Prelude.seq`
          Prelude.rnf packageType `Prelude.seq`
            Prelude.rnf vpcConfig `Prelude.seq`
              Prelude.rnf codeSha256 `Prelude.seq`
                Prelude.rnf executionRoleArn `Prelude.seq`
                  Prelude.rnf functionName `Prelude.seq`
                    Prelude.rnf runtime `Prelude.seq`
                      Prelude.rnf version
