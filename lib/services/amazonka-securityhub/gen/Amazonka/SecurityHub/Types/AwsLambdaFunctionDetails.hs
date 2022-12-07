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
-- Module      : Amazonka.SecurityHub.Types.AwsLambdaFunctionDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsLambdaFunctionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsLambdaFunctionCode
import Amazonka.SecurityHub.Types.AwsLambdaFunctionDeadLetterConfig
import Amazonka.SecurityHub.Types.AwsLambdaFunctionEnvironment
import Amazonka.SecurityHub.Types.AwsLambdaFunctionLayer
import Amazonka.SecurityHub.Types.AwsLambdaFunctionTracingConfig
import Amazonka.SecurityHub.Types.AwsLambdaFunctionVpcConfig

-- | Details about an Lambda function\'s configuration.
--
-- /See:/ 'newAwsLambdaFunctionDetails' smart constructor.
data AwsLambdaFunctionDetails = AwsLambdaFunctionDetails'
  { -- | The function\'s X-Ray tracing configuration.
    tracingConfig :: Prelude.Maybe AwsLambdaFunctionTracingConfig,
    -- | For Lambda\@Edge functions, the ARN of the master function.
    masterArn :: Prelude.Maybe Prelude.Text,
    -- | The amount of time that Lambda allows a function to run before stopping
    -- it.
    timeout :: Prelude.Maybe Prelude.Int,
    -- | The memory that is allocated to the function.
    memorySize :: Prelude.Maybe Prelude.Int,
    -- | The SHA256 hash of the function\'s deployment package.
    codeSha256 :: Prelude.Maybe Prelude.Text,
    -- | The function\'s environment variables.
    environment :: Prelude.Maybe AwsLambdaFunctionEnvironment,
    -- | An @AwsLambdaFunctionCode@ object.
    code :: Prelude.Maybe AwsLambdaFunctionCode,
    -- | The function\'s networking configuration.
    vpcConfig :: Prelude.Maybe AwsLambdaFunctionVpcConfig,
    -- | The name of the function.
    functionName :: Prelude.Maybe Prelude.Text,
    -- | The runtime environment for the Lambda function.
    runtime :: Prelude.Maybe Prelude.Text,
    -- | The KMS key that is used to encrypt the function\'s environment
    -- variables. This key is only returned if you\'ve configured a customer
    -- managed customer managed key.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The function that Lambda calls to begin executing your function.
    handler :: Prelude.Maybe Prelude.Text,
    -- | The function\'s layers.
    layers :: Prelude.Maybe [AwsLambdaFunctionLayer],
    -- | The type of deployment package that\'s used to deploy the function code
    -- to Lambda. Set to @Image@ for a container image and @Zip@ for a .zip
    -- file archive.
    packageType :: Prelude.Maybe Prelude.Text,
    -- | The latest updated revision of the function or alias.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the function was last updated.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    lastModified :: Prelude.Maybe Prelude.Text,
    -- | The function\'s execution role.
    role' :: Prelude.Maybe Prelude.Text,
    -- | The instruction set architecture that the function uses. Valid values
    -- are @x86_64@ or @arm64@.
    architectures :: Prelude.Maybe [Prelude.Text],
    -- | The version of the Lambda function.
    version :: Prelude.Maybe Prelude.Text,
    -- | The function\'s dead letter queue.
    deadLetterConfig :: Prelude.Maybe AwsLambdaFunctionDeadLetterConfig
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
-- 'tracingConfig', 'awsLambdaFunctionDetails_tracingConfig' - The function\'s X-Ray tracing configuration.
--
-- 'masterArn', 'awsLambdaFunctionDetails_masterArn' - For Lambda\@Edge functions, the ARN of the master function.
--
-- 'timeout', 'awsLambdaFunctionDetails_timeout' - The amount of time that Lambda allows a function to run before stopping
-- it.
--
-- 'memorySize', 'awsLambdaFunctionDetails_memorySize' - The memory that is allocated to the function.
--
-- 'codeSha256', 'awsLambdaFunctionDetails_codeSha256' - The SHA256 hash of the function\'s deployment package.
--
-- 'environment', 'awsLambdaFunctionDetails_environment' - The function\'s environment variables.
--
-- 'code', 'awsLambdaFunctionDetails_code' - An @AwsLambdaFunctionCode@ object.
--
-- 'vpcConfig', 'awsLambdaFunctionDetails_vpcConfig' - The function\'s networking configuration.
--
-- 'functionName', 'awsLambdaFunctionDetails_functionName' - The name of the function.
--
-- 'runtime', 'awsLambdaFunctionDetails_runtime' - The runtime environment for the Lambda function.
--
-- 'kmsKeyArn', 'awsLambdaFunctionDetails_kmsKeyArn' - The KMS key that is used to encrypt the function\'s environment
-- variables. This key is only returned if you\'ve configured a customer
-- managed customer managed key.
--
-- 'handler', 'awsLambdaFunctionDetails_handler' - The function that Lambda calls to begin executing your function.
--
-- 'layers', 'awsLambdaFunctionDetails_layers' - The function\'s layers.
--
-- 'packageType', 'awsLambdaFunctionDetails_packageType' - The type of deployment package that\'s used to deploy the function code
-- to Lambda. Set to @Image@ for a container image and @Zip@ for a .zip
-- file archive.
--
-- 'revisionId', 'awsLambdaFunctionDetails_revisionId' - The latest updated revision of the function or alias.
--
-- 'lastModified', 'awsLambdaFunctionDetails_lastModified' - Indicates when the function was last updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'role'', 'awsLambdaFunctionDetails_role' - The function\'s execution role.
--
-- 'architectures', 'awsLambdaFunctionDetails_architectures' - The instruction set architecture that the function uses. Valid values
-- are @x86_64@ or @arm64@.
--
-- 'version', 'awsLambdaFunctionDetails_version' - The version of the Lambda function.
--
-- 'deadLetterConfig', 'awsLambdaFunctionDetails_deadLetterConfig' - The function\'s dead letter queue.
newAwsLambdaFunctionDetails ::
  AwsLambdaFunctionDetails
newAwsLambdaFunctionDetails =
  AwsLambdaFunctionDetails'
    { tracingConfig =
        Prelude.Nothing,
      masterArn = Prelude.Nothing,
      timeout = Prelude.Nothing,
      memorySize = Prelude.Nothing,
      codeSha256 = Prelude.Nothing,
      environment = Prelude.Nothing,
      code = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      functionName = Prelude.Nothing,
      runtime = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      handler = Prelude.Nothing,
      layers = Prelude.Nothing,
      packageType = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      role' = Prelude.Nothing,
      architectures = Prelude.Nothing,
      version = Prelude.Nothing,
      deadLetterConfig = Prelude.Nothing
    }

-- | The function\'s X-Ray tracing configuration.
awsLambdaFunctionDetails_tracingConfig :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe AwsLambdaFunctionTracingConfig)
awsLambdaFunctionDetails_tracingConfig = Lens.lens (\AwsLambdaFunctionDetails' {tracingConfig} -> tracingConfig) (\s@AwsLambdaFunctionDetails' {} a -> s {tracingConfig = a} :: AwsLambdaFunctionDetails)

-- | For Lambda\@Edge functions, the ARN of the master function.
awsLambdaFunctionDetails_masterArn :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_masterArn = Lens.lens (\AwsLambdaFunctionDetails' {masterArn} -> masterArn) (\s@AwsLambdaFunctionDetails' {} a -> s {masterArn = a} :: AwsLambdaFunctionDetails)

-- | The amount of time that Lambda allows a function to run before stopping
-- it.
awsLambdaFunctionDetails_timeout :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Int)
awsLambdaFunctionDetails_timeout = Lens.lens (\AwsLambdaFunctionDetails' {timeout} -> timeout) (\s@AwsLambdaFunctionDetails' {} a -> s {timeout = a} :: AwsLambdaFunctionDetails)

-- | The memory that is allocated to the function.
awsLambdaFunctionDetails_memorySize :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Int)
awsLambdaFunctionDetails_memorySize = Lens.lens (\AwsLambdaFunctionDetails' {memorySize} -> memorySize) (\s@AwsLambdaFunctionDetails' {} a -> s {memorySize = a} :: AwsLambdaFunctionDetails)

-- | The SHA256 hash of the function\'s deployment package.
awsLambdaFunctionDetails_codeSha256 :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_codeSha256 = Lens.lens (\AwsLambdaFunctionDetails' {codeSha256} -> codeSha256) (\s@AwsLambdaFunctionDetails' {} a -> s {codeSha256 = a} :: AwsLambdaFunctionDetails)

-- | The function\'s environment variables.
awsLambdaFunctionDetails_environment :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe AwsLambdaFunctionEnvironment)
awsLambdaFunctionDetails_environment = Lens.lens (\AwsLambdaFunctionDetails' {environment} -> environment) (\s@AwsLambdaFunctionDetails' {} a -> s {environment = a} :: AwsLambdaFunctionDetails)

-- | An @AwsLambdaFunctionCode@ object.
awsLambdaFunctionDetails_code :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe AwsLambdaFunctionCode)
awsLambdaFunctionDetails_code = Lens.lens (\AwsLambdaFunctionDetails' {code} -> code) (\s@AwsLambdaFunctionDetails' {} a -> s {code = a} :: AwsLambdaFunctionDetails)

-- | The function\'s networking configuration.
awsLambdaFunctionDetails_vpcConfig :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe AwsLambdaFunctionVpcConfig)
awsLambdaFunctionDetails_vpcConfig = Lens.lens (\AwsLambdaFunctionDetails' {vpcConfig} -> vpcConfig) (\s@AwsLambdaFunctionDetails' {} a -> s {vpcConfig = a} :: AwsLambdaFunctionDetails)

-- | The name of the function.
awsLambdaFunctionDetails_functionName :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_functionName = Lens.lens (\AwsLambdaFunctionDetails' {functionName} -> functionName) (\s@AwsLambdaFunctionDetails' {} a -> s {functionName = a} :: AwsLambdaFunctionDetails)

-- | The runtime environment for the Lambda function.
awsLambdaFunctionDetails_runtime :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_runtime = Lens.lens (\AwsLambdaFunctionDetails' {runtime} -> runtime) (\s@AwsLambdaFunctionDetails' {} a -> s {runtime = a} :: AwsLambdaFunctionDetails)

-- | The KMS key that is used to encrypt the function\'s environment
-- variables. This key is only returned if you\'ve configured a customer
-- managed customer managed key.
awsLambdaFunctionDetails_kmsKeyArn :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_kmsKeyArn = Lens.lens (\AwsLambdaFunctionDetails' {kmsKeyArn} -> kmsKeyArn) (\s@AwsLambdaFunctionDetails' {} a -> s {kmsKeyArn = a} :: AwsLambdaFunctionDetails)

-- | The function that Lambda calls to begin executing your function.
awsLambdaFunctionDetails_handler :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_handler = Lens.lens (\AwsLambdaFunctionDetails' {handler} -> handler) (\s@AwsLambdaFunctionDetails' {} a -> s {handler = a} :: AwsLambdaFunctionDetails)

-- | The function\'s layers.
awsLambdaFunctionDetails_layers :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe [AwsLambdaFunctionLayer])
awsLambdaFunctionDetails_layers = Lens.lens (\AwsLambdaFunctionDetails' {layers} -> layers) (\s@AwsLambdaFunctionDetails' {} a -> s {layers = a} :: AwsLambdaFunctionDetails) Prelude.. Lens.mapping Lens.coerced

-- | The type of deployment package that\'s used to deploy the function code
-- to Lambda. Set to @Image@ for a container image and @Zip@ for a .zip
-- file archive.
awsLambdaFunctionDetails_packageType :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_packageType = Lens.lens (\AwsLambdaFunctionDetails' {packageType} -> packageType) (\s@AwsLambdaFunctionDetails' {} a -> s {packageType = a} :: AwsLambdaFunctionDetails)

-- | The latest updated revision of the function or alias.
awsLambdaFunctionDetails_revisionId :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_revisionId = Lens.lens (\AwsLambdaFunctionDetails' {revisionId} -> revisionId) (\s@AwsLambdaFunctionDetails' {} a -> s {revisionId = a} :: AwsLambdaFunctionDetails)

-- | Indicates when the function was last updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsLambdaFunctionDetails_lastModified :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_lastModified = Lens.lens (\AwsLambdaFunctionDetails' {lastModified} -> lastModified) (\s@AwsLambdaFunctionDetails' {} a -> s {lastModified = a} :: AwsLambdaFunctionDetails)

-- | The function\'s execution role.
awsLambdaFunctionDetails_role :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_role = Lens.lens (\AwsLambdaFunctionDetails' {role'} -> role') (\s@AwsLambdaFunctionDetails' {} a -> s {role' = a} :: AwsLambdaFunctionDetails)

-- | The instruction set architecture that the function uses. Valid values
-- are @x86_64@ or @arm64@.
awsLambdaFunctionDetails_architectures :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe [Prelude.Text])
awsLambdaFunctionDetails_architectures = Lens.lens (\AwsLambdaFunctionDetails' {architectures} -> architectures) (\s@AwsLambdaFunctionDetails' {} a -> s {architectures = a} :: AwsLambdaFunctionDetails) Prelude.. Lens.mapping Lens.coerced

-- | The version of the Lambda function.
awsLambdaFunctionDetails_version :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_version = Lens.lens (\AwsLambdaFunctionDetails' {version} -> version) (\s@AwsLambdaFunctionDetails' {} a -> s {version = a} :: AwsLambdaFunctionDetails)

-- | The function\'s dead letter queue.
awsLambdaFunctionDetails_deadLetterConfig :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe AwsLambdaFunctionDeadLetterConfig)
awsLambdaFunctionDetails_deadLetterConfig = Lens.lens (\AwsLambdaFunctionDetails' {deadLetterConfig} -> deadLetterConfig) (\s@AwsLambdaFunctionDetails' {} a -> s {deadLetterConfig = a} :: AwsLambdaFunctionDetails)

instance Data.FromJSON AwsLambdaFunctionDetails where
  parseJSON =
    Data.withObject
      "AwsLambdaFunctionDetails"
      ( \x ->
          AwsLambdaFunctionDetails'
            Prelude.<$> (x Data..:? "TracingConfig")
            Prelude.<*> (x Data..:? "MasterArn")
            Prelude.<*> (x Data..:? "Timeout")
            Prelude.<*> (x Data..:? "MemorySize")
            Prelude.<*> (x Data..:? "CodeSha256")
            Prelude.<*> (x Data..:? "Environment")
            Prelude.<*> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "VpcConfig")
            Prelude.<*> (x Data..:? "FunctionName")
            Prelude.<*> (x Data..:? "Runtime")
            Prelude.<*> (x Data..:? "KmsKeyArn")
            Prelude.<*> (x Data..:? "Handler")
            Prelude.<*> (x Data..:? "Layers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PackageType")
            Prelude.<*> (x Data..:? "RevisionId")
            Prelude.<*> (x Data..:? "LastModified")
            Prelude.<*> (x Data..:? "Role")
            Prelude.<*> (x Data..:? "Architectures" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Version")
            Prelude.<*> (x Data..:? "DeadLetterConfig")
      )

instance Prelude.Hashable AwsLambdaFunctionDetails where
  hashWithSalt _salt AwsLambdaFunctionDetails' {..} =
    _salt `Prelude.hashWithSalt` tracingConfig
      `Prelude.hashWithSalt` masterArn
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` memorySize
      `Prelude.hashWithSalt` codeSha256
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` functionName
      `Prelude.hashWithSalt` runtime
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` handler
      `Prelude.hashWithSalt` layers
      `Prelude.hashWithSalt` packageType
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` lastModified
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` architectures
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` deadLetterConfig

instance Prelude.NFData AwsLambdaFunctionDetails where
  rnf AwsLambdaFunctionDetails' {..} =
    Prelude.rnf tracingConfig
      `Prelude.seq` Prelude.rnf masterArn
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf memorySize
      `Prelude.seq` Prelude.rnf codeSha256
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf functionName
      `Prelude.seq` Prelude.rnf runtime
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf handler
      `Prelude.seq` Prelude.rnf layers
      `Prelude.seq` Prelude.rnf packageType
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf lastModified
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf architectures
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf deadLetterConfig

instance Data.ToJSON AwsLambdaFunctionDetails where
  toJSON AwsLambdaFunctionDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TracingConfig" Data..=) Prelude.<$> tracingConfig,
            ("MasterArn" Data..=) Prelude.<$> masterArn,
            ("Timeout" Data..=) Prelude.<$> timeout,
            ("MemorySize" Data..=) Prelude.<$> memorySize,
            ("CodeSha256" Data..=) Prelude.<$> codeSha256,
            ("Environment" Data..=) Prelude.<$> environment,
            ("Code" Data..=) Prelude.<$> code,
            ("VpcConfig" Data..=) Prelude.<$> vpcConfig,
            ("FunctionName" Data..=) Prelude.<$> functionName,
            ("Runtime" Data..=) Prelude.<$> runtime,
            ("KmsKeyArn" Data..=) Prelude.<$> kmsKeyArn,
            ("Handler" Data..=) Prelude.<$> handler,
            ("Layers" Data..=) Prelude.<$> layers,
            ("PackageType" Data..=) Prelude.<$> packageType,
            ("RevisionId" Data..=) Prelude.<$> revisionId,
            ("LastModified" Data..=) Prelude.<$> lastModified,
            ("Role" Data..=) Prelude.<$> role',
            ("Architectures" Data..=) Prelude.<$> architectures,
            ("Version" Data..=) Prelude.<$> version,
            ("DeadLetterConfig" Data..=)
              Prelude.<$> deadLetterConfig
          ]
      )
