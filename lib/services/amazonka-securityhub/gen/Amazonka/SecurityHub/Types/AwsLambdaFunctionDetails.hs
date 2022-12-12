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
  { -- | The instruction set architecture that the function uses. Valid values
    -- are @x86_64@ or @arm64@.
    architectures :: Prelude.Maybe [Prelude.Text],
    -- | An @AwsLambdaFunctionCode@ object.
    code :: Prelude.Maybe AwsLambdaFunctionCode,
    -- | The SHA256 hash of the function\'s deployment package.
    codeSha256 :: Prelude.Maybe Prelude.Text,
    -- | The function\'s dead letter queue.
    deadLetterConfig :: Prelude.Maybe AwsLambdaFunctionDeadLetterConfig,
    -- | The function\'s environment variables.
    environment :: Prelude.Maybe AwsLambdaFunctionEnvironment,
    -- | The name of the function.
    functionName :: Prelude.Maybe Prelude.Text,
    -- | The function that Lambda calls to begin executing your function.
    handler :: Prelude.Maybe Prelude.Text,
    -- | The KMS key that is used to encrypt the function\'s environment
    -- variables. This key is only returned if you\'ve configured a customer
    -- managed customer managed key.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the function was last updated.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    lastModified :: Prelude.Maybe Prelude.Text,
    -- | The function\'s layers.
    layers :: Prelude.Maybe [AwsLambdaFunctionLayer],
    -- | For Lambda\@Edge functions, the ARN of the master function.
    masterArn :: Prelude.Maybe Prelude.Text,
    -- | The memory that is allocated to the function.
    memorySize :: Prelude.Maybe Prelude.Int,
    -- | The type of deployment package that\'s used to deploy the function code
    -- to Lambda. Set to @Image@ for a container image and @Zip@ for a .zip
    -- file archive.
    packageType :: Prelude.Maybe Prelude.Text,
    -- | The latest updated revision of the function or alias.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The function\'s execution role.
    role' :: Prelude.Maybe Prelude.Text,
    -- | The runtime environment for the Lambda function.
    runtime :: Prelude.Maybe Prelude.Text,
    -- | The amount of time that Lambda allows a function to run before stopping
    -- it.
    timeout :: Prelude.Maybe Prelude.Int,
    -- | The function\'s X-Ray tracing configuration.
    tracingConfig :: Prelude.Maybe AwsLambdaFunctionTracingConfig,
    -- | The version of the Lambda function.
    version :: Prelude.Maybe Prelude.Text,
    -- | The function\'s networking configuration.
    vpcConfig :: Prelude.Maybe AwsLambdaFunctionVpcConfig
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
-- 'architectures', 'awsLambdaFunctionDetails_architectures' - The instruction set architecture that the function uses. Valid values
-- are @x86_64@ or @arm64@.
--
-- 'code', 'awsLambdaFunctionDetails_code' - An @AwsLambdaFunctionCode@ object.
--
-- 'codeSha256', 'awsLambdaFunctionDetails_codeSha256' - The SHA256 hash of the function\'s deployment package.
--
-- 'deadLetterConfig', 'awsLambdaFunctionDetails_deadLetterConfig' - The function\'s dead letter queue.
--
-- 'environment', 'awsLambdaFunctionDetails_environment' - The function\'s environment variables.
--
-- 'functionName', 'awsLambdaFunctionDetails_functionName' - The name of the function.
--
-- 'handler', 'awsLambdaFunctionDetails_handler' - The function that Lambda calls to begin executing your function.
--
-- 'kmsKeyArn', 'awsLambdaFunctionDetails_kmsKeyArn' - The KMS key that is used to encrypt the function\'s environment
-- variables. This key is only returned if you\'ve configured a customer
-- managed customer managed key.
--
-- 'lastModified', 'awsLambdaFunctionDetails_lastModified' - Indicates when the function was last updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'layers', 'awsLambdaFunctionDetails_layers' - The function\'s layers.
--
-- 'masterArn', 'awsLambdaFunctionDetails_masterArn' - For Lambda\@Edge functions, the ARN of the master function.
--
-- 'memorySize', 'awsLambdaFunctionDetails_memorySize' - The memory that is allocated to the function.
--
-- 'packageType', 'awsLambdaFunctionDetails_packageType' - The type of deployment package that\'s used to deploy the function code
-- to Lambda. Set to @Image@ for a container image and @Zip@ for a .zip
-- file archive.
--
-- 'revisionId', 'awsLambdaFunctionDetails_revisionId' - The latest updated revision of the function or alias.
--
-- 'role'', 'awsLambdaFunctionDetails_role' - The function\'s execution role.
--
-- 'runtime', 'awsLambdaFunctionDetails_runtime' - The runtime environment for the Lambda function.
--
-- 'timeout', 'awsLambdaFunctionDetails_timeout' - The amount of time that Lambda allows a function to run before stopping
-- it.
--
-- 'tracingConfig', 'awsLambdaFunctionDetails_tracingConfig' - The function\'s X-Ray tracing configuration.
--
-- 'version', 'awsLambdaFunctionDetails_version' - The version of the Lambda function.
--
-- 'vpcConfig', 'awsLambdaFunctionDetails_vpcConfig' - The function\'s networking configuration.
newAwsLambdaFunctionDetails ::
  AwsLambdaFunctionDetails
newAwsLambdaFunctionDetails =
  AwsLambdaFunctionDetails'
    { architectures =
        Prelude.Nothing,
      code = Prelude.Nothing,
      codeSha256 = Prelude.Nothing,
      deadLetterConfig = Prelude.Nothing,
      environment = Prelude.Nothing,
      functionName = Prelude.Nothing,
      handler = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      layers = Prelude.Nothing,
      masterArn = Prelude.Nothing,
      memorySize = Prelude.Nothing,
      packageType = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      role' = Prelude.Nothing,
      runtime = Prelude.Nothing,
      timeout = Prelude.Nothing,
      tracingConfig = Prelude.Nothing,
      version = Prelude.Nothing,
      vpcConfig = Prelude.Nothing
    }

-- | The instruction set architecture that the function uses. Valid values
-- are @x86_64@ or @arm64@.
awsLambdaFunctionDetails_architectures :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe [Prelude.Text])
awsLambdaFunctionDetails_architectures = Lens.lens (\AwsLambdaFunctionDetails' {architectures} -> architectures) (\s@AwsLambdaFunctionDetails' {} a -> s {architectures = a} :: AwsLambdaFunctionDetails) Prelude.. Lens.mapping Lens.coerced

-- | An @AwsLambdaFunctionCode@ object.
awsLambdaFunctionDetails_code :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe AwsLambdaFunctionCode)
awsLambdaFunctionDetails_code = Lens.lens (\AwsLambdaFunctionDetails' {code} -> code) (\s@AwsLambdaFunctionDetails' {} a -> s {code = a} :: AwsLambdaFunctionDetails)

-- | The SHA256 hash of the function\'s deployment package.
awsLambdaFunctionDetails_codeSha256 :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_codeSha256 = Lens.lens (\AwsLambdaFunctionDetails' {codeSha256} -> codeSha256) (\s@AwsLambdaFunctionDetails' {} a -> s {codeSha256 = a} :: AwsLambdaFunctionDetails)

-- | The function\'s dead letter queue.
awsLambdaFunctionDetails_deadLetterConfig :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe AwsLambdaFunctionDeadLetterConfig)
awsLambdaFunctionDetails_deadLetterConfig = Lens.lens (\AwsLambdaFunctionDetails' {deadLetterConfig} -> deadLetterConfig) (\s@AwsLambdaFunctionDetails' {} a -> s {deadLetterConfig = a} :: AwsLambdaFunctionDetails)

-- | The function\'s environment variables.
awsLambdaFunctionDetails_environment :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe AwsLambdaFunctionEnvironment)
awsLambdaFunctionDetails_environment = Lens.lens (\AwsLambdaFunctionDetails' {environment} -> environment) (\s@AwsLambdaFunctionDetails' {} a -> s {environment = a} :: AwsLambdaFunctionDetails)

-- | The name of the function.
awsLambdaFunctionDetails_functionName :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_functionName = Lens.lens (\AwsLambdaFunctionDetails' {functionName} -> functionName) (\s@AwsLambdaFunctionDetails' {} a -> s {functionName = a} :: AwsLambdaFunctionDetails)

-- | The function that Lambda calls to begin executing your function.
awsLambdaFunctionDetails_handler :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_handler = Lens.lens (\AwsLambdaFunctionDetails' {handler} -> handler) (\s@AwsLambdaFunctionDetails' {} a -> s {handler = a} :: AwsLambdaFunctionDetails)

-- | The KMS key that is used to encrypt the function\'s environment
-- variables. This key is only returned if you\'ve configured a customer
-- managed customer managed key.
awsLambdaFunctionDetails_kmsKeyArn :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_kmsKeyArn = Lens.lens (\AwsLambdaFunctionDetails' {kmsKeyArn} -> kmsKeyArn) (\s@AwsLambdaFunctionDetails' {} a -> s {kmsKeyArn = a} :: AwsLambdaFunctionDetails)

-- | Indicates when the function was last updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsLambdaFunctionDetails_lastModified :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_lastModified = Lens.lens (\AwsLambdaFunctionDetails' {lastModified} -> lastModified) (\s@AwsLambdaFunctionDetails' {} a -> s {lastModified = a} :: AwsLambdaFunctionDetails)

-- | The function\'s layers.
awsLambdaFunctionDetails_layers :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe [AwsLambdaFunctionLayer])
awsLambdaFunctionDetails_layers = Lens.lens (\AwsLambdaFunctionDetails' {layers} -> layers) (\s@AwsLambdaFunctionDetails' {} a -> s {layers = a} :: AwsLambdaFunctionDetails) Prelude.. Lens.mapping Lens.coerced

-- | For Lambda\@Edge functions, the ARN of the master function.
awsLambdaFunctionDetails_masterArn :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_masterArn = Lens.lens (\AwsLambdaFunctionDetails' {masterArn} -> masterArn) (\s@AwsLambdaFunctionDetails' {} a -> s {masterArn = a} :: AwsLambdaFunctionDetails)

-- | The memory that is allocated to the function.
awsLambdaFunctionDetails_memorySize :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Int)
awsLambdaFunctionDetails_memorySize = Lens.lens (\AwsLambdaFunctionDetails' {memorySize} -> memorySize) (\s@AwsLambdaFunctionDetails' {} a -> s {memorySize = a} :: AwsLambdaFunctionDetails)

-- | The type of deployment package that\'s used to deploy the function code
-- to Lambda. Set to @Image@ for a container image and @Zip@ for a .zip
-- file archive.
awsLambdaFunctionDetails_packageType :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_packageType = Lens.lens (\AwsLambdaFunctionDetails' {packageType} -> packageType) (\s@AwsLambdaFunctionDetails' {} a -> s {packageType = a} :: AwsLambdaFunctionDetails)

-- | The latest updated revision of the function or alias.
awsLambdaFunctionDetails_revisionId :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_revisionId = Lens.lens (\AwsLambdaFunctionDetails' {revisionId} -> revisionId) (\s@AwsLambdaFunctionDetails' {} a -> s {revisionId = a} :: AwsLambdaFunctionDetails)

-- | The function\'s execution role.
awsLambdaFunctionDetails_role :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_role = Lens.lens (\AwsLambdaFunctionDetails' {role'} -> role') (\s@AwsLambdaFunctionDetails' {} a -> s {role' = a} :: AwsLambdaFunctionDetails)

-- | The runtime environment for the Lambda function.
awsLambdaFunctionDetails_runtime :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_runtime = Lens.lens (\AwsLambdaFunctionDetails' {runtime} -> runtime) (\s@AwsLambdaFunctionDetails' {} a -> s {runtime = a} :: AwsLambdaFunctionDetails)

-- | The amount of time that Lambda allows a function to run before stopping
-- it.
awsLambdaFunctionDetails_timeout :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Int)
awsLambdaFunctionDetails_timeout = Lens.lens (\AwsLambdaFunctionDetails' {timeout} -> timeout) (\s@AwsLambdaFunctionDetails' {} a -> s {timeout = a} :: AwsLambdaFunctionDetails)

-- | The function\'s X-Ray tracing configuration.
awsLambdaFunctionDetails_tracingConfig :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe AwsLambdaFunctionTracingConfig)
awsLambdaFunctionDetails_tracingConfig = Lens.lens (\AwsLambdaFunctionDetails' {tracingConfig} -> tracingConfig) (\s@AwsLambdaFunctionDetails' {} a -> s {tracingConfig = a} :: AwsLambdaFunctionDetails)

-- | The version of the Lambda function.
awsLambdaFunctionDetails_version :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_version = Lens.lens (\AwsLambdaFunctionDetails' {version} -> version) (\s@AwsLambdaFunctionDetails' {} a -> s {version = a} :: AwsLambdaFunctionDetails)

-- | The function\'s networking configuration.
awsLambdaFunctionDetails_vpcConfig :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe AwsLambdaFunctionVpcConfig)
awsLambdaFunctionDetails_vpcConfig = Lens.lens (\AwsLambdaFunctionDetails' {vpcConfig} -> vpcConfig) (\s@AwsLambdaFunctionDetails' {} a -> s {vpcConfig = a} :: AwsLambdaFunctionDetails)

instance Data.FromJSON AwsLambdaFunctionDetails where
  parseJSON =
    Data.withObject
      "AwsLambdaFunctionDetails"
      ( \x ->
          AwsLambdaFunctionDetails'
            Prelude.<$> (x Data..:? "Architectures" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "CodeSha256")
            Prelude.<*> (x Data..:? "DeadLetterConfig")
            Prelude.<*> (x Data..:? "Environment")
            Prelude.<*> (x Data..:? "FunctionName")
            Prelude.<*> (x Data..:? "Handler")
            Prelude.<*> (x Data..:? "KmsKeyArn")
            Prelude.<*> (x Data..:? "LastModified")
            Prelude.<*> (x Data..:? "Layers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "MasterArn")
            Prelude.<*> (x Data..:? "MemorySize")
            Prelude.<*> (x Data..:? "PackageType")
            Prelude.<*> (x Data..:? "RevisionId")
            Prelude.<*> (x Data..:? "Role")
            Prelude.<*> (x Data..:? "Runtime")
            Prelude.<*> (x Data..:? "Timeout")
            Prelude.<*> (x Data..:? "TracingConfig")
            Prelude.<*> (x Data..:? "Version")
            Prelude.<*> (x Data..:? "VpcConfig")
      )

instance Prelude.Hashable AwsLambdaFunctionDetails where
  hashWithSalt _salt AwsLambdaFunctionDetails' {..} =
    _salt `Prelude.hashWithSalt` architectures
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` codeSha256
      `Prelude.hashWithSalt` deadLetterConfig
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` functionName
      `Prelude.hashWithSalt` handler
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` lastModified
      `Prelude.hashWithSalt` layers
      `Prelude.hashWithSalt` masterArn
      `Prelude.hashWithSalt` memorySize
      `Prelude.hashWithSalt` packageType
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` runtime
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` tracingConfig
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` vpcConfig

instance Prelude.NFData AwsLambdaFunctionDetails where
  rnf AwsLambdaFunctionDetails' {..} =
    Prelude.rnf architectures
      `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf codeSha256
      `Prelude.seq` Prelude.rnf deadLetterConfig
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf functionName
      `Prelude.seq` Prelude.rnf handler
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf lastModified
      `Prelude.seq` Prelude.rnf layers
      `Prelude.seq` Prelude.rnf masterArn
      `Prelude.seq` Prelude.rnf memorySize
      `Prelude.seq` Prelude.rnf packageType
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf runtime
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf tracingConfig
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf vpcConfig

instance Data.ToJSON AwsLambdaFunctionDetails where
  toJSON AwsLambdaFunctionDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Architectures" Data..=) Prelude.<$> architectures,
            ("Code" Data..=) Prelude.<$> code,
            ("CodeSha256" Data..=) Prelude.<$> codeSha256,
            ("DeadLetterConfig" Data..=)
              Prelude.<$> deadLetterConfig,
            ("Environment" Data..=) Prelude.<$> environment,
            ("FunctionName" Data..=) Prelude.<$> functionName,
            ("Handler" Data..=) Prelude.<$> handler,
            ("KmsKeyArn" Data..=) Prelude.<$> kmsKeyArn,
            ("LastModified" Data..=) Prelude.<$> lastModified,
            ("Layers" Data..=) Prelude.<$> layers,
            ("MasterArn" Data..=) Prelude.<$> masterArn,
            ("MemorySize" Data..=) Prelude.<$> memorySize,
            ("PackageType" Data..=) Prelude.<$> packageType,
            ("RevisionId" Data..=) Prelude.<$> revisionId,
            ("Role" Data..=) Prelude.<$> role',
            ("Runtime" Data..=) Prelude.<$> runtime,
            ("Timeout" Data..=) Prelude.<$> timeout,
            ("TracingConfig" Data..=) Prelude.<$> tracingConfig,
            ("Version" Data..=) Prelude.<$> version,
            ("VpcConfig" Data..=) Prelude.<$> vpcConfig
          ]
      )
