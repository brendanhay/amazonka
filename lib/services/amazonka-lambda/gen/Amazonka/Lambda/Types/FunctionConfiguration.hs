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
-- Module      : Amazonka.Lambda.Types.FunctionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.FunctionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types.Architecture
import Amazonka.Lambda.Types.DeadLetterConfig
import Amazonka.Lambda.Types.EnvironmentResponse
import Amazonka.Lambda.Types.EphemeralStorage
import Amazonka.Lambda.Types.FileSystemConfig
import Amazonka.Lambda.Types.ImageConfigResponse
import Amazonka.Lambda.Types.LastUpdateStatus
import Amazonka.Lambda.Types.LastUpdateStatusReasonCode
import Amazonka.Lambda.Types.Layer
import Amazonka.Lambda.Types.PackageType
import Amazonka.Lambda.Types.Runtime
import Amazonka.Lambda.Types.SnapStartResponse
import Amazonka.Lambda.Types.State
import Amazonka.Lambda.Types.StateReasonCode
import Amazonka.Lambda.Types.TracingConfigResponse
import Amazonka.Lambda.Types.VpcConfigResponse
import qualified Amazonka.Prelude as Prelude

-- | Details about a function\'s configuration.
--
-- /See:/ 'newFunctionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
  { -- | The instruction set architecture that the function supports.
    -- Architecture is a string array with one of the valid values. The default
    -- architecture value is @x86_64@.
    architectures :: Prelude.Maybe (Prelude.NonEmpty Architecture),
    -- | The SHA256 hash of the function\'s deployment package.
    codeSha256 :: Prelude.Maybe Prelude.Text,
    -- | The size of the function\'s deployment package, in bytes.
    codeSize :: Prelude.Maybe Prelude.Integer,
    -- | The function\'s dead letter queue.
    deadLetterConfig :: Prelude.Maybe DeadLetterConfig,
    -- | The function\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The function\'s
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-envvars.html environment variables>.
    -- Omitted from CloudTrail logs.
    environment :: Prelude.Maybe EnvironmentResponse,
    -- | The size of the function’s @\/tmp@ directory in MB. The default value is
    -- 512, but it can be any whole number between 512 and 10,240 MB.
    ephemeralStorage :: Prelude.Maybe EphemeralStorage,
    -- | Connection settings for an
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-filesystem.html Amazon EFS file system>.
    fileSystemConfigs :: Prelude.Maybe [FileSystemConfig],
    -- | The function\'s Amazon Resource Name (ARN).
    functionArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the function.
    functionName :: Prelude.Maybe Prelude.Text,
    -- | The function that Lambda calls to begin running your function.
    handler :: Prelude.Maybe Prelude.Text,
    -- | The function\'s image configuration values.
    imageConfigResponse :: Prelude.Maybe ImageConfigResponse,
    -- | The KMS key that\'s used to encrypt the function\'s environment
    -- variables. This key is returned only if you\'ve configured a customer
    -- managed key.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the function was last updated, in
    -- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
    -- (YYYY-MM-DDThh:mm:ss.sTZD).
    lastModified :: Prelude.Maybe Prelude.Text,
    -- | The status of the last update that was performed on the function. This
    -- is first set to @Successful@ after function creation completes.
    lastUpdateStatus :: Prelude.Maybe LastUpdateStatus,
    -- | The reason for the last update that was performed on the function.
    lastUpdateStatusReason :: Prelude.Maybe Prelude.Text,
    -- | The reason code for the last update that was performed on the function.
    lastUpdateStatusReasonCode :: Prelude.Maybe LastUpdateStatusReasonCode,
    -- | The function\'s
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers>.
    layers :: Prelude.Maybe [Layer],
    -- | For Lambda\@Edge functions, the ARN of the main function.
    masterArn :: Prelude.Maybe Prelude.Text,
    -- | The amount of memory available to the function at runtime.
    memorySize :: Prelude.Maybe Prelude.Natural,
    -- | The type of deployment package. Set to @Image@ for container image and
    -- set @Zip@ for .zip file archive.
    packageType :: Prelude.Maybe PackageType,
    -- | The latest updated revision of the function or alias.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The function\'s execution role.
    role' :: Prelude.Maybe Prelude.Text,
    -- | The runtime environment for the Lambda function.
    runtime :: Prelude.Maybe Runtime,
    -- | The ARN of the signing job.
    signingJobArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the signing profile version.
    signingProfileVersionArn :: Prelude.Maybe Prelude.Text,
    -- | Set @ApplyOn@ to @PublishedVersions@ to create a snapshot of the
    -- initialized execution environment when you publish a function version.
    -- For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/snapstart.html Reducing startup time with Lambda SnapStart>.
    snapStart :: Prelude.Maybe SnapStartResponse,
    -- | The current state of the function. When the state is @Inactive@, you can
    -- reactivate the function by invoking it.
    state :: Prelude.Maybe State,
    -- | The reason for the function\'s current state.
    stateReason :: Prelude.Maybe Prelude.Text,
    -- | The reason code for the function\'s current state. When the code is
    -- @Creating@, you can\'t invoke or modify the function.
    stateReasonCode :: Prelude.Maybe StateReasonCode,
    -- | The amount of time in seconds that Lambda allows a function to run
    -- before stopping it.
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The function\'s X-Ray tracing configuration.
    tracingConfig :: Prelude.Maybe TracingConfigResponse,
    -- | The version of the Lambda function.
    version :: Prelude.Maybe Prelude.Text,
    -- | The function\'s networking configuration.
    vpcConfig :: Prelude.Maybe VpcConfigResponse
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunctionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'architectures', 'functionConfiguration_architectures' - The instruction set architecture that the function supports.
-- Architecture is a string array with one of the valid values. The default
-- architecture value is @x86_64@.
--
-- 'codeSha256', 'functionConfiguration_codeSha256' - The SHA256 hash of the function\'s deployment package.
--
-- 'codeSize', 'functionConfiguration_codeSize' - The size of the function\'s deployment package, in bytes.
--
-- 'deadLetterConfig', 'functionConfiguration_deadLetterConfig' - The function\'s dead letter queue.
--
-- 'description', 'functionConfiguration_description' - The function\'s description.
--
-- 'environment', 'functionConfiguration_environment' - The function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-envvars.html environment variables>.
-- Omitted from CloudTrail logs.
--
-- 'ephemeralStorage', 'functionConfiguration_ephemeralStorage' - The size of the function’s @\/tmp@ directory in MB. The default value is
-- 512, but it can be any whole number between 512 and 10,240 MB.
--
-- 'fileSystemConfigs', 'functionConfiguration_fileSystemConfigs' - Connection settings for an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-filesystem.html Amazon EFS file system>.
--
-- 'functionArn', 'functionConfiguration_functionArn' - The function\'s Amazon Resource Name (ARN).
--
-- 'functionName', 'functionConfiguration_functionName' - The name of the function.
--
-- 'handler', 'functionConfiguration_handler' - The function that Lambda calls to begin running your function.
--
-- 'imageConfigResponse', 'functionConfiguration_imageConfigResponse' - The function\'s image configuration values.
--
-- 'kmsKeyArn', 'functionConfiguration_kmsKeyArn' - The KMS key that\'s used to encrypt the function\'s environment
-- variables. This key is returned only if you\'ve configured a customer
-- managed key.
--
-- 'lastModified', 'functionConfiguration_lastModified' - The date and time that the function was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- 'lastUpdateStatus', 'functionConfiguration_lastUpdateStatus' - The status of the last update that was performed on the function. This
-- is first set to @Successful@ after function creation completes.
--
-- 'lastUpdateStatusReason', 'functionConfiguration_lastUpdateStatusReason' - The reason for the last update that was performed on the function.
--
-- 'lastUpdateStatusReasonCode', 'functionConfiguration_lastUpdateStatusReasonCode' - The reason code for the last update that was performed on the function.
--
-- 'layers', 'functionConfiguration_layers' - The function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers>.
--
-- 'masterArn', 'functionConfiguration_masterArn' - For Lambda\@Edge functions, the ARN of the main function.
--
-- 'memorySize', 'functionConfiguration_memorySize' - The amount of memory available to the function at runtime.
--
-- 'packageType', 'functionConfiguration_packageType' - The type of deployment package. Set to @Image@ for container image and
-- set @Zip@ for .zip file archive.
--
-- 'revisionId', 'functionConfiguration_revisionId' - The latest updated revision of the function or alias.
--
-- 'role'', 'functionConfiguration_role' - The function\'s execution role.
--
-- 'runtime', 'functionConfiguration_runtime' - The runtime environment for the Lambda function.
--
-- 'signingJobArn', 'functionConfiguration_signingJobArn' - The ARN of the signing job.
--
-- 'signingProfileVersionArn', 'functionConfiguration_signingProfileVersionArn' - The ARN of the signing profile version.
--
-- 'snapStart', 'functionConfiguration_snapStart' - Set @ApplyOn@ to @PublishedVersions@ to create a snapshot of the
-- initialized execution environment when you publish a function version.
-- For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/snapstart.html Reducing startup time with Lambda SnapStart>.
--
-- 'state', 'functionConfiguration_state' - The current state of the function. When the state is @Inactive@, you can
-- reactivate the function by invoking it.
--
-- 'stateReason', 'functionConfiguration_stateReason' - The reason for the function\'s current state.
--
-- 'stateReasonCode', 'functionConfiguration_stateReasonCode' - The reason code for the function\'s current state. When the code is
-- @Creating@, you can\'t invoke or modify the function.
--
-- 'timeout', 'functionConfiguration_timeout' - The amount of time in seconds that Lambda allows a function to run
-- before stopping it.
--
-- 'tracingConfig', 'functionConfiguration_tracingConfig' - The function\'s X-Ray tracing configuration.
--
-- 'version', 'functionConfiguration_version' - The version of the Lambda function.
--
-- 'vpcConfig', 'functionConfiguration_vpcConfig' - The function\'s networking configuration.
newFunctionConfiguration ::
  FunctionConfiguration
newFunctionConfiguration =
  FunctionConfiguration'
    { architectures =
        Prelude.Nothing,
      codeSha256 = Prelude.Nothing,
      codeSize = Prelude.Nothing,
      deadLetterConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      environment = Prelude.Nothing,
      ephemeralStorage = Prelude.Nothing,
      fileSystemConfigs = Prelude.Nothing,
      functionArn = Prelude.Nothing,
      functionName = Prelude.Nothing,
      handler = Prelude.Nothing,
      imageConfigResponse = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      lastUpdateStatus = Prelude.Nothing,
      lastUpdateStatusReason = Prelude.Nothing,
      lastUpdateStatusReasonCode = Prelude.Nothing,
      layers = Prelude.Nothing,
      masterArn = Prelude.Nothing,
      memorySize = Prelude.Nothing,
      packageType = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      role' = Prelude.Nothing,
      runtime = Prelude.Nothing,
      signingJobArn = Prelude.Nothing,
      signingProfileVersionArn = Prelude.Nothing,
      snapStart = Prelude.Nothing,
      state = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      stateReasonCode = Prelude.Nothing,
      timeout = Prelude.Nothing,
      tracingConfig = Prelude.Nothing,
      version = Prelude.Nothing,
      vpcConfig = Prelude.Nothing
    }

-- | The instruction set architecture that the function supports.
-- Architecture is a string array with one of the valid values. The default
-- architecture value is @x86_64@.
functionConfiguration_architectures :: Lens.Lens' FunctionConfiguration (Prelude.Maybe (Prelude.NonEmpty Architecture))
functionConfiguration_architectures = Lens.lens (\FunctionConfiguration' {architectures} -> architectures) (\s@FunctionConfiguration' {} a -> s {architectures = a} :: FunctionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The SHA256 hash of the function\'s deployment package.
functionConfiguration_codeSha256 :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_codeSha256 = Lens.lens (\FunctionConfiguration' {codeSha256} -> codeSha256) (\s@FunctionConfiguration' {} a -> s {codeSha256 = a} :: FunctionConfiguration)

-- | The size of the function\'s deployment package, in bytes.
functionConfiguration_codeSize :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Integer)
functionConfiguration_codeSize = Lens.lens (\FunctionConfiguration' {codeSize} -> codeSize) (\s@FunctionConfiguration' {} a -> s {codeSize = a} :: FunctionConfiguration)

-- | The function\'s dead letter queue.
functionConfiguration_deadLetterConfig :: Lens.Lens' FunctionConfiguration (Prelude.Maybe DeadLetterConfig)
functionConfiguration_deadLetterConfig = Lens.lens (\FunctionConfiguration' {deadLetterConfig} -> deadLetterConfig) (\s@FunctionConfiguration' {} a -> s {deadLetterConfig = a} :: FunctionConfiguration)

-- | The function\'s description.
functionConfiguration_description :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_description = Lens.lens (\FunctionConfiguration' {description} -> description) (\s@FunctionConfiguration' {} a -> s {description = a} :: FunctionConfiguration)

-- | The function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-envvars.html environment variables>.
-- Omitted from CloudTrail logs.
functionConfiguration_environment :: Lens.Lens' FunctionConfiguration (Prelude.Maybe EnvironmentResponse)
functionConfiguration_environment = Lens.lens (\FunctionConfiguration' {environment} -> environment) (\s@FunctionConfiguration' {} a -> s {environment = a} :: FunctionConfiguration)

-- | The size of the function’s @\/tmp@ directory in MB. The default value is
-- 512, but it can be any whole number between 512 and 10,240 MB.
functionConfiguration_ephemeralStorage :: Lens.Lens' FunctionConfiguration (Prelude.Maybe EphemeralStorage)
functionConfiguration_ephemeralStorage = Lens.lens (\FunctionConfiguration' {ephemeralStorage} -> ephemeralStorage) (\s@FunctionConfiguration' {} a -> s {ephemeralStorage = a} :: FunctionConfiguration)

-- | Connection settings for an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-filesystem.html Amazon EFS file system>.
functionConfiguration_fileSystemConfigs :: Lens.Lens' FunctionConfiguration (Prelude.Maybe [FileSystemConfig])
functionConfiguration_fileSystemConfigs = Lens.lens (\FunctionConfiguration' {fileSystemConfigs} -> fileSystemConfigs) (\s@FunctionConfiguration' {} a -> s {fileSystemConfigs = a} :: FunctionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The function\'s Amazon Resource Name (ARN).
functionConfiguration_functionArn :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_functionArn = Lens.lens (\FunctionConfiguration' {functionArn} -> functionArn) (\s@FunctionConfiguration' {} a -> s {functionArn = a} :: FunctionConfiguration)

-- | The name of the function.
functionConfiguration_functionName :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_functionName = Lens.lens (\FunctionConfiguration' {functionName} -> functionName) (\s@FunctionConfiguration' {} a -> s {functionName = a} :: FunctionConfiguration)

-- | The function that Lambda calls to begin running your function.
functionConfiguration_handler :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_handler = Lens.lens (\FunctionConfiguration' {handler} -> handler) (\s@FunctionConfiguration' {} a -> s {handler = a} :: FunctionConfiguration)

-- | The function\'s image configuration values.
functionConfiguration_imageConfigResponse :: Lens.Lens' FunctionConfiguration (Prelude.Maybe ImageConfigResponse)
functionConfiguration_imageConfigResponse = Lens.lens (\FunctionConfiguration' {imageConfigResponse} -> imageConfigResponse) (\s@FunctionConfiguration' {} a -> s {imageConfigResponse = a} :: FunctionConfiguration)

-- | The KMS key that\'s used to encrypt the function\'s environment
-- variables. This key is returned only if you\'ve configured a customer
-- managed key.
functionConfiguration_kmsKeyArn :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_kmsKeyArn = Lens.lens (\FunctionConfiguration' {kmsKeyArn} -> kmsKeyArn) (\s@FunctionConfiguration' {} a -> s {kmsKeyArn = a} :: FunctionConfiguration)

-- | The date and time that the function was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
functionConfiguration_lastModified :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_lastModified = Lens.lens (\FunctionConfiguration' {lastModified} -> lastModified) (\s@FunctionConfiguration' {} a -> s {lastModified = a} :: FunctionConfiguration)

-- | The status of the last update that was performed on the function. This
-- is first set to @Successful@ after function creation completes.
functionConfiguration_lastUpdateStatus :: Lens.Lens' FunctionConfiguration (Prelude.Maybe LastUpdateStatus)
functionConfiguration_lastUpdateStatus = Lens.lens (\FunctionConfiguration' {lastUpdateStatus} -> lastUpdateStatus) (\s@FunctionConfiguration' {} a -> s {lastUpdateStatus = a} :: FunctionConfiguration)

-- | The reason for the last update that was performed on the function.
functionConfiguration_lastUpdateStatusReason :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_lastUpdateStatusReason = Lens.lens (\FunctionConfiguration' {lastUpdateStatusReason} -> lastUpdateStatusReason) (\s@FunctionConfiguration' {} a -> s {lastUpdateStatusReason = a} :: FunctionConfiguration)

-- | The reason code for the last update that was performed on the function.
functionConfiguration_lastUpdateStatusReasonCode :: Lens.Lens' FunctionConfiguration (Prelude.Maybe LastUpdateStatusReasonCode)
functionConfiguration_lastUpdateStatusReasonCode = Lens.lens (\FunctionConfiguration' {lastUpdateStatusReasonCode} -> lastUpdateStatusReasonCode) (\s@FunctionConfiguration' {} a -> s {lastUpdateStatusReasonCode = a} :: FunctionConfiguration)

-- | The function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers>.
functionConfiguration_layers :: Lens.Lens' FunctionConfiguration (Prelude.Maybe [Layer])
functionConfiguration_layers = Lens.lens (\FunctionConfiguration' {layers} -> layers) (\s@FunctionConfiguration' {} a -> s {layers = a} :: FunctionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | For Lambda\@Edge functions, the ARN of the main function.
functionConfiguration_masterArn :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_masterArn = Lens.lens (\FunctionConfiguration' {masterArn} -> masterArn) (\s@FunctionConfiguration' {} a -> s {masterArn = a} :: FunctionConfiguration)

-- | The amount of memory available to the function at runtime.
functionConfiguration_memorySize :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Natural)
functionConfiguration_memorySize = Lens.lens (\FunctionConfiguration' {memorySize} -> memorySize) (\s@FunctionConfiguration' {} a -> s {memorySize = a} :: FunctionConfiguration)

-- | The type of deployment package. Set to @Image@ for container image and
-- set @Zip@ for .zip file archive.
functionConfiguration_packageType :: Lens.Lens' FunctionConfiguration (Prelude.Maybe PackageType)
functionConfiguration_packageType = Lens.lens (\FunctionConfiguration' {packageType} -> packageType) (\s@FunctionConfiguration' {} a -> s {packageType = a} :: FunctionConfiguration)

-- | The latest updated revision of the function or alias.
functionConfiguration_revisionId :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_revisionId = Lens.lens (\FunctionConfiguration' {revisionId} -> revisionId) (\s@FunctionConfiguration' {} a -> s {revisionId = a} :: FunctionConfiguration)

-- | The function\'s execution role.
functionConfiguration_role :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_role = Lens.lens (\FunctionConfiguration' {role'} -> role') (\s@FunctionConfiguration' {} a -> s {role' = a} :: FunctionConfiguration)

-- | The runtime environment for the Lambda function.
functionConfiguration_runtime :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Runtime)
functionConfiguration_runtime = Lens.lens (\FunctionConfiguration' {runtime} -> runtime) (\s@FunctionConfiguration' {} a -> s {runtime = a} :: FunctionConfiguration)

-- | The ARN of the signing job.
functionConfiguration_signingJobArn :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_signingJobArn = Lens.lens (\FunctionConfiguration' {signingJobArn} -> signingJobArn) (\s@FunctionConfiguration' {} a -> s {signingJobArn = a} :: FunctionConfiguration)

-- | The ARN of the signing profile version.
functionConfiguration_signingProfileVersionArn :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_signingProfileVersionArn = Lens.lens (\FunctionConfiguration' {signingProfileVersionArn} -> signingProfileVersionArn) (\s@FunctionConfiguration' {} a -> s {signingProfileVersionArn = a} :: FunctionConfiguration)

-- | Set @ApplyOn@ to @PublishedVersions@ to create a snapshot of the
-- initialized execution environment when you publish a function version.
-- For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/snapstart.html Reducing startup time with Lambda SnapStart>.
functionConfiguration_snapStart :: Lens.Lens' FunctionConfiguration (Prelude.Maybe SnapStartResponse)
functionConfiguration_snapStart = Lens.lens (\FunctionConfiguration' {snapStart} -> snapStart) (\s@FunctionConfiguration' {} a -> s {snapStart = a} :: FunctionConfiguration)

-- | The current state of the function. When the state is @Inactive@, you can
-- reactivate the function by invoking it.
functionConfiguration_state :: Lens.Lens' FunctionConfiguration (Prelude.Maybe State)
functionConfiguration_state = Lens.lens (\FunctionConfiguration' {state} -> state) (\s@FunctionConfiguration' {} a -> s {state = a} :: FunctionConfiguration)

-- | The reason for the function\'s current state.
functionConfiguration_stateReason :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_stateReason = Lens.lens (\FunctionConfiguration' {stateReason} -> stateReason) (\s@FunctionConfiguration' {} a -> s {stateReason = a} :: FunctionConfiguration)

-- | The reason code for the function\'s current state. When the code is
-- @Creating@, you can\'t invoke or modify the function.
functionConfiguration_stateReasonCode :: Lens.Lens' FunctionConfiguration (Prelude.Maybe StateReasonCode)
functionConfiguration_stateReasonCode = Lens.lens (\FunctionConfiguration' {stateReasonCode} -> stateReasonCode) (\s@FunctionConfiguration' {} a -> s {stateReasonCode = a} :: FunctionConfiguration)

-- | The amount of time in seconds that Lambda allows a function to run
-- before stopping it.
functionConfiguration_timeout :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Natural)
functionConfiguration_timeout = Lens.lens (\FunctionConfiguration' {timeout} -> timeout) (\s@FunctionConfiguration' {} a -> s {timeout = a} :: FunctionConfiguration)

-- | The function\'s X-Ray tracing configuration.
functionConfiguration_tracingConfig :: Lens.Lens' FunctionConfiguration (Prelude.Maybe TracingConfigResponse)
functionConfiguration_tracingConfig = Lens.lens (\FunctionConfiguration' {tracingConfig} -> tracingConfig) (\s@FunctionConfiguration' {} a -> s {tracingConfig = a} :: FunctionConfiguration)

-- | The version of the Lambda function.
functionConfiguration_version :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_version = Lens.lens (\FunctionConfiguration' {version} -> version) (\s@FunctionConfiguration' {} a -> s {version = a} :: FunctionConfiguration)

-- | The function\'s networking configuration.
functionConfiguration_vpcConfig :: Lens.Lens' FunctionConfiguration (Prelude.Maybe VpcConfigResponse)
functionConfiguration_vpcConfig = Lens.lens (\FunctionConfiguration' {vpcConfig} -> vpcConfig) (\s@FunctionConfiguration' {} a -> s {vpcConfig = a} :: FunctionConfiguration)

instance Data.FromJSON FunctionConfiguration where
  parseJSON =
    Data.withObject
      "FunctionConfiguration"
      ( \x ->
          FunctionConfiguration'
            Prelude.<$> (x Data..:? "Architectures")
            Prelude.<*> (x Data..:? "CodeSha256")
            Prelude.<*> (x Data..:? "CodeSize")
            Prelude.<*> (x Data..:? "DeadLetterConfig")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Environment")
            Prelude.<*> (x Data..:? "EphemeralStorage")
            Prelude.<*> ( x Data..:? "FileSystemConfigs"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "FunctionArn")
            Prelude.<*> (x Data..:? "FunctionName")
            Prelude.<*> (x Data..:? "Handler")
            Prelude.<*> (x Data..:? "ImageConfigResponse")
            Prelude.<*> (x Data..:? "KMSKeyArn")
            Prelude.<*> (x Data..:? "LastModified")
            Prelude.<*> (x Data..:? "LastUpdateStatus")
            Prelude.<*> (x Data..:? "LastUpdateStatusReason")
            Prelude.<*> (x Data..:? "LastUpdateStatusReasonCode")
            Prelude.<*> (x Data..:? "Layers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "MasterArn")
            Prelude.<*> (x Data..:? "MemorySize")
            Prelude.<*> (x Data..:? "PackageType")
            Prelude.<*> (x Data..:? "RevisionId")
            Prelude.<*> (x Data..:? "Role")
            Prelude.<*> (x Data..:? "Runtime")
            Prelude.<*> (x Data..:? "SigningJobArn")
            Prelude.<*> (x Data..:? "SigningProfileVersionArn")
            Prelude.<*> (x Data..:? "SnapStart")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "StateReason")
            Prelude.<*> (x Data..:? "StateReasonCode")
            Prelude.<*> (x Data..:? "Timeout")
            Prelude.<*> (x Data..:? "TracingConfig")
            Prelude.<*> (x Data..:? "Version")
            Prelude.<*> (x Data..:? "VpcConfig")
      )

instance Prelude.Hashable FunctionConfiguration where
  hashWithSalt _salt FunctionConfiguration' {..} =
    _salt `Prelude.hashWithSalt` architectures
      `Prelude.hashWithSalt` codeSha256
      `Prelude.hashWithSalt` codeSize
      `Prelude.hashWithSalt` deadLetterConfig
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` ephemeralStorage
      `Prelude.hashWithSalt` fileSystemConfigs
      `Prelude.hashWithSalt` functionArn
      `Prelude.hashWithSalt` functionName
      `Prelude.hashWithSalt` handler
      `Prelude.hashWithSalt` imageConfigResponse
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` lastModified
      `Prelude.hashWithSalt` lastUpdateStatus
      `Prelude.hashWithSalt` lastUpdateStatusReason
      `Prelude.hashWithSalt` lastUpdateStatusReasonCode
      `Prelude.hashWithSalt` layers
      `Prelude.hashWithSalt` masterArn
      `Prelude.hashWithSalt` memorySize
      `Prelude.hashWithSalt` packageType
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` runtime
      `Prelude.hashWithSalt` signingJobArn
      `Prelude.hashWithSalt` signingProfileVersionArn
      `Prelude.hashWithSalt` snapStart
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` stateReason
      `Prelude.hashWithSalt` stateReasonCode
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` tracingConfig
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` vpcConfig

instance Prelude.NFData FunctionConfiguration where
  rnf FunctionConfiguration' {..} =
    Prelude.rnf architectures
      `Prelude.seq` Prelude.rnf codeSha256
      `Prelude.seq` Prelude.rnf codeSize
      `Prelude.seq` Prelude.rnf deadLetterConfig
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf ephemeralStorage
      `Prelude.seq` Prelude.rnf fileSystemConfigs
      `Prelude.seq` Prelude.rnf functionArn
      `Prelude.seq` Prelude.rnf functionName
      `Prelude.seq` Prelude.rnf handler
      `Prelude.seq` Prelude.rnf imageConfigResponse
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf lastModified
      `Prelude.seq` Prelude.rnf lastUpdateStatus
      `Prelude.seq` Prelude.rnf lastUpdateStatusReason
      `Prelude.seq` Prelude.rnf
        lastUpdateStatusReasonCode
      `Prelude.seq` Prelude.rnf layers
      `Prelude.seq` Prelude.rnf masterArn
      `Prelude.seq` Prelude.rnf memorySize
      `Prelude.seq` Prelude.rnf packageType
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf runtime
      `Prelude.seq` Prelude.rnf
        signingJobArn
      `Prelude.seq` Prelude.rnf
        signingProfileVersionArn
      `Prelude.seq` Prelude.rnf
        snapStart
      `Prelude.seq` Prelude.rnf
        state
      `Prelude.seq` Prelude.rnf
        stateReason
      `Prelude.seq` Prelude.rnf
        stateReasonCode
      `Prelude.seq` Prelude.rnf
        timeout
      `Prelude.seq` Prelude.rnf
        tracingConfig
      `Prelude.seq` Prelude.rnf
        version
      `Prelude.seq` Prelude.rnf
        vpcConfig
