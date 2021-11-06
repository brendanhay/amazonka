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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.FunctionConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.Lambda.Types.Architecture
import Amazonka.Lambda.Types.DeadLetterConfig
import Amazonka.Lambda.Types.EnvironmentResponse
import Amazonka.Lambda.Types.FileSystemConfig
import Amazonka.Lambda.Types.ImageConfigResponse
import Amazonka.Lambda.Types.LastUpdateStatus
import Amazonka.Lambda.Types.LastUpdateStatusReasonCode
import Amazonka.Lambda.Types.Layer
import Amazonka.Lambda.Types.PackageType
import Amazonka.Lambda.Types.Runtime
import Amazonka.Lambda.Types.State
import Amazonka.Lambda.Types.StateReasonCode
import Amazonka.Lambda.Types.TracingConfigResponse
import Amazonka.Lambda.Types.VpcConfigResponse
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about a function\'s configuration.
--
-- /See:/ 'newFunctionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
  { -- | The amount of memory available to the function at runtime.
    memorySize :: Prelude.Maybe Prelude.Natural,
    -- | The runtime environment for the Lambda function.
    runtime :: Prelude.Maybe Runtime,
    -- | The current state of the function. When the state is @Inactive@, you can
    -- reactivate the function by invoking it.
    state :: Prelude.Maybe State,
    -- | The ARN of the signing profile version.
    signingProfileVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the last update that was performed on the function. This
    -- is first set to @Successful@ after function creation completes.
    lastUpdateStatus :: Prelude.Maybe LastUpdateStatus,
    -- | The function\'s Amazon Resource Name (ARN).
    functionArn :: Prelude.Maybe Prelude.Text,
    -- | The KMS key that\'s used to encrypt the function\'s environment
    -- variables. This key is only returned if you\'ve configured a customer
    -- managed CMK.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The type of deployment package. Set to @Image@ for container image and
    -- set @Zip@ for .zip file archive.
    packageType :: Prelude.Maybe PackageType,
    -- | Connection settings for an
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-filesystem.html Amazon EFS file system>.
    fileSystemConfigs :: Prelude.Maybe [FileSystemConfig],
    -- | The function\'s
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-envvars.html environment variables>.
    environment :: Prelude.Maybe EnvironmentResponse,
    -- | The function\'s dead letter queue.
    deadLetterConfig :: Prelude.Maybe DeadLetterConfig,
    -- | The instruction set architecture that the function supports.
    -- Architecture is a string array with one of the valid values. The default
    -- architecture value is @x86_64@.
    architectures :: Prelude.Maybe (Prelude.NonEmpty Architecture),
    -- | The ARN of the signing job.
    signingJobArn :: Prelude.Maybe Prelude.Text,
    -- | The function\'s execution role.
    role' :: Prelude.Maybe Prelude.Text,
    -- | The function\'s networking configuration.
    vpcConfig :: Prelude.Maybe VpcConfigResponse,
    -- | The version of the Lambda function.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the function.
    functionName :: Prelude.Maybe Prelude.Text,
    -- | The function\'s
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers>.
    layers :: Prelude.Maybe [Layer],
    -- | The size of the function\'s deployment package, in bytes.
    codeSize :: Prelude.Maybe Prelude.Integer,
    -- | The function that Lambda calls to begin executing your function.
    handler :: Prelude.Maybe Prelude.Text,
    -- | The amount of time in seconds that Lambda allows a function to run
    -- before stopping it.
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The reason for the last update that was performed on the function.
    lastUpdateStatusReason :: Prelude.Maybe Prelude.Text,
    -- | The reason for the function\'s current state.
    stateReason :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the function was last updated, in
    -- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
    -- (YYYY-MM-DDThh:mm:ss.sTZD).
    lastModified :: Prelude.Maybe Prelude.Text,
    -- | The SHA256 hash of the function\'s deployment package.
    codeSha256 :: Prelude.Maybe Prelude.Text,
    -- | The function\'s X-Ray tracing configuration.
    tracingConfig :: Prelude.Maybe TracingConfigResponse,
    -- | The reason code for the function\'s current state. When the code is
    -- @Creating@, you can\'t invoke or modify the function.
    stateReasonCode :: Prelude.Maybe StateReasonCode,
    -- | The function\'s image configuration values.
    imageConfigResponse :: Prelude.Maybe ImageConfigResponse,
    -- | The function\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The reason code for the last update that was performed on the function.
    lastUpdateStatusReasonCode :: Prelude.Maybe LastUpdateStatusReasonCode,
    -- | The latest updated revision of the function or alias.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | For Lambda\@Edge functions, the ARN of the master function.
    masterArn :: Prelude.Maybe Prelude.Text
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
-- 'memorySize', 'functionConfiguration_memorySize' - The amount of memory available to the function at runtime.
--
-- 'runtime', 'functionConfiguration_runtime' - The runtime environment for the Lambda function.
--
-- 'state', 'functionConfiguration_state' - The current state of the function. When the state is @Inactive@, you can
-- reactivate the function by invoking it.
--
-- 'signingProfileVersionArn', 'functionConfiguration_signingProfileVersionArn' - The ARN of the signing profile version.
--
-- 'lastUpdateStatus', 'functionConfiguration_lastUpdateStatus' - The status of the last update that was performed on the function. This
-- is first set to @Successful@ after function creation completes.
--
-- 'functionArn', 'functionConfiguration_functionArn' - The function\'s Amazon Resource Name (ARN).
--
-- 'kmsKeyArn', 'functionConfiguration_kmsKeyArn' - The KMS key that\'s used to encrypt the function\'s environment
-- variables. This key is only returned if you\'ve configured a customer
-- managed CMK.
--
-- 'packageType', 'functionConfiguration_packageType' - The type of deployment package. Set to @Image@ for container image and
-- set @Zip@ for .zip file archive.
--
-- 'fileSystemConfigs', 'functionConfiguration_fileSystemConfigs' - Connection settings for an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-filesystem.html Amazon EFS file system>.
--
-- 'environment', 'functionConfiguration_environment' - The function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-envvars.html environment variables>.
--
-- 'deadLetterConfig', 'functionConfiguration_deadLetterConfig' - The function\'s dead letter queue.
--
-- 'architectures', 'functionConfiguration_architectures' - The instruction set architecture that the function supports.
-- Architecture is a string array with one of the valid values. The default
-- architecture value is @x86_64@.
--
-- 'signingJobArn', 'functionConfiguration_signingJobArn' - The ARN of the signing job.
--
-- 'role'', 'functionConfiguration_role' - The function\'s execution role.
--
-- 'vpcConfig', 'functionConfiguration_vpcConfig' - The function\'s networking configuration.
--
-- 'version', 'functionConfiguration_version' - The version of the Lambda function.
--
-- 'functionName', 'functionConfiguration_functionName' - The name of the function.
--
-- 'layers', 'functionConfiguration_layers' - The function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers>.
--
-- 'codeSize', 'functionConfiguration_codeSize' - The size of the function\'s deployment package, in bytes.
--
-- 'handler', 'functionConfiguration_handler' - The function that Lambda calls to begin executing your function.
--
-- 'timeout', 'functionConfiguration_timeout' - The amount of time in seconds that Lambda allows a function to run
-- before stopping it.
--
-- 'lastUpdateStatusReason', 'functionConfiguration_lastUpdateStatusReason' - The reason for the last update that was performed on the function.
--
-- 'stateReason', 'functionConfiguration_stateReason' - The reason for the function\'s current state.
--
-- 'lastModified', 'functionConfiguration_lastModified' - The date and time that the function was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- 'codeSha256', 'functionConfiguration_codeSha256' - The SHA256 hash of the function\'s deployment package.
--
-- 'tracingConfig', 'functionConfiguration_tracingConfig' - The function\'s X-Ray tracing configuration.
--
-- 'stateReasonCode', 'functionConfiguration_stateReasonCode' - The reason code for the function\'s current state. When the code is
-- @Creating@, you can\'t invoke or modify the function.
--
-- 'imageConfigResponse', 'functionConfiguration_imageConfigResponse' - The function\'s image configuration values.
--
-- 'description', 'functionConfiguration_description' - The function\'s description.
--
-- 'lastUpdateStatusReasonCode', 'functionConfiguration_lastUpdateStatusReasonCode' - The reason code for the last update that was performed on the function.
--
-- 'revisionId', 'functionConfiguration_revisionId' - The latest updated revision of the function or alias.
--
-- 'masterArn', 'functionConfiguration_masterArn' - For Lambda\@Edge functions, the ARN of the master function.
newFunctionConfiguration ::
  FunctionConfiguration
newFunctionConfiguration =
  FunctionConfiguration'
    { memorySize =
        Prelude.Nothing,
      runtime = Prelude.Nothing,
      state = Prelude.Nothing,
      signingProfileVersionArn = Prelude.Nothing,
      lastUpdateStatus = Prelude.Nothing,
      functionArn = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      packageType = Prelude.Nothing,
      fileSystemConfigs = Prelude.Nothing,
      environment = Prelude.Nothing,
      deadLetterConfig = Prelude.Nothing,
      architectures = Prelude.Nothing,
      signingJobArn = Prelude.Nothing,
      role' = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      version = Prelude.Nothing,
      functionName = Prelude.Nothing,
      layers = Prelude.Nothing,
      codeSize = Prelude.Nothing,
      handler = Prelude.Nothing,
      timeout = Prelude.Nothing,
      lastUpdateStatusReason = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      codeSha256 = Prelude.Nothing,
      tracingConfig = Prelude.Nothing,
      stateReasonCode = Prelude.Nothing,
      imageConfigResponse = Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdateStatusReasonCode = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      masterArn = Prelude.Nothing
    }

-- | The amount of memory available to the function at runtime.
functionConfiguration_memorySize :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Natural)
functionConfiguration_memorySize = Lens.lens (\FunctionConfiguration' {memorySize} -> memorySize) (\s@FunctionConfiguration' {} a -> s {memorySize = a} :: FunctionConfiguration)

-- | The runtime environment for the Lambda function.
functionConfiguration_runtime :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Runtime)
functionConfiguration_runtime = Lens.lens (\FunctionConfiguration' {runtime} -> runtime) (\s@FunctionConfiguration' {} a -> s {runtime = a} :: FunctionConfiguration)

-- | The current state of the function. When the state is @Inactive@, you can
-- reactivate the function by invoking it.
functionConfiguration_state :: Lens.Lens' FunctionConfiguration (Prelude.Maybe State)
functionConfiguration_state = Lens.lens (\FunctionConfiguration' {state} -> state) (\s@FunctionConfiguration' {} a -> s {state = a} :: FunctionConfiguration)

-- | The ARN of the signing profile version.
functionConfiguration_signingProfileVersionArn :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_signingProfileVersionArn = Lens.lens (\FunctionConfiguration' {signingProfileVersionArn} -> signingProfileVersionArn) (\s@FunctionConfiguration' {} a -> s {signingProfileVersionArn = a} :: FunctionConfiguration)

-- | The status of the last update that was performed on the function. This
-- is first set to @Successful@ after function creation completes.
functionConfiguration_lastUpdateStatus :: Lens.Lens' FunctionConfiguration (Prelude.Maybe LastUpdateStatus)
functionConfiguration_lastUpdateStatus = Lens.lens (\FunctionConfiguration' {lastUpdateStatus} -> lastUpdateStatus) (\s@FunctionConfiguration' {} a -> s {lastUpdateStatus = a} :: FunctionConfiguration)

-- | The function\'s Amazon Resource Name (ARN).
functionConfiguration_functionArn :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_functionArn = Lens.lens (\FunctionConfiguration' {functionArn} -> functionArn) (\s@FunctionConfiguration' {} a -> s {functionArn = a} :: FunctionConfiguration)

-- | The KMS key that\'s used to encrypt the function\'s environment
-- variables. This key is only returned if you\'ve configured a customer
-- managed CMK.
functionConfiguration_kmsKeyArn :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_kmsKeyArn = Lens.lens (\FunctionConfiguration' {kmsKeyArn} -> kmsKeyArn) (\s@FunctionConfiguration' {} a -> s {kmsKeyArn = a} :: FunctionConfiguration)

-- | The type of deployment package. Set to @Image@ for container image and
-- set @Zip@ for .zip file archive.
functionConfiguration_packageType :: Lens.Lens' FunctionConfiguration (Prelude.Maybe PackageType)
functionConfiguration_packageType = Lens.lens (\FunctionConfiguration' {packageType} -> packageType) (\s@FunctionConfiguration' {} a -> s {packageType = a} :: FunctionConfiguration)

-- | Connection settings for an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-filesystem.html Amazon EFS file system>.
functionConfiguration_fileSystemConfigs :: Lens.Lens' FunctionConfiguration (Prelude.Maybe [FileSystemConfig])
functionConfiguration_fileSystemConfigs = Lens.lens (\FunctionConfiguration' {fileSystemConfigs} -> fileSystemConfigs) (\s@FunctionConfiguration' {} a -> s {fileSystemConfigs = a} :: FunctionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-envvars.html environment variables>.
functionConfiguration_environment :: Lens.Lens' FunctionConfiguration (Prelude.Maybe EnvironmentResponse)
functionConfiguration_environment = Lens.lens (\FunctionConfiguration' {environment} -> environment) (\s@FunctionConfiguration' {} a -> s {environment = a} :: FunctionConfiguration)

-- | The function\'s dead letter queue.
functionConfiguration_deadLetterConfig :: Lens.Lens' FunctionConfiguration (Prelude.Maybe DeadLetterConfig)
functionConfiguration_deadLetterConfig = Lens.lens (\FunctionConfiguration' {deadLetterConfig} -> deadLetterConfig) (\s@FunctionConfiguration' {} a -> s {deadLetterConfig = a} :: FunctionConfiguration)

-- | The instruction set architecture that the function supports.
-- Architecture is a string array with one of the valid values. The default
-- architecture value is @x86_64@.
functionConfiguration_architectures :: Lens.Lens' FunctionConfiguration (Prelude.Maybe (Prelude.NonEmpty Architecture))
functionConfiguration_architectures = Lens.lens (\FunctionConfiguration' {architectures} -> architectures) (\s@FunctionConfiguration' {} a -> s {architectures = a} :: FunctionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the signing job.
functionConfiguration_signingJobArn :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_signingJobArn = Lens.lens (\FunctionConfiguration' {signingJobArn} -> signingJobArn) (\s@FunctionConfiguration' {} a -> s {signingJobArn = a} :: FunctionConfiguration)

-- | The function\'s execution role.
functionConfiguration_role :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_role = Lens.lens (\FunctionConfiguration' {role'} -> role') (\s@FunctionConfiguration' {} a -> s {role' = a} :: FunctionConfiguration)

-- | The function\'s networking configuration.
functionConfiguration_vpcConfig :: Lens.Lens' FunctionConfiguration (Prelude.Maybe VpcConfigResponse)
functionConfiguration_vpcConfig = Lens.lens (\FunctionConfiguration' {vpcConfig} -> vpcConfig) (\s@FunctionConfiguration' {} a -> s {vpcConfig = a} :: FunctionConfiguration)

-- | The version of the Lambda function.
functionConfiguration_version :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_version = Lens.lens (\FunctionConfiguration' {version} -> version) (\s@FunctionConfiguration' {} a -> s {version = a} :: FunctionConfiguration)

-- | The name of the function.
functionConfiguration_functionName :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_functionName = Lens.lens (\FunctionConfiguration' {functionName} -> functionName) (\s@FunctionConfiguration' {} a -> s {functionName = a} :: FunctionConfiguration)

-- | The function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers>.
functionConfiguration_layers :: Lens.Lens' FunctionConfiguration (Prelude.Maybe [Layer])
functionConfiguration_layers = Lens.lens (\FunctionConfiguration' {layers} -> layers) (\s@FunctionConfiguration' {} a -> s {layers = a} :: FunctionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The size of the function\'s deployment package, in bytes.
functionConfiguration_codeSize :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Integer)
functionConfiguration_codeSize = Lens.lens (\FunctionConfiguration' {codeSize} -> codeSize) (\s@FunctionConfiguration' {} a -> s {codeSize = a} :: FunctionConfiguration)

-- | The function that Lambda calls to begin executing your function.
functionConfiguration_handler :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_handler = Lens.lens (\FunctionConfiguration' {handler} -> handler) (\s@FunctionConfiguration' {} a -> s {handler = a} :: FunctionConfiguration)

-- | The amount of time in seconds that Lambda allows a function to run
-- before stopping it.
functionConfiguration_timeout :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Natural)
functionConfiguration_timeout = Lens.lens (\FunctionConfiguration' {timeout} -> timeout) (\s@FunctionConfiguration' {} a -> s {timeout = a} :: FunctionConfiguration)

-- | The reason for the last update that was performed on the function.
functionConfiguration_lastUpdateStatusReason :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_lastUpdateStatusReason = Lens.lens (\FunctionConfiguration' {lastUpdateStatusReason} -> lastUpdateStatusReason) (\s@FunctionConfiguration' {} a -> s {lastUpdateStatusReason = a} :: FunctionConfiguration)

-- | The reason for the function\'s current state.
functionConfiguration_stateReason :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_stateReason = Lens.lens (\FunctionConfiguration' {stateReason} -> stateReason) (\s@FunctionConfiguration' {} a -> s {stateReason = a} :: FunctionConfiguration)

-- | The date and time that the function was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
functionConfiguration_lastModified :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_lastModified = Lens.lens (\FunctionConfiguration' {lastModified} -> lastModified) (\s@FunctionConfiguration' {} a -> s {lastModified = a} :: FunctionConfiguration)

-- | The SHA256 hash of the function\'s deployment package.
functionConfiguration_codeSha256 :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_codeSha256 = Lens.lens (\FunctionConfiguration' {codeSha256} -> codeSha256) (\s@FunctionConfiguration' {} a -> s {codeSha256 = a} :: FunctionConfiguration)

-- | The function\'s X-Ray tracing configuration.
functionConfiguration_tracingConfig :: Lens.Lens' FunctionConfiguration (Prelude.Maybe TracingConfigResponse)
functionConfiguration_tracingConfig = Lens.lens (\FunctionConfiguration' {tracingConfig} -> tracingConfig) (\s@FunctionConfiguration' {} a -> s {tracingConfig = a} :: FunctionConfiguration)

-- | The reason code for the function\'s current state. When the code is
-- @Creating@, you can\'t invoke or modify the function.
functionConfiguration_stateReasonCode :: Lens.Lens' FunctionConfiguration (Prelude.Maybe StateReasonCode)
functionConfiguration_stateReasonCode = Lens.lens (\FunctionConfiguration' {stateReasonCode} -> stateReasonCode) (\s@FunctionConfiguration' {} a -> s {stateReasonCode = a} :: FunctionConfiguration)

-- | The function\'s image configuration values.
functionConfiguration_imageConfigResponse :: Lens.Lens' FunctionConfiguration (Prelude.Maybe ImageConfigResponse)
functionConfiguration_imageConfigResponse = Lens.lens (\FunctionConfiguration' {imageConfigResponse} -> imageConfigResponse) (\s@FunctionConfiguration' {} a -> s {imageConfigResponse = a} :: FunctionConfiguration)

-- | The function\'s description.
functionConfiguration_description :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_description = Lens.lens (\FunctionConfiguration' {description} -> description) (\s@FunctionConfiguration' {} a -> s {description = a} :: FunctionConfiguration)

-- | The reason code for the last update that was performed on the function.
functionConfiguration_lastUpdateStatusReasonCode :: Lens.Lens' FunctionConfiguration (Prelude.Maybe LastUpdateStatusReasonCode)
functionConfiguration_lastUpdateStatusReasonCode = Lens.lens (\FunctionConfiguration' {lastUpdateStatusReasonCode} -> lastUpdateStatusReasonCode) (\s@FunctionConfiguration' {} a -> s {lastUpdateStatusReasonCode = a} :: FunctionConfiguration)

-- | The latest updated revision of the function or alias.
functionConfiguration_revisionId :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_revisionId = Lens.lens (\FunctionConfiguration' {revisionId} -> revisionId) (\s@FunctionConfiguration' {} a -> s {revisionId = a} :: FunctionConfiguration)

-- | For Lambda\@Edge functions, the ARN of the master function.
functionConfiguration_masterArn :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_masterArn = Lens.lens (\FunctionConfiguration' {masterArn} -> masterArn) (\s@FunctionConfiguration' {} a -> s {masterArn = a} :: FunctionConfiguration)

instance Core.FromJSON FunctionConfiguration where
  parseJSON =
    Core.withObject
      "FunctionConfiguration"
      ( \x ->
          FunctionConfiguration'
            Prelude.<$> (x Core..:? "MemorySize")
            Prelude.<*> (x Core..:? "Runtime")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "SigningProfileVersionArn")
            Prelude.<*> (x Core..:? "LastUpdateStatus")
            Prelude.<*> (x Core..:? "FunctionArn")
            Prelude.<*> (x Core..:? "KMSKeyArn")
            Prelude.<*> (x Core..:? "PackageType")
            Prelude.<*> ( x Core..:? "FileSystemConfigs"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Environment")
            Prelude.<*> (x Core..:? "DeadLetterConfig")
            Prelude.<*> (x Core..:? "Architectures")
            Prelude.<*> (x Core..:? "SigningJobArn")
            Prelude.<*> (x Core..:? "Role")
            Prelude.<*> (x Core..:? "VpcConfig")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "FunctionName")
            Prelude.<*> (x Core..:? "Layers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CodeSize")
            Prelude.<*> (x Core..:? "Handler")
            Prelude.<*> (x Core..:? "Timeout")
            Prelude.<*> (x Core..:? "LastUpdateStatusReason")
            Prelude.<*> (x Core..:? "StateReason")
            Prelude.<*> (x Core..:? "LastModified")
            Prelude.<*> (x Core..:? "CodeSha256")
            Prelude.<*> (x Core..:? "TracingConfig")
            Prelude.<*> (x Core..:? "StateReasonCode")
            Prelude.<*> (x Core..:? "ImageConfigResponse")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "LastUpdateStatusReasonCode")
            Prelude.<*> (x Core..:? "RevisionId")
            Prelude.<*> (x Core..:? "MasterArn")
      )

instance Prelude.Hashable FunctionConfiguration

instance Prelude.NFData FunctionConfiguration
