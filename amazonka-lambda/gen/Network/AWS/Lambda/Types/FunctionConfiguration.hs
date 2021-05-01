{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Lambda.Types.FunctionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.FunctionConfiguration where

import Network.AWS.Lambda.Types.DeadLetterConfig
import Network.AWS.Lambda.Types.EnvironmentResponse
import Network.AWS.Lambda.Types.FileSystemConfig
import Network.AWS.Lambda.Types.ImageConfigResponse
import Network.AWS.Lambda.Types.LastUpdateStatus
import Network.AWS.Lambda.Types.LastUpdateStatusReasonCode
import Network.AWS.Lambda.Types.Layer
import Network.AWS.Lambda.Types.PackageType
import Network.AWS.Lambda.Types.Runtime
import Network.AWS.Lambda.Types.State
import Network.AWS.Lambda.Types.StateReasonCode
import Network.AWS.Lambda.Types.TracingConfigResponse
import Network.AWS.Lambda.Types.VpcConfigResponse
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about a function\'s configuration.
--
-- /See:/ 'newFunctionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
  { -- | The ARN of the signing profile version.
    signingProfileVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the last update that was performed on the function. This
    -- is first set to @Successful@ after function creation completes.
    lastUpdateStatus :: Prelude.Maybe LastUpdateStatus,
    -- | The function\'s networking configuration.
    vpcConfig :: Prelude.Maybe VpcConfigResponse,
    -- | The amount of memory available to the function at runtime.
    memorySize :: Prelude.Maybe Prelude.Natural,
    -- | For Lambda\@Edge functions, the ARN of the master function.
    masterArn :: Prelude.Maybe Prelude.Text,
    -- | The latest updated revision of the function or alias.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The reason code for the last update that was performed on the function.
    lastUpdateStatusReasonCode :: Prelude.Maybe LastUpdateStatusReasonCode,
    -- | The SHA256 hash of the function\'s deployment package.
    codeSha256 :: Prelude.Maybe Prelude.Text,
    -- | The reason for the function\'s current state.
    stateReason :: Prelude.Maybe Prelude.Text,
    -- | The amount of time in seconds that Lambda allows a function to run
    -- before stopping it.
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The function that Lambda calls to begin executing your function.
    handler :: Prelude.Maybe Prelude.Text,
    -- | The function\'s dead letter queue.
    deadLetterConfig :: Prelude.Maybe DeadLetterConfig,
    -- | The name of the function.
    functionName :: Prelude.Maybe Prelude.Text,
    -- | The function\'s environment variables.
    environment :: Prelude.Maybe EnvironmentResponse,
    -- | The version of the Lambda function.
    version :: Prelude.Maybe Prelude.Text,
    -- | The function\'s Amazon Resource Name (ARN).
    functionArn :: Prelude.Maybe Prelude.Text,
    -- | The current state of the function. When the state is @Inactive@, you can
    -- reactivate the function by invoking it.
    state :: Prelude.Maybe State,
    -- | The KMS key that\'s used to encrypt the function\'s environment
    -- variables. This key is only returned if you\'ve configured a customer
    -- managed CMK.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The runtime environment for the Lambda function.
    runtime :: Prelude.Maybe Runtime,
    -- | The function\'s execution role.
    role' :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the signing job.
    signingJobArn :: Prelude.Maybe Prelude.Text,
    -- | The reason code for the function\'s current state. When the code is
    -- @Creating@, you can\'t invoke or modify the function.
    stateReasonCode :: Prelude.Maybe StateReasonCode,
    -- | The function\'s image configuration values.
    imageConfigResponse :: Prelude.Maybe ImageConfigResponse,
    -- | The function\'s AWS X-Ray tracing configuration.
    tracingConfig :: Prelude.Maybe TracingConfigResponse,
    -- | The function\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the function was last updated, in
    -- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
    -- (YYYY-MM-DDThh:mm:ss.sTZD).
    lastModified :: Prelude.Maybe Prelude.Text,
    -- | The reason for the last update that was performed on the function.
    lastUpdateStatusReason :: Prelude.Maybe Prelude.Text,
    -- | The function\'s
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers>.
    layers :: Prelude.Maybe [Layer],
    -- | The size of the function\'s deployment package, in bytes.
    codeSize :: Prelude.Maybe Prelude.Integer,
    -- | Connection settings for an Amazon EFS file system.
    fileSystemConfigs :: Prelude.Maybe [FileSystemConfig],
    -- | The type of deployment package. Set to @Image@ for container image and
    -- set @Zip@ for .zip file archive.
    packageType :: Prelude.Maybe PackageType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FunctionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signingProfileVersionArn', 'functionConfiguration_signingProfileVersionArn' - The ARN of the signing profile version.
--
-- 'lastUpdateStatus', 'functionConfiguration_lastUpdateStatus' - The status of the last update that was performed on the function. This
-- is first set to @Successful@ after function creation completes.
--
-- 'vpcConfig', 'functionConfiguration_vpcConfig' - The function\'s networking configuration.
--
-- 'memorySize', 'functionConfiguration_memorySize' - The amount of memory available to the function at runtime.
--
-- 'masterArn', 'functionConfiguration_masterArn' - For Lambda\@Edge functions, the ARN of the master function.
--
-- 'revisionId', 'functionConfiguration_revisionId' - The latest updated revision of the function or alias.
--
-- 'lastUpdateStatusReasonCode', 'functionConfiguration_lastUpdateStatusReasonCode' - The reason code for the last update that was performed on the function.
--
-- 'codeSha256', 'functionConfiguration_codeSha256' - The SHA256 hash of the function\'s deployment package.
--
-- 'stateReason', 'functionConfiguration_stateReason' - The reason for the function\'s current state.
--
-- 'timeout', 'functionConfiguration_timeout' - The amount of time in seconds that Lambda allows a function to run
-- before stopping it.
--
-- 'handler', 'functionConfiguration_handler' - The function that Lambda calls to begin executing your function.
--
-- 'deadLetterConfig', 'functionConfiguration_deadLetterConfig' - The function\'s dead letter queue.
--
-- 'functionName', 'functionConfiguration_functionName' - The name of the function.
--
-- 'environment', 'functionConfiguration_environment' - The function\'s environment variables.
--
-- 'version', 'functionConfiguration_version' - The version of the Lambda function.
--
-- 'functionArn', 'functionConfiguration_functionArn' - The function\'s Amazon Resource Name (ARN).
--
-- 'state', 'functionConfiguration_state' - The current state of the function. When the state is @Inactive@, you can
-- reactivate the function by invoking it.
--
-- 'kmsKeyArn', 'functionConfiguration_kmsKeyArn' - The KMS key that\'s used to encrypt the function\'s environment
-- variables. This key is only returned if you\'ve configured a customer
-- managed CMK.
--
-- 'runtime', 'functionConfiguration_runtime' - The runtime environment for the Lambda function.
--
-- 'role'', 'functionConfiguration_role' - The function\'s execution role.
--
-- 'signingJobArn', 'functionConfiguration_signingJobArn' - The ARN of the signing job.
--
-- 'stateReasonCode', 'functionConfiguration_stateReasonCode' - The reason code for the function\'s current state. When the code is
-- @Creating@, you can\'t invoke or modify the function.
--
-- 'imageConfigResponse', 'functionConfiguration_imageConfigResponse' - The function\'s image configuration values.
--
-- 'tracingConfig', 'functionConfiguration_tracingConfig' - The function\'s AWS X-Ray tracing configuration.
--
-- 'description', 'functionConfiguration_description' - The function\'s description.
--
-- 'lastModified', 'functionConfiguration_lastModified' - The date and time that the function was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- 'lastUpdateStatusReason', 'functionConfiguration_lastUpdateStatusReason' - The reason for the last update that was performed on the function.
--
-- 'layers', 'functionConfiguration_layers' - The function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers>.
--
-- 'codeSize', 'functionConfiguration_codeSize' - The size of the function\'s deployment package, in bytes.
--
-- 'fileSystemConfigs', 'functionConfiguration_fileSystemConfigs' - Connection settings for an Amazon EFS file system.
--
-- 'packageType', 'functionConfiguration_packageType' - The type of deployment package. Set to @Image@ for container image and
-- set @Zip@ for .zip file archive.
newFunctionConfiguration ::
  FunctionConfiguration
newFunctionConfiguration =
  FunctionConfiguration'
    { signingProfileVersionArn =
        Prelude.Nothing,
      lastUpdateStatus = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      memorySize = Prelude.Nothing,
      masterArn = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      lastUpdateStatusReasonCode = Prelude.Nothing,
      codeSha256 = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      timeout = Prelude.Nothing,
      handler = Prelude.Nothing,
      deadLetterConfig = Prelude.Nothing,
      functionName = Prelude.Nothing,
      environment = Prelude.Nothing,
      version = Prelude.Nothing,
      functionArn = Prelude.Nothing,
      state = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      runtime = Prelude.Nothing,
      role' = Prelude.Nothing,
      signingJobArn = Prelude.Nothing,
      stateReasonCode = Prelude.Nothing,
      imageConfigResponse = Prelude.Nothing,
      tracingConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      lastUpdateStatusReason = Prelude.Nothing,
      layers = Prelude.Nothing,
      codeSize = Prelude.Nothing,
      fileSystemConfigs = Prelude.Nothing,
      packageType = Prelude.Nothing
    }

-- | The ARN of the signing profile version.
functionConfiguration_signingProfileVersionArn :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_signingProfileVersionArn = Lens.lens (\FunctionConfiguration' {signingProfileVersionArn} -> signingProfileVersionArn) (\s@FunctionConfiguration' {} a -> s {signingProfileVersionArn = a} :: FunctionConfiguration)

-- | The status of the last update that was performed on the function. This
-- is first set to @Successful@ after function creation completes.
functionConfiguration_lastUpdateStatus :: Lens.Lens' FunctionConfiguration (Prelude.Maybe LastUpdateStatus)
functionConfiguration_lastUpdateStatus = Lens.lens (\FunctionConfiguration' {lastUpdateStatus} -> lastUpdateStatus) (\s@FunctionConfiguration' {} a -> s {lastUpdateStatus = a} :: FunctionConfiguration)

-- | The function\'s networking configuration.
functionConfiguration_vpcConfig :: Lens.Lens' FunctionConfiguration (Prelude.Maybe VpcConfigResponse)
functionConfiguration_vpcConfig = Lens.lens (\FunctionConfiguration' {vpcConfig} -> vpcConfig) (\s@FunctionConfiguration' {} a -> s {vpcConfig = a} :: FunctionConfiguration)

-- | The amount of memory available to the function at runtime.
functionConfiguration_memorySize :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Natural)
functionConfiguration_memorySize = Lens.lens (\FunctionConfiguration' {memorySize} -> memorySize) (\s@FunctionConfiguration' {} a -> s {memorySize = a} :: FunctionConfiguration)

-- | For Lambda\@Edge functions, the ARN of the master function.
functionConfiguration_masterArn :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_masterArn = Lens.lens (\FunctionConfiguration' {masterArn} -> masterArn) (\s@FunctionConfiguration' {} a -> s {masterArn = a} :: FunctionConfiguration)

-- | The latest updated revision of the function or alias.
functionConfiguration_revisionId :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_revisionId = Lens.lens (\FunctionConfiguration' {revisionId} -> revisionId) (\s@FunctionConfiguration' {} a -> s {revisionId = a} :: FunctionConfiguration)

-- | The reason code for the last update that was performed on the function.
functionConfiguration_lastUpdateStatusReasonCode :: Lens.Lens' FunctionConfiguration (Prelude.Maybe LastUpdateStatusReasonCode)
functionConfiguration_lastUpdateStatusReasonCode = Lens.lens (\FunctionConfiguration' {lastUpdateStatusReasonCode} -> lastUpdateStatusReasonCode) (\s@FunctionConfiguration' {} a -> s {lastUpdateStatusReasonCode = a} :: FunctionConfiguration)

-- | The SHA256 hash of the function\'s deployment package.
functionConfiguration_codeSha256 :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_codeSha256 = Lens.lens (\FunctionConfiguration' {codeSha256} -> codeSha256) (\s@FunctionConfiguration' {} a -> s {codeSha256 = a} :: FunctionConfiguration)

-- | The reason for the function\'s current state.
functionConfiguration_stateReason :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_stateReason = Lens.lens (\FunctionConfiguration' {stateReason} -> stateReason) (\s@FunctionConfiguration' {} a -> s {stateReason = a} :: FunctionConfiguration)

-- | The amount of time in seconds that Lambda allows a function to run
-- before stopping it.
functionConfiguration_timeout :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Natural)
functionConfiguration_timeout = Lens.lens (\FunctionConfiguration' {timeout} -> timeout) (\s@FunctionConfiguration' {} a -> s {timeout = a} :: FunctionConfiguration)

-- | The function that Lambda calls to begin executing your function.
functionConfiguration_handler :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_handler = Lens.lens (\FunctionConfiguration' {handler} -> handler) (\s@FunctionConfiguration' {} a -> s {handler = a} :: FunctionConfiguration)

-- | The function\'s dead letter queue.
functionConfiguration_deadLetterConfig :: Lens.Lens' FunctionConfiguration (Prelude.Maybe DeadLetterConfig)
functionConfiguration_deadLetterConfig = Lens.lens (\FunctionConfiguration' {deadLetterConfig} -> deadLetterConfig) (\s@FunctionConfiguration' {} a -> s {deadLetterConfig = a} :: FunctionConfiguration)

-- | The name of the function.
functionConfiguration_functionName :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_functionName = Lens.lens (\FunctionConfiguration' {functionName} -> functionName) (\s@FunctionConfiguration' {} a -> s {functionName = a} :: FunctionConfiguration)

-- | The function\'s environment variables.
functionConfiguration_environment :: Lens.Lens' FunctionConfiguration (Prelude.Maybe EnvironmentResponse)
functionConfiguration_environment = Lens.lens (\FunctionConfiguration' {environment} -> environment) (\s@FunctionConfiguration' {} a -> s {environment = a} :: FunctionConfiguration)

-- | The version of the Lambda function.
functionConfiguration_version :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_version = Lens.lens (\FunctionConfiguration' {version} -> version) (\s@FunctionConfiguration' {} a -> s {version = a} :: FunctionConfiguration)

-- | The function\'s Amazon Resource Name (ARN).
functionConfiguration_functionArn :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_functionArn = Lens.lens (\FunctionConfiguration' {functionArn} -> functionArn) (\s@FunctionConfiguration' {} a -> s {functionArn = a} :: FunctionConfiguration)

-- | The current state of the function. When the state is @Inactive@, you can
-- reactivate the function by invoking it.
functionConfiguration_state :: Lens.Lens' FunctionConfiguration (Prelude.Maybe State)
functionConfiguration_state = Lens.lens (\FunctionConfiguration' {state} -> state) (\s@FunctionConfiguration' {} a -> s {state = a} :: FunctionConfiguration)

-- | The KMS key that\'s used to encrypt the function\'s environment
-- variables. This key is only returned if you\'ve configured a customer
-- managed CMK.
functionConfiguration_kmsKeyArn :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_kmsKeyArn = Lens.lens (\FunctionConfiguration' {kmsKeyArn} -> kmsKeyArn) (\s@FunctionConfiguration' {} a -> s {kmsKeyArn = a} :: FunctionConfiguration)

-- | The runtime environment for the Lambda function.
functionConfiguration_runtime :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Runtime)
functionConfiguration_runtime = Lens.lens (\FunctionConfiguration' {runtime} -> runtime) (\s@FunctionConfiguration' {} a -> s {runtime = a} :: FunctionConfiguration)

-- | The function\'s execution role.
functionConfiguration_role :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_role = Lens.lens (\FunctionConfiguration' {role'} -> role') (\s@FunctionConfiguration' {} a -> s {role' = a} :: FunctionConfiguration)

-- | The ARN of the signing job.
functionConfiguration_signingJobArn :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_signingJobArn = Lens.lens (\FunctionConfiguration' {signingJobArn} -> signingJobArn) (\s@FunctionConfiguration' {} a -> s {signingJobArn = a} :: FunctionConfiguration)

-- | The reason code for the function\'s current state. When the code is
-- @Creating@, you can\'t invoke or modify the function.
functionConfiguration_stateReasonCode :: Lens.Lens' FunctionConfiguration (Prelude.Maybe StateReasonCode)
functionConfiguration_stateReasonCode = Lens.lens (\FunctionConfiguration' {stateReasonCode} -> stateReasonCode) (\s@FunctionConfiguration' {} a -> s {stateReasonCode = a} :: FunctionConfiguration)

-- | The function\'s image configuration values.
functionConfiguration_imageConfigResponse :: Lens.Lens' FunctionConfiguration (Prelude.Maybe ImageConfigResponse)
functionConfiguration_imageConfigResponse = Lens.lens (\FunctionConfiguration' {imageConfigResponse} -> imageConfigResponse) (\s@FunctionConfiguration' {} a -> s {imageConfigResponse = a} :: FunctionConfiguration)

-- | The function\'s AWS X-Ray tracing configuration.
functionConfiguration_tracingConfig :: Lens.Lens' FunctionConfiguration (Prelude.Maybe TracingConfigResponse)
functionConfiguration_tracingConfig = Lens.lens (\FunctionConfiguration' {tracingConfig} -> tracingConfig) (\s@FunctionConfiguration' {} a -> s {tracingConfig = a} :: FunctionConfiguration)

-- | The function\'s description.
functionConfiguration_description :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_description = Lens.lens (\FunctionConfiguration' {description} -> description) (\s@FunctionConfiguration' {} a -> s {description = a} :: FunctionConfiguration)

-- | The date and time that the function was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
functionConfiguration_lastModified :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_lastModified = Lens.lens (\FunctionConfiguration' {lastModified} -> lastModified) (\s@FunctionConfiguration' {} a -> s {lastModified = a} :: FunctionConfiguration)

-- | The reason for the last update that was performed on the function.
functionConfiguration_lastUpdateStatusReason :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_lastUpdateStatusReason = Lens.lens (\FunctionConfiguration' {lastUpdateStatusReason} -> lastUpdateStatusReason) (\s@FunctionConfiguration' {} a -> s {lastUpdateStatusReason = a} :: FunctionConfiguration)

-- | The function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers>.
functionConfiguration_layers :: Lens.Lens' FunctionConfiguration (Prelude.Maybe [Layer])
functionConfiguration_layers = Lens.lens (\FunctionConfiguration' {layers} -> layers) (\s@FunctionConfiguration' {} a -> s {layers = a} :: FunctionConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | The size of the function\'s deployment package, in bytes.
functionConfiguration_codeSize :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Integer)
functionConfiguration_codeSize = Lens.lens (\FunctionConfiguration' {codeSize} -> codeSize) (\s@FunctionConfiguration' {} a -> s {codeSize = a} :: FunctionConfiguration)

-- | Connection settings for an Amazon EFS file system.
functionConfiguration_fileSystemConfigs :: Lens.Lens' FunctionConfiguration (Prelude.Maybe [FileSystemConfig])
functionConfiguration_fileSystemConfigs = Lens.lens (\FunctionConfiguration' {fileSystemConfigs} -> fileSystemConfigs) (\s@FunctionConfiguration' {} a -> s {fileSystemConfigs = a} :: FunctionConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | The type of deployment package. Set to @Image@ for container image and
-- set @Zip@ for .zip file archive.
functionConfiguration_packageType :: Lens.Lens' FunctionConfiguration (Prelude.Maybe PackageType)
functionConfiguration_packageType = Lens.lens (\FunctionConfiguration' {packageType} -> packageType) (\s@FunctionConfiguration' {} a -> s {packageType = a} :: FunctionConfiguration)

instance Prelude.FromJSON FunctionConfiguration where
  parseJSON =
    Prelude.withObject
      "FunctionConfiguration"
      ( \x ->
          FunctionConfiguration'
            Prelude.<$> (x Prelude..:? "SigningProfileVersionArn")
            Prelude.<*> (x Prelude..:? "LastUpdateStatus")
            Prelude.<*> (x Prelude..:? "VpcConfig")
            Prelude.<*> (x Prelude..:? "MemorySize")
            Prelude.<*> (x Prelude..:? "MasterArn")
            Prelude.<*> (x Prelude..:? "RevisionId")
            Prelude.<*> (x Prelude..:? "LastUpdateStatusReasonCode")
            Prelude.<*> (x Prelude..:? "CodeSha256")
            Prelude.<*> (x Prelude..:? "StateReason")
            Prelude.<*> (x Prelude..:? "Timeout")
            Prelude.<*> (x Prelude..:? "Handler")
            Prelude.<*> (x Prelude..:? "DeadLetterConfig")
            Prelude.<*> (x Prelude..:? "FunctionName")
            Prelude.<*> (x Prelude..:? "Environment")
            Prelude.<*> (x Prelude..:? "Version")
            Prelude.<*> (x Prelude..:? "FunctionArn")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "KMSKeyArn")
            Prelude.<*> (x Prelude..:? "Runtime")
            Prelude.<*> (x Prelude..:? "Role")
            Prelude.<*> (x Prelude..:? "SigningJobArn")
            Prelude.<*> (x Prelude..:? "StateReasonCode")
            Prelude.<*> (x Prelude..:? "ImageConfigResponse")
            Prelude.<*> (x Prelude..:? "TracingConfig")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "LastModified")
            Prelude.<*> (x Prelude..:? "LastUpdateStatusReason")
            Prelude.<*> (x Prelude..:? "Layers" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "CodeSize")
            Prelude.<*> ( x Prelude..:? "FileSystemConfigs"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "PackageType")
      )

instance Prelude.Hashable FunctionConfiguration

instance Prelude.NFData FunctionConfiguration
