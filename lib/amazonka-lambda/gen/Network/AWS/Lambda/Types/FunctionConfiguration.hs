{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.FunctionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.FunctionConfiguration
  ( FunctionConfiguration (..),

    -- * Smart constructor
    mkFunctionConfiguration,

    -- * Lenses
    fcMemorySize,
    fcRuntime,
    fcState,
    fcSigningProfileVersionARN,
    fcLastUpdateStatus,
    fcFunctionARN,
    fcKMSKeyARN,
    fcFileSystemConfigs,
    fcEnvironment,
    fcDeadLetterConfig,
    fcSigningJobARN,
    fcRole,
    fcVPCConfig,
    fcVersion,
    fcFunctionName,
    fcLayers,
    fcCodeSize,
    fcHandler,
    fcTimeout,
    fcLastUpdateStatusReason,
    fcStateReason,
    fcLastModified,
    fcCodeSha256,
    fcTracingConfig,
    fcStateReasonCode,
    fcDescription,
    fcLastUpdateStatusReasonCode,
    fcRevisionId,
    fcMasterARN,
  )
where

import Network.AWS.Lambda.Types.DeadLetterConfig
import Network.AWS.Lambda.Types.EnvironmentResponse
import Network.AWS.Lambda.Types.FileSystemConfig
import Network.AWS.Lambda.Types.LastUpdateStatus
import Network.AWS.Lambda.Types.LastUpdateStatusReasonCode
import Network.AWS.Lambda.Types.Layer
import Network.AWS.Lambda.Types.Runtime
import Network.AWS.Lambda.Types.State
import Network.AWS.Lambda.Types.StateReasonCode
import Network.AWS.Lambda.Types.TracingConfigResponse
import Network.AWS.Lambda.Types.VPCConfigResponse
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about a function's configuration.
--
-- /See:/ 'mkFunctionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
  { -- | The memory that's allocated to the function.
    memorySize :: Lude.Maybe Lude.Natural,
    -- | The runtime environment for the Lambda function.
    runtime :: Lude.Maybe Runtime,
    -- | The current state of the function. When the state is @Inactive@ , you can reactivate the function by invoking it.
    state :: Lude.Maybe State,
    -- | The ARN of the signing profile version.
    signingProfileVersionARN :: Lude.Maybe Lude.Text,
    -- | The status of the last update that was performed on the function. This is first set to @Successful@ after function creation completes.
    lastUpdateStatus :: Lude.Maybe LastUpdateStatus,
    -- | The function's Amazon Resource Name (ARN).
    functionARN :: Lude.Maybe Lude.Text,
    -- | The KMS key that's used to encrypt the function's environment variables. This key is only returned if you've configured a customer managed CMK.
    kmsKeyARN :: Lude.Maybe Lude.Text,
    -- | Connection settings for an Amazon EFS file system.
    fileSystemConfigs :: Lude.Maybe [FileSystemConfig],
    -- | The function's environment variables.
    environment :: Lude.Maybe EnvironmentResponse,
    -- | The function's dead letter queue.
    deadLetterConfig :: Lude.Maybe DeadLetterConfig,
    -- | The ARN of the signing job.
    signingJobARN :: Lude.Maybe Lude.Text,
    -- | The function's execution role.
    role' :: Lude.Maybe Lude.Text,
    -- | The function's networking configuration.
    vpcConfig :: Lude.Maybe VPCConfigResponse,
    -- | The version of the Lambda function.
    version :: Lude.Maybe Lude.Text,
    -- | The name of the function.
    functionName :: Lude.Maybe Lude.Text,
    -- | The function's <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers> .
    layers :: Lude.Maybe [Layer],
    -- | The size of the function's deployment package, in bytes.
    codeSize :: Lude.Maybe Lude.Integer,
    -- | The function that Lambda calls to begin executing your function.
    handler :: Lude.Maybe Lude.Text,
    -- | The amount of time in seconds that Lambda allows a function to run before stopping it.
    timeout :: Lude.Maybe Lude.Natural,
    -- | The reason for the last update that was performed on the function.
    lastUpdateStatusReason :: Lude.Maybe Lude.Text,
    -- | The reason for the function's current state.
    stateReason :: Lude.Maybe Lude.Text,
    -- | The date and time that the function was last updated, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
    lastModified :: Lude.Maybe Lude.Text,
    -- | The SHA256 hash of the function's deployment package.
    codeSha256 :: Lude.Maybe Lude.Text,
    -- | The function's AWS X-Ray tracing configuration.
    tracingConfig :: Lude.Maybe TracingConfigResponse,
    -- | The reason code for the function's current state. When the code is @Creating@ , you can't invoke or modify the function.
    stateReasonCode :: Lude.Maybe StateReasonCode,
    -- | The function's description.
    description :: Lude.Maybe Lude.Text,
    -- | The reason code for the last update that was performed on the function.
    lastUpdateStatusReasonCode :: Lude.Maybe LastUpdateStatusReasonCode,
    -- | The latest updated revision of the function or alias.
    revisionId :: Lude.Maybe Lude.Text,
    -- | For Lambda@Edge functions, the ARN of the master function.
    masterARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FunctionConfiguration' with the minimum fields required to make a request.
--
-- * 'memorySize' - The memory that's allocated to the function.
-- * 'runtime' - The runtime environment for the Lambda function.
-- * 'state' - The current state of the function. When the state is @Inactive@ , you can reactivate the function by invoking it.
-- * 'signingProfileVersionARN' - The ARN of the signing profile version.
-- * 'lastUpdateStatus' - The status of the last update that was performed on the function. This is first set to @Successful@ after function creation completes.
-- * 'functionARN' - The function's Amazon Resource Name (ARN).
-- * 'kmsKeyARN' - The KMS key that's used to encrypt the function's environment variables. This key is only returned if you've configured a customer managed CMK.
-- * 'fileSystemConfigs' - Connection settings for an Amazon EFS file system.
-- * 'environment' - The function's environment variables.
-- * 'deadLetterConfig' - The function's dead letter queue.
-- * 'signingJobARN' - The ARN of the signing job.
-- * 'role'' - The function's execution role.
-- * 'vpcConfig' - The function's networking configuration.
-- * 'version' - The version of the Lambda function.
-- * 'functionName' - The name of the function.
-- * 'layers' - The function's <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers> .
-- * 'codeSize' - The size of the function's deployment package, in bytes.
-- * 'handler' - The function that Lambda calls to begin executing your function.
-- * 'timeout' - The amount of time in seconds that Lambda allows a function to run before stopping it.
-- * 'lastUpdateStatusReason' - The reason for the last update that was performed on the function.
-- * 'stateReason' - The reason for the function's current state.
-- * 'lastModified' - The date and time that the function was last updated, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
-- * 'codeSha256' - The SHA256 hash of the function's deployment package.
-- * 'tracingConfig' - The function's AWS X-Ray tracing configuration.
-- * 'stateReasonCode' - The reason code for the function's current state. When the code is @Creating@ , you can't invoke or modify the function.
-- * 'description' - The function's description.
-- * 'lastUpdateStatusReasonCode' - The reason code for the last update that was performed on the function.
-- * 'revisionId' - The latest updated revision of the function or alias.
-- * 'masterARN' - For Lambda@Edge functions, the ARN of the master function.
mkFunctionConfiguration ::
  FunctionConfiguration
mkFunctionConfiguration =
  FunctionConfiguration'
    { memorySize = Lude.Nothing,
      runtime = Lude.Nothing,
      state = Lude.Nothing,
      signingProfileVersionARN = Lude.Nothing,
      lastUpdateStatus = Lude.Nothing,
      functionARN = Lude.Nothing,
      kmsKeyARN = Lude.Nothing,
      fileSystemConfigs = Lude.Nothing,
      environment = Lude.Nothing,
      deadLetterConfig = Lude.Nothing,
      signingJobARN = Lude.Nothing,
      role' = Lude.Nothing,
      vpcConfig = Lude.Nothing,
      version = Lude.Nothing,
      functionName = Lude.Nothing,
      layers = Lude.Nothing,
      codeSize = Lude.Nothing,
      handler = Lude.Nothing,
      timeout = Lude.Nothing,
      lastUpdateStatusReason = Lude.Nothing,
      stateReason = Lude.Nothing,
      lastModified = Lude.Nothing,
      codeSha256 = Lude.Nothing,
      tracingConfig = Lude.Nothing,
      stateReasonCode = Lude.Nothing,
      description = Lude.Nothing,
      lastUpdateStatusReasonCode = Lude.Nothing,
      revisionId = Lude.Nothing,
      masterARN = Lude.Nothing
    }

-- | The memory that's allocated to the function.
--
-- /Note:/ Consider using 'memorySize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcMemorySize :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Natural)
fcMemorySize = Lens.lens (memorySize :: FunctionConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {memorySize = a} :: FunctionConfiguration)
{-# DEPRECATED fcMemorySize "Use generic-lens or generic-optics with 'memorySize' instead." #-}

-- | The runtime environment for the Lambda function.
--
-- /Note:/ Consider using 'runtime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcRuntime :: Lens.Lens' FunctionConfiguration (Lude.Maybe Runtime)
fcRuntime = Lens.lens (runtime :: FunctionConfiguration -> Lude.Maybe Runtime) (\s a -> s {runtime = a} :: FunctionConfiguration)
{-# DEPRECATED fcRuntime "Use generic-lens or generic-optics with 'runtime' instead." #-}

-- | The current state of the function. When the state is @Inactive@ , you can reactivate the function by invoking it.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcState :: Lens.Lens' FunctionConfiguration (Lude.Maybe State)
fcState = Lens.lens (state :: FunctionConfiguration -> Lude.Maybe State) (\s a -> s {state = a} :: FunctionConfiguration)
{-# DEPRECATED fcState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ARN of the signing profile version.
--
-- /Note:/ Consider using 'signingProfileVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcSigningProfileVersionARN :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcSigningProfileVersionARN = Lens.lens (signingProfileVersionARN :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {signingProfileVersionARN = a} :: FunctionConfiguration)
{-# DEPRECATED fcSigningProfileVersionARN "Use generic-lens or generic-optics with 'signingProfileVersionARN' instead." #-}

-- | The status of the last update that was performed on the function. This is first set to @Successful@ after function creation completes.
--
-- /Note:/ Consider using 'lastUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcLastUpdateStatus :: Lens.Lens' FunctionConfiguration (Lude.Maybe LastUpdateStatus)
fcLastUpdateStatus = Lens.lens (lastUpdateStatus :: FunctionConfiguration -> Lude.Maybe LastUpdateStatus) (\s a -> s {lastUpdateStatus = a} :: FunctionConfiguration)
{-# DEPRECATED fcLastUpdateStatus "Use generic-lens or generic-optics with 'lastUpdateStatus' instead." #-}

-- | The function's Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'functionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcFunctionARN :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcFunctionARN = Lens.lens (functionARN :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {functionARN = a} :: FunctionConfiguration)
{-# DEPRECATED fcFunctionARN "Use generic-lens or generic-optics with 'functionARN' instead." #-}

-- | The KMS key that's used to encrypt the function's environment variables. This key is only returned if you've configured a customer managed CMK.
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcKMSKeyARN :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcKMSKeyARN = Lens.lens (kmsKeyARN :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyARN = a} :: FunctionConfiguration)
{-# DEPRECATED fcKMSKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead." #-}

-- | Connection settings for an Amazon EFS file system.
--
-- /Note:/ Consider using 'fileSystemConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcFileSystemConfigs :: Lens.Lens' FunctionConfiguration (Lude.Maybe [FileSystemConfig])
fcFileSystemConfigs = Lens.lens (fileSystemConfigs :: FunctionConfiguration -> Lude.Maybe [FileSystemConfig]) (\s a -> s {fileSystemConfigs = a} :: FunctionConfiguration)
{-# DEPRECATED fcFileSystemConfigs "Use generic-lens or generic-optics with 'fileSystemConfigs' instead." #-}

-- | The function's environment variables.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcEnvironment :: Lens.Lens' FunctionConfiguration (Lude.Maybe EnvironmentResponse)
fcEnvironment = Lens.lens (environment :: FunctionConfiguration -> Lude.Maybe EnvironmentResponse) (\s a -> s {environment = a} :: FunctionConfiguration)
{-# DEPRECATED fcEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | The function's dead letter queue.
--
-- /Note:/ Consider using 'deadLetterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcDeadLetterConfig :: Lens.Lens' FunctionConfiguration (Lude.Maybe DeadLetterConfig)
fcDeadLetterConfig = Lens.lens (deadLetterConfig :: FunctionConfiguration -> Lude.Maybe DeadLetterConfig) (\s a -> s {deadLetterConfig = a} :: FunctionConfiguration)
{-# DEPRECATED fcDeadLetterConfig "Use generic-lens or generic-optics with 'deadLetterConfig' instead." #-}

-- | The ARN of the signing job.
--
-- /Note:/ Consider using 'signingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcSigningJobARN :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcSigningJobARN = Lens.lens (signingJobARN :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {signingJobARN = a} :: FunctionConfiguration)
{-# DEPRECATED fcSigningJobARN "Use generic-lens or generic-optics with 'signingJobARN' instead." #-}

-- | The function's execution role.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcRole :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcRole = Lens.lens (role' :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {role' = a} :: FunctionConfiguration)
{-# DEPRECATED fcRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The function's networking configuration.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcVPCConfig :: Lens.Lens' FunctionConfiguration (Lude.Maybe VPCConfigResponse)
fcVPCConfig = Lens.lens (vpcConfig :: FunctionConfiguration -> Lude.Maybe VPCConfigResponse) (\s a -> s {vpcConfig = a} :: FunctionConfiguration)
{-# DEPRECATED fcVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | The version of the Lambda function.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcVersion :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcVersion = Lens.lens (version :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: FunctionConfiguration)
{-# DEPRECATED fcVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the function.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcFunctionName :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcFunctionName = Lens.lens (functionName :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {functionName = a} :: FunctionConfiguration)
{-# DEPRECATED fcFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | The function's <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers> .
--
-- /Note:/ Consider using 'layers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcLayers :: Lens.Lens' FunctionConfiguration (Lude.Maybe [Layer])
fcLayers = Lens.lens (layers :: FunctionConfiguration -> Lude.Maybe [Layer]) (\s a -> s {layers = a} :: FunctionConfiguration)
{-# DEPRECATED fcLayers "Use generic-lens or generic-optics with 'layers' instead." #-}

-- | The size of the function's deployment package, in bytes.
--
-- /Note:/ Consider using 'codeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcCodeSize :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Integer)
fcCodeSize = Lens.lens (codeSize :: FunctionConfiguration -> Lude.Maybe Lude.Integer) (\s a -> s {codeSize = a} :: FunctionConfiguration)
{-# DEPRECATED fcCodeSize "Use generic-lens or generic-optics with 'codeSize' instead." #-}

-- | The function that Lambda calls to begin executing your function.
--
-- /Note:/ Consider using 'handler' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcHandler :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcHandler = Lens.lens (handler :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {handler = a} :: FunctionConfiguration)
{-# DEPRECATED fcHandler "Use generic-lens or generic-optics with 'handler' instead." #-}

-- | The amount of time in seconds that Lambda allows a function to run before stopping it.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcTimeout :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Natural)
fcTimeout = Lens.lens (timeout :: FunctionConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {timeout = a} :: FunctionConfiguration)
{-# DEPRECATED fcTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | The reason for the last update that was performed on the function.
--
-- /Note:/ Consider using 'lastUpdateStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcLastUpdateStatusReason :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcLastUpdateStatusReason = Lens.lens (lastUpdateStatusReason :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdateStatusReason = a} :: FunctionConfiguration)
{-# DEPRECATED fcLastUpdateStatusReason "Use generic-lens or generic-optics with 'lastUpdateStatusReason' instead." #-}

-- | The reason for the function's current state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcStateReason :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcStateReason = Lens.lens (stateReason :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {stateReason = a} :: FunctionConfiguration)
{-# DEPRECATED fcStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

-- | The date and time that the function was last updated, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcLastModified :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcLastModified = Lens.lens (lastModified :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {lastModified = a} :: FunctionConfiguration)
{-# DEPRECATED fcLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | The SHA256 hash of the function's deployment package.
--
-- /Note:/ Consider using 'codeSha256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcCodeSha256 :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcCodeSha256 = Lens.lens (codeSha256 :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {codeSha256 = a} :: FunctionConfiguration)
{-# DEPRECATED fcCodeSha256 "Use generic-lens or generic-optics with 'codeSha256' instead." #-}

-- | The function's AWS X-Ray tracing configuration.
--
-- /Note:/ Consider using 'tracingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcTracingConfig :: Lens.Lens' FunctionConfiguration (Lude.Maybe TracingConfigResponse)
fcTracingConfig = Lens.lens (tracingConfig :: FunctionConfiguration -> Lude.Maybe TracingConfigResponse) (\s a -> s {tracingConfig = a} :: FunctionConfiguration)
{-# DEPRECATED fcTracingConfig "Use generic-lens or generic-optics with 'tracingConfig' instead." #-}

-- | The reason code for the function's current state. When the code is @Creating@ , you can't invoke or modify the function.
--
-- /Note:/ Consider using 'stateReasonCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcStateReasonCode :: Lens.Lens' FunctionConfiguration (Lude.Maybe StateReasonCode)
fcStateReasonCode = Lens.lens (stateReasonCode :: FunctionConfiguration -> Lude.Maybe StateReasonCode) (\s a -> s {stateReasonCode = a} :: FunctionConfiguration)
{-# DEPRECATED fcStateReasonCode "Use generic-lens or generic-optics with 'stateReasonCode' instead." #-}

-- | The function's description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcDescription :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcDescription = Lens.lens (description :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: FunctionConfiguration)
{-# DEPRECATED fcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The reason code for the last update that was performed on the function.
--
-- /Note:/ Consider using 'lastUpdateStatusReasonCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcLastUpdateStatusReasonCode :: Lens.Lens' FunctionConfiguration (Lude.Maybe LastUpdateStatusReasonCode)
fcLastUpdateStatusReasonCode = Lens.lens (lastUpdateStatusReasonCode :: FunctionConfiguration -> Lude.Maybe LastUpdateStatusReasonCode) (\s a -> s {lastUpdateStatusReasonCode = a} :: FunctionConfiguration)
{-# DEPRECATED fcLastUpdateStatusReasonCode "Use generic-lens or generic-optics with 'lastUpdateStatusReasonCode' instead." #-}

-- | The latest updated revision of the function or alias.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcRevisionId :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcRevisionId = Lens.lens (revisionId :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {revisionId = a} :: FunctionConfiguration)
{-# DEPRECATED fcRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

-- | For Lambda@Edge functions, the ARN of the master function.
--
-- /Note:/ Consider using 'masterARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcMasterARN :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcMasterARN = Lens.lens (masterARN :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {masterARN = a} :: FunctionConfiguration)
{-# DEPRECATED fcMasterARN "Use generic-lens or generic-optics with 'masterARN' instead." #-}

instance Lude.FromJSON FunctionConfiguration where
  parseJSON =
    Lude.withObject
      "FunctionConfiguration"
      ( \x ->
          FunctionConfiguration'
            Lude.<$> (x Lude..:? "MemorySize")
            Lude.<*> (x Lude..:? "Runtime")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "SigningProfileVersionArn")
            Lude.<*> (x Lude..:? "LastUpdateStatus")
            Lude.<*> (x Lude..:? "FunctionArn")
            Lude.<*> (x Lude..:? "KMSKeyArn")
            Lude.<*> (x Lude..:? "FileSystemConfigs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Environment")
            Lude.<*> (x Lude..:? "DeadLetterConfig")
            Lude.<*> (x Lude..:? "SigningJobArn")
            Lude.<*> (x Lude..:? "Role")
            Lude.<*> (x Lude..:? "VpcConfig")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "FunctionName")
            Lude.<*> (x Lude..:? "Layers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CodeSize")
            Lude.<*> (x Lude..:? "Handler")
            Lude.<*> (x Lude..:? "Timeout")
            Lude.<*> (x Lude..:? "LastUpdateStatusReason")
            Lude.<*> (x Lude..:? "StateReason")
            Lude.<*> (x Lude..:? "LastModified")
            Lude.<*> (x Lude..:? "CodeSha256")
            Lude.<*> (x Lude..:? "TracingConfig")
            Lude.<*> (x Lude..:? "StateReasonCode")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "LastUpdateStatusReasonCode")
            Lude.<*> (x Lude..:? "RevisionId")
            Lude.<*> (x Lude..:? "MasterArn")
      )
