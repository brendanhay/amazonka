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
    fcCodeSha256,
    fcCodeSize,
    fcDeadLetterConfig,
    fcDescription,
    fcEnvironment,
    fcFileSystemConfigs,
    fcFunctionArn,
    fcFunctionName,
    fcHandler,
    fcKMSKeyArn,
    fcLastModified,
    fcLastUpdateStatus,
    fcLastUpdateStatusReason,
    fcLastUpdateStatusReasonCode,
    fcLayers,
    fcMasterArn,
    fcMemorySize,
    fcRevisionId,
    fcRole,
    fcRuntime,
    fcSigningJobArn,
    fcSigningProfileVersionArn,
    fcState,
    fcStateReason,
    fcStateReasonCode,
    fcTimeout,
    fcTracingConfig,
    fcVersion,
    fcVpcConfig,
  )
where

import qualified Network.AWS.Lambda.Types.Arn as Types
import qualified Network.AWS.Lambda.Types.DeadLetterConfig as Types
import qualified Network.AWS.Lambda.Types.Description as Types
import qualified Network.AWS.Lambda.Types.EnvironmentResponse as Types
import qualified Network.AWS.Lambda.Types.FileSystemConfig as Types
import qualified Network.AWS.Lambda.Types.FunctionArn as Types
import qualified Network.AWS.Lambda.Types.Handler as Types
import qualified Network.AWS.Lambda.Types.KMSKeyArn as Types
import qualified Network.AWS.Lambda.Types.LastModified as Types
import qualified Network.AWS.Lambda.Types.LastUpdateStatus as Types
import qualified Network.AWS.Lambda.Types.LastUpdateStatusReason as Types
import qualified Network.AWS.Lambda.Types.LastUpdateStatusReasonCode as Types
import qualified Network.AWS.Lambda.Types.Layer as Types
import qualified Network.AWS.Lambda.Types.NamespacedFunctionName as Types
import qualified Network.AWS.Lambda.Types.Role as Types
import qualified Network.AWS.Lambda.Types.Runtime as Types
import qualified Network.AWS.Lambda.Types.State as Types
import qualified Network.AWS.Lambda.Types.StateReason as Types
import qualified Network.AWS.Lambda.Types.StateReasonCode as Types
import qualified Network.AWS.Lambda.Types.String as Types
import qualified Network.AWS.Lambda.Types.TracingConfigResponse as Types
import qualified Network.AWS.Lambda.Types.Version as Types
import qualified Network.AWS.Lambda.Types.VpcConfigResponse as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about a function's configuration.
--
-- /See:/ 'mkFunctionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
  { -- | The SHA256 hash of the function's deployment package.
    codeSha256 :: Core.Maybe Types.String,
    -- | The size of the function's deployment package, in bytes.
    codeSize :: Core.Maybe Core.Integer,
    -- | The function's dead letter queue.
    deadLetterConfig :: Core.Maybe Types.DeadLetterConfig,
    -- | The function's description.
    description :: Core.Maybe Types.Description,
    -- | The function's environment variables.
    environment :: Core.Maybe Types.EnvironmentResponse,
    -- | Connection settings for an Amazon EFS file system.
    fileSystemConfigs :: Core.Maybe [Types.FileSystemConfig],
    -- | The function's Amazon Resource Name (ARN).
    functionArn :: Core.Maybe Types.FunctionArn,
    -- | The name of the function.
    functionName :: Core.Maybe Types.NamespacedFunctionName,
    -- | The function that Lambda calls to begin executing your function.
    handler :: Core.Maybe Types.Handler,
    -- | The KMS key that's used to encrypt the function's environment variables. This key is only returned if you've configured a customer managed CMK.
    kMSKeyArn :: Core.Maybe Types.KMSKeyArn,
    -- | The date and time that the function was last updated, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
    lastModified :: Core.Maybe Types.LastModified,
    -- | The status of the last update that was performed on the function. This is first set to @Successful@ after function creation completes.
    lastUpdateStatus :: Core.Maybe Types.LastUpdateStatus,
    -- | The reason for the last update that was performed on the function.
    lastUpdateStatusReason :: Core.Maybe Types.LastUpdateStatusReason,
    -- | The reason code for the last update that was performed on the function.
    lastUpdateStatusReasonCode :: Core.Maybe Types.LastUpdateStatusReasonCode,
    -- | The function's <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers> .
    layers :: Core.Maybe [Types.Layer],
    -- | For Lambda@Edge functions, the ARN of the master function.
    masterArn :: Core.Maybe Types.FunctionArn,
    -- | The memory that's allocated to the function.
    memorySize :: Core.Maybe Core.Natural,
    -- | The latest updated revision of the function or alias.
    revisionId :: Core.Maybe Types.String,
    -- | The function's execution role.
    role' :: Core.Maybe Types.Role,
    -- | The runtime environment for the Lambda function.
    runtime :: Core.Maybe Types.Runtime,
    -- | The ARN of the signing job.
    signingJobArn :: Core.Maybe Types.Arn,
    -- | The ARN of the signing profile version.
    signingProfileVersionArn :: Core.Maybe Types.Arn,
    -- | The current state of the function. When the state is @Inactive@ , you can reactivate the function by invoking it.
    state :: Core.Maybe Types.State,
    -- | The reason for the function's current state.
    stateReason :: Core.Maybe Types.StateReason,
    -- | The reason code for the function's current state. When the code is @Creating@ , you can't invoke or modify the function.
    stateReasonCode :: Core.Maybe Types.StateReasonCode,
    -- | The amount of time in seconds that Lambda allows a function to run before stopping it.
    timeout :: Core.Maybe Core.Natural,
    -- | The function's AWS X-Ray tracing configuration.
    tracingConfig :: Core.Maybe Types.TracingConfigResponse,
    -- | The version of the Lambda function.
    version :: Core.Maybe Types.Version,
    -- | The function's networking configuration.
    vpcConfig :: Core.Maybe Types.VpcConfigResponse
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FunctionConfiguration' value with any optional fields omitted.
mkFunctionConfiguration ::
  FunctionConfiguration
mkFunctionConfiguration =
  FunctionConfiguration'
    { codeSha256 = Core.Nothing,
      codeSize = Core.Nothing,
      deadLetterConfig = Core.Nothing,
      description = Core.Nothing,
      environment = Core.Nothing,
      fileSystemConfigs = Core.Nothing,
      functionArn = Core.Nothing,
      functionName = Core.Nothing,
      handler = Core.Nothing,
      kMSKeyArn = Core.Nothing,
      lastModified = Core.Nothing,
      lastUpdateStatus = Core.Nothing,
      lastUpdateStatusReason = Core.Nothing,
      lastUpdateStatusReasonCode = Core.Nothing,
      layers = Core.Nothing,
      masterArn = Core.Nothing,
      memorySize = Core.Nothing,
      revisionId = Core.Nothing,
      role' = Core.Nothing,
      runtime = Core.Nothing,
      signingJobArn = Core.Nothing,
      signingProfileVersionArn = Core.Nothing,
      state = Core.Nothing,
      stateReason = Core.Nothing,
      stateReasonCode = Core.Nothing,
      timeout = Core.Nothing,
      tracingConfig = Core.Nothing,
      version = Core.Nothing,
      vpcConfig = Core.Nothing
    }

-- | The SHA256 hash of the function's deployment package.
--
-- /Note:/ Consider using 'codeSha256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcCodeSha256 :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.String)
fcCodeSha256 = Lens.field @"codeSha256"
{-# DEPRECATED fcCodeSha256 "Use generic-lens or generic-optics with 'codeSha256' instead." #-}

-- | The size of the function's deployment package, in bytes.
--
-- /Note:/ Consider using 'codeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcCodeSize :: Lens.Lens' FunctionConfiguration (Core.Maybe Core.Integer)
fcCodeSize = Lens.field @"codeSize"
{-# DEPRECATED fcCodeSize "Use generic-lens or generic-optics with 'codeSize' instead." #-}

-- | The function's dead letter queue.
--
-- /Note:/ Consider using 'deadLetterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcDeadLetterConfig :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.DeadLetterConfig)
fcDeadLetterConfig = Lens.field @"deadLetterConfig"
{-# DEPRECATED fcDeadLetterConfig "Use generic-lens or generic-optics with 'deadLetterConfig' instead." #-}

-- | The function's description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcDescription :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.Description)
fcDescription = Lens.field @"description"
{-# DEPRECATED fcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The function's environment variables.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcEnvironment :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.EnvironmentResponse)
fcEnvironment = Lens.field @"environment"
{-# DEPRECATED fcEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | Connection settings for an Amazon EFS file system.
--
-- /Note:/ Consider using 'fileSystemConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcFileSystemConfigs :: Lens.Lens' FunctionConfiguration (Core.Maybe [Types.FileSystemConfig])
fcFileSystemConfigs = Lens.field @"fileSystemConfigs"
{-# DEPRECATED fcFileSystemConfigs "Use generic-lens or generic-optics with 'fileSystemConfigs' instead." #-}

-- | The function's Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'functionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcFunctionArn :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.FunctionArn)
fcFunctionArn = Lens.field @"functionArn"
{-# DEPRECATED fcFunctionArn "Use generic-lens or generic-optics with 'functionArn' instead." #-}

-- | The name of the function.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcFunctionName :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.NamespacedFunctionName)
fcFunctionName = Lens.field @"functionName"
{-# DEPRECATED fcFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | The function that Lambda calls to begin executing your function.
--
-- /Note:/ Consider using 'handler' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcHandler :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.Handler)
fcHandler = Lens.field @"handler"
{-# DEPRECATED fcHandler "Use generic-lens or generic-optics with 'handler' instead." #-}

-- | The KMS key that's used to encrypt the function's environment variables. This key is only returned if you've configured a customer managed CMK.
--
-- /Note:/ Consider using 'kMSKeyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcKMSKeyArn :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.KMSKeyArn)
fcKMSKeyArn = Lens.field @"kMSKeyArn"
{-# DEPRECATED fcKMSKeyArn "Use generic-lens or generic-optics with 'kMSKeyArn' instead." #-}

-- | The date and time that the function was last updated, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcLastModified :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.LastModified)
fcLastModified = Lens.field @"lastModified"
{-# DEPRECATED fcLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | The status of the last update that was performed on the function. This is first set to @Successful@ after function creation completes.
--
-- /Note:/ Consider using 'lastUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcLastUpdateStatus :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.LastUpdateStatus)
fcLastUpdateStatus = Lens.field @"lastUpdateStatus"
{-# DEPRECATED fcLastUpdateStatus "Use generic-lens or generic-optics with 'lastUpdateStatus' instead." #-}

-- | The reason for the last update that was performed on the function.
--
-- /Note:/ Consider using 'lastUpdateStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcLastUpdateStatusReason :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.LastUpdateStatusReason)
fcLastUpdateStatusReason = Lens.field @"lastUpdateStatusReason"
{-# DEPRECATED fcLastUpdateStatusReason "Use generic-lens or generic-optics with 'lastUpdateStatusReason' instead." #-}

-- | The reason code for the last update that was performed on the function.
--
-- /Note:/ Consider using 'lastUpdateStatusReasonCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcLastUpdateStatusReasonCode :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.LastUpdateStatusReasonCode)
fcLastUpdateStatusReasonCode = Lens.field @"lastUpdateStatusReasonCode"
{-# DEPRECATED fcLastUpdateStatusReasonCode "Use generic-lens or generic-optics with 'lastUpdateStatusReasonCode' instead." #-}

-- | The function's <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers> .
--
-- /Note:/ Consider using 'layers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcLayers :: Lens.Lens' FunctionConfiguration (Core.Maybe [Types.Layer])
fcLayers = Lens.field @"layers"
{-# DEPRECATED fcLayers "Use generic-lens or generic-optics with 'layers' instead." #-}

-- | For Lambda@Edge functions, the ARN of the master function.
--
-- /Note:/ Consider using 'masterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcMasterArn :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.FunctionArn)
fcMasterArn = Lens.field @"masterArn"
{-# DEPRECATED fcMasterArn "Use generic-lens or generic-optics with 'masterArn' instead." #-}

-- | The memory that's allocated to the function.
--
-- /Note:/ Consider using 'memorySize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcMemorySize :: Lens.Lens' FunctionConfiguration (Core.Maybe Core.Natural)
fcMemorySize = Lens.field @"memorySize"
{-# DEPRECATED fcMemorySize "Use generic-lens or generic-optics with 'memorySize' instead." #-}

-- | The latest updated revision of the function or alias.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcRevisionId :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.String)
fcRevisionId = Lens.field @"revisionId"
{-# DEPRECATED fcRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

-- | The function's execution role.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcRole :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.Role)
fcRole = Lens.field @"role'"
{-# DEPRECATED fcRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The runtime environment for the Lambda function.
--
-- /Note:/ Consider using 'runtime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcRuntime :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.Runtime)
fcRuntime = Lens.field @"runtime"
{-# DEPRECATED fcRuntime "Use generic-lens or generic-optics with 'runtime' instead." #-}

-- | The ARN of the signing job.
--
-- /Note:/ Consider using 'signingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcSigningJobArn :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.Arn)
fcSigningJobArn = Lens.field @"signingJobArn"
{-# DEPRECATED fcSigningJobArn "Use generic-lens or generic-optics with 'signingJobArn' instead." #-}

-- | The ARN of the signing profile version.
--
-- /Note:/ Consider using 'signingProfileVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcSigningProfileVersionArn :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.Arn)
fcSigningProfileVersionArn = Lens.field @"signingProfileVersionArn"
{-# DEPRECATED fcSigningProfileVersionArn "Use generic-lens or generic-optics with 'signingProfileVersionArn' instead." #-}

-- | The current state of the function. When the state is @Inactive@ , you can reactivate the function by invoking it.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcState :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.State)
fcState = Lens.field @"state"
{-# DEPRECATED fcState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The reason for the function's current state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcStateReason :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.StateReason)
fcStateReason = Lens.field @"stateReason"
{-# DEPRECATED fcStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

-- | The reason code for the function's current state. When the code is @Creating@ , you can't invoke or modify the function.
--
-- /Note:/ Consider using 'stateReasonCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcStateReasonCode :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.StateReasonCode)
fcStateReasonCode = Lens.field @"stateReasonCode"
{-# DEPRECATED fcStateReasonCode "Use generic-lens or generic-optics with 'stateReasonCode' instead." #-}

-- | The amount of time in seconds that Lambda allows a function to run before stopping it.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcTimeout :: Lens.Lens' FunctionConfiguration (Core.Maybe Core.Natural)
fcTimeout = Lens.field @"timeout"
{-# DEPRECATED fcTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | The function's AWS X-Ray tracing configuration.
--
-- /Note:/ Consider using 'tracingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcTracingConfig :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.TracingConfigResponse)
fcTracingConfig = Lens.field @"tracingConfig"
{-# DEPRECATED fcTracingConfig "Use generic-lens or generic-optics with 'tracingConfig' instead." #-}

-- | The version of the Lambda function.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcVersion :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.Version)
fcVersion = Lens.field @"version"
{-# DEPRECATED fcVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The function's networking configuration.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcVpcConfig :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.VpcConfigResponse)
fcVpcConfig = Lens.field @"vpcConfig"
{-# DEPRECATED fcVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

instance Core.FromJSON FunctionConfiguration where
  parseJSON =
    Core.withObject "FunctionConfiguration" Core.$
      \x ->
        FunctionConfiguration'
          Core.<$> (x Core..:? "CodeSha256")
          Core.<*> (x Core..:? "CodeSize")
          Core.<*> (x Core..:? "DeadLetterConfig")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "Environment")
          Core.<*> (x Core..:? "FileSystemConfigs")
          Core.<*> (x Core..:? "FunctionArn")
          Core.<*> (x Core..:? "FunctionName")
          Core.<*> (x Core..:? "Handler")
          Core.<*> (x Core..:? "KMSKeyArn")
          Core.<*> (x Core..:? "LastModified")
          Core.<*> (x Core..:? "LastUpdateStatus")
          Core.<*> (x Core..:? "LastUpdateStatusReason")
          Core.<*> (x Core..:? "LastUpdateStatusReasonCode")
          Core.<*> (x Core..:? "Layers")
          Core.<*> (x Core..:? "MasterArn")
          Core.<*> (x Core..:? "MemorySize")
          Core.<*> (x Core..:? "RevisionId")
          Core.<*> (x Core..:? "Role")
          Core.<*> (x Core..:? "Runtime")
          Core.<*> (x Core..:? "SigningJobArn")
          Core.<*> (x Core..:? "SigningProfileVersionArn")
          Core.<*> (x Core..:? "State")
          Core.<*> (x Core..:? "StateReason")
          Core.<*> (x Core..:? "StateReasonCode")
          Core.<*> (x Core..:? "Timeout")
          Core.<*> (x Core..:? "TracingConfig")
          Core.<*> (x Core..:? "Version")
          Core.<*> (x Core..:? "VpcConfig")
