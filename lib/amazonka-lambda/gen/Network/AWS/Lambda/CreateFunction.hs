{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.CreateFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Lambda function. To create a function, you need a <https://docs.aws.amazon.com/lambda/latest/dg/deployment-package-v2.html deployment package> and an <https://docs.aws.amazon.com/lambda/latest/dg/intro-permission-model.html#lambda-intro-execution-role execution role> . The deployment package contains your function code. The execution role grants the function permission to use AWS services, such as Amazon CloudWatch Logs for log streaming and AWS X-Ray for request tracing.
--
-- When you create a function, Lambda provisions an instance of the function and its supporting resources. If your function connects to a VPC, this process can take a minute or so. During this time, you can't invoke or modify the function. The @State@ , @StateReason@ , and @StateReasonCode@ fields in the response from 'GetFunctionConfiguration' indicate when the function is ready to invoke. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/functions-states.html Function States> .
-- A function has an unpublished version, and can have published versions and aliases. The unpublished version changes when you update your function's code and configuration. A published version is a snapshot of your function code and configuration that can't be changed. An alias is a named resource that maps to a version, and can be changed to map to a different version. Use the @Publish@ parameter to create version @1@ of your function from its initial configuration.
-- The other parameters let you configure version-specific and function-level settings. You can modify version-specific settings later with 'UpdateFunctionConfiguration' . Function-level settings apply to both the unpublished and published versions of the function, and include tags ('TagResource' ) and per-function concurrency limits ('PutFunctionConcurrency' ).
-- To enable code signing for this function, specify the ARN of a code-signing configuration. When a user attempts to deploy a code package with 'UpdateFunctionCode' , Lambda checks that the code package has a valid signature from a trusted publisher. The code-signing configuration includes set set of signing profiles, which define the trusted publishers for this function.
-- If another account or an AWS service invokes your function, use 'AddPermission' to grant permission by creating a resource-based IAM policy. You can grant permissions at the function level, on a version, or on an alias.
-- To invoke your function directly, use 'Invoke' . To invoke your function in response to events in other AWS services, create an event source mapping ('CreateEventSourceMapping' ), or configure a function trigger in the other service. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/lambda-invocation.html Invoking Functions> .
module Network.AWS.Lambda.CreateFunction
    (
    -- * Creating a request
      CreateFunction (..)
    , mkCreateFunction
    -- ** Request lenses
    , cfFunctionName
    , cfRuntime
    , cfRole
    , cfHandler
    , cfCode
    , cfCodeSigningConfigArn
    , cfDeadLetterConfig
    , cfDescription
    , cfEnvironment
    , cfFileSystemConfigs
    , cfKMSKeyArn
    , cfLayers
    , cfMemorySize
    , cfPublish
    , cfTags
    , cfTimeout
    , cfTracingConfig
    , cfVpcConfig

     -- * Destructuring the response
    , Types.FunctionConfiguration (..)
    , Types.mkFunctionConfiguration
    -- ** Response lenses
    , Types.fcCodeSha256
    , Types.fcCodeSize
    , Types.fcDeadLetterConfig
    , Types.fcDescription
    , Types.fcEnvironment
    , Types.fcFileSystemConfigs
    , Types.fcFunctionArn
    , Types.fcFunctionName
    , Types.fcHandler
    , Types.fcKMSKeyArn
    , Types.fcLastModified
    , Types.fcLastUpdateStatus
    , Types.fcLastUpdateStatusReason
    , Types.fcLastUpdateStatusReasonCode
    , Types.fcLayers
    , Types.fcMasterArn
    , Types.fcMemorySize
    , Types.fcRevisionId
    , Types.fcRole
    , Types.fcRuntime
    , Types.fcSigningJobArn
    , Types.fcSigningProfileVersionArn
    , Types.fcState
    , Types.fcStateReason
    , Types.fcStateReasonCode
    , Types.fcTimeout
    , Types.fcTracingConfig
    , Types.fcVersion
    , Types.fcVpcConfig
    ) where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateFunction' smart constructor.
data CreateFunction = CreateFunction'
  { functionName :: Types.FunctionName
    -- ^ The name of the Lambda function.
--
-- __Name formats__ 
--
--     * __Function name__ - @my-function@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
  , runtime :: Types.Runtime
    -- ^ The identifier of the function's <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime> .
  , role' :: Types.Role
    -- ^ The Amazon Resource Name (ARN) of the function's execution role.
  , handler :: Types.Handler
    -- ^ The name of the method within your code that Lambda calls to execute your function. The format includes the file name. It can also include namespaces and other qualifiers, depending on the runtime. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model> .
  , code :: Types.FunctionCode
    -- ^ The code for the function.
  , codeSigningConfigArn :: Core.Maybe Types.CodeSigningConfigArn
    -- ^ To enable code signing for this function, specify the ARN of a code-signing configuration. A code-signing configuration includes set set of signing profiles, which define the trusted publishers for this function.
  , deadLetterConfig :: Core.Maybe Types.DeadLetterConfig
    -- ^ A dead letter queue configuration that specifies the queue or topic where Lambda sends asynchronous events when they fail processing. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq Dead Letter Queues> .
  , description :: Core.Maybe Types.Description
    -- ^ A description of the function.
  , environment :: Core.Maybe Types.Environment
    -- ^ Environment variables that are accessible from function code during execution.
  , fileSystemConfigs :: Core.Maybe [Types.FileSystemConfig]
    -- ^ Connection settings for an Amazon EFS file system.
  , kMSKeyArn :: Core.Maybe Types.KMSKeyArn
    -- ^ The ARN of the AWS Key Management Service (AWS KMS) key that's used to encrypt your function's environment variables. If it's not provided, AWS Lambda uses a default service key.
  , layers :: Core.Maybe [Types.LayerVersionArn]
    -- ^ A list of <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers> to add to the function's execution environment. Specify each layer by its ARN, including the version.
  , memorySize :: Core.Maybe Core.Natural
    -- ^ The amount of memory that your function has access to. Increasing the function's memory also increases its CPU allocation. The default value is 128 MB. The value must be a multiple of 64 MB.
  , publish :: Core.Maybe Core.Bool
    -- ^ Set to true to publish the first version of the function during creation.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ A list of <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> to apply to the function.
  , timeout :: Core.Maybe Core.Natural
    -- ^ The amount of time that Lambda allows a function to run before stopping it. The default is 3 seconds. The maximum allowed value is 900 seconds.
  , tracingConfig :: Core.Maybe Types.TracingConfig
    -- ^ Set @Mode@ to @Active@ to sample and trace a subset of incoming requests with AWS X-Ray.
  , vpcConfig :: Core.Maybe Types.VpcConfig
    -- ^ For network connectivity to AWS resources in a VPC, specify a list of security groups and subnets in the VPC. When you connect a function to a VPC, it can only access resources and the internet through that VPC. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFunction' value with any optional fields omitted.
mkCreateFunction
    :: Types.FunctionName -- ^ 'functionName'
    -> Types.Runtime -- ^ 'runtime'
    -> Types.Role -- ^ 'role\''
    -> Types.Handler -- ^ 'handler'
    -> Types.FunctionCode -- ^ 'code'
    -> CreateFunction
mkCreateFunction functionName runtime role' handler code
  = CreateFunction'{functionName, runtime, role', handler, code,
                    codeSigningConfigArn = Core.Nothing,
                    deadLetterConfig = Core.Nothing, description = Core.Nothing,
                    environment = Core.Nothing, fileSystemConfigs = Core.Nothing,
                    kMSKeyArn = Core.Nothing, layers = Core.Nothing,
                    memorySize = Core.Nothing, publish = Core.Nothing,
                    tags = Core.Nothing, timeout = Core.Nothing,
                    tracingConfig = Core.Nothing, vpcConfig = Core.Nothing}

-- | The name of the Lambda function.
--
-- __Name formats__ 
--
--     * __Function name__ - @my-function@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfFunctionName :: Lens.Lens' CreateFunction Types.FunctionName
cfFunctionName = Lens.field @"functionName"
{-# INLINEABLE cfFunctionName #-}
{-# DEPRECATED functionName "Use generic-lens or generic-optics with 'functionName' instead"  #-}

-- | The identifier of the function's <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime> .
--
-- /Note:/ Consider using 'runtime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRuntime :: Lens.Lens' CreateFunction Types.Runtime
cfRuntime = Lens.field @"runtime"
{-# INLINEABLE cfRuntime #-}
{-# DEPRECATED runtime "Use generic-lens or generic-optics with 'runtime' instead"  #-}

-- | The Amazon Resource Name (ARN) of the function's execution role.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRole :: Lens.Lens' CreateFunction Types.Role
cfRole = Lens.field @"role'"
{-# INLINEABLE cfRole #-}
{-# DEPRECATED role' "Use generic-lens or generic-optics with 'role'' instead"  #-}

-- | The name of the method within your code that Lambda calls to execute your function. The format includes the file name. It can also include namespaces and other qualifiers, depending on the runtime. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model> .
--
-- /Note:/ Consider using 'handler' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfHandler :: Lens.Lens' CreateFunction Types.Handler
cfHandler = Lens.field @"handler"
{-# INLINEABLE cfHandler #-}
{-# DEPRECATED handler "Use generic-lens or generic-optics with 'handler' instead"  #-}

-- | The code for the function.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfCode :: Lens.Lens' CreateFunction Types.FunctionCode
cfCode = Lens.field @"code"
{-# INLINEABLE cfCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | To enable code signing for this function, specify the ARN of a code-signing configuration. A code-signing configuration includes set set of signing profiles, which define the trusted publishers for this function.
--
-- /Note:/ Consider using 'codeSigningConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfCodeSigningConfigArn :: Lens.Lens' CreateFunction (Core.Maybe Types.CodeSigningConfigArn)
cfCodeSigningConfigArn = Lens.field @"codeSigningConfigArn"
{-# INLINEABLE cfCodeSigningConfigArn #-}
{-# DEPRECATED codeSigningConfigArn "Use generic-lens or generic-optics with 'codeSigningConfigArn' instead"  #-}

-- | A dead letter queue configuration that specifies the queue or topic where Lambda sends asynchronous events when they fail processing. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq Dead Letter Queues> .
--
-- /Note:/ Consider using 'deadLetterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDeadLetterConfig :: Lens.Lens' CreateFunction (Core.Maybe Types.DeadLetterConfig)
cfDeadLetterConfig = Lens.field @"deadLetterConfig"
{-# INLINEABLE cfDeadLetterConfig #-}
{-# DEPRECATED deadLetterConfig "Use generic-lens or generic-optics with 'deadLetterConfig' instead"  #-}

-- | A description of the function.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDescription :: Lens.Lens' CreateFunction (Core.Maybe Types.Description)
cfDescription = Lens.field @"description"
{-# INLINEABLE cfDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Environment variables that are accessible from function code during execution.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfEnvironment :: Lens.Lens' CreateFunction (Core.Maybe Types.Environment)
cfEnvironment = Lens.field @"environment"
{-# INLINEABLE cfEnvironment #-}
{-# DEPRECATED environment "Use generic-lens or generic-optics with 'environment' instead"  #-}

-- | Connection settings for an Amazon EFS file system.
--
-- /Note:/ Consider using 'fileSystemConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfFileSystemConfigs :: Lens.Lens' CreateFunction (Core.Maybe [Types.FileSystemConfig])
cfFileSystemConfigs = Lens.field @"fileSystemConfigs"
{-# INLINEABLE cfFileSystemConfigs #-}
{-# DEPRECATED fileSystemConfigs "Use generic-lens or generic-optics with 'fileSystemConfigs' instead"  #-}

-- | The ARN of the AWS Key Management Service (AWS KMS) key that's used to encrypt your function's environment variables. If it's not provided, AWS Lambda uses a default service key.
--
-- /Note:/ Consider using 'kMSKeyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfKMSKeyArn :: Lens.Lens' CreateFunction (Core.Maybe Types.KMSKeyArn)
cfKMSKeyArn = Lens.field @"kMSKeyArn"
{-# INLINEABLE cfKMSKeyArn #-}
{-# DEPRECATED kMSKeyArn "Use generic-lens or generic-optics with 'kMSKeyArn' instead"  #-}

-- | A list of <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers> to add to the function's execution environment. Specify each layer by its ARN, including the version.
--
-- /Note:/ Consider using 'layers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLayers :: Lens.Lens' CreateFunction (Core.Maybe [Types.LayerVersionArn])
cfLayers = Lens.field @"layers"
{-# INLINEABLE cfLayers #-}
{-# DEPRECATED layers "Use generic-lens or generic-optics with 'layers' instead"  #-}

-- | The amount of memory that your function has access to. Increasing the function's memory also increases its CPU allocation. The default value is 128 MB. The value must be a multiple of 64 MB.
--
-- /Note:/ Consider using 'memorySize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfMemorySize :: Lens.Lens' CreateFunction (Core.Maybe Core.Natural)
cfMemorySize = Lens.field @"memorySize"
{-# INLINEABLE cfMemorySize #-}
{-# DEPRECATED memorySize "Use generic-lens or generic-optics with 'memorySize' instead"  #-}

-- | Set to true to publish the first version of the function during creation.
--
-- /Note:/ Consider using 'publish' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfPublish :: Lens.Lens' CreateFunction (Core.Maybe Core.Bool)
cfPublish = Lens.field @"publish"
{-# INLINEABLE cfPublish #-}
{-# DEPRECATED publish "Use generic-lens or generic-optics with 'publish' instead"  #-}

-- | A list of <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> to apply to the function.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTags :: Lens.Lens' CreateFunction (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
cfTags = Lens.field @"tags"
{-# INLINEABLE cfTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The amount of time that Lambda allows a function to run before stopping it. The default is 3 seconds. The maximum allowed value is 900 seconds.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTimeout :: Lens.Lens' CreateFunction (Core.Maybe Core.Natural)
cfTimeout = Lens.field @"timeout"
{-# INLINEABLE cfTimeout #-}
{-# DEPRECATED timeout "Use generic-lens or generic-optics with 'timeout' instead"  #-}

-- | Set @Mode@ to @Active@ to sample and trace a subset of incoming requests with AWS X-Ray.
--
-- /Note:/ Consider using 'tracingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTracingConfig :: Lens.Lens' CreateFunction (Core.Maybe Types.TracingConfig)
cfTracingConfig = Lens.field @"tracingConfig"
{-# INLINEABLE cfTracingConfig #-}
{-# DEPRECATED tracingConfig "Use generic-lens or generic-optics with 'tracingConfig' instead"  #-}

-- | For network connectivity to AWS resources in a VPC, specify a list of security groups and subnets in the VPC. When you connect a function to a VPC, it can only access resources and the internet through that VPC. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfVpcConfig :: Lens.Lens' CreateFunction (Core.Maybe Types.VpcConfig)
cfVpcConfig = Lens.field @"vpcConfig"
{-# INLINEABLE cfVpcConfig #-}
{-# DEPRECATED vpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead"  #-}

instance Core.ToQuery CreateFunction where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateFunction where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateFunction where
        toJSON CreateFunction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FunctionName" Core..= functionName),
                  Core.Just ("Runtime" Core..= runtime),
                  Core.Just ("Role" Core..= role'),
                  Core.Just ("Handler" Core..= handler),
                  Core.Just ("Code" Core..= code),
                  ("CodeSigningConfigArn" Core..=) Core.<$> codeSigningConfigArn,
                  ("DeadLetterConfig" Core..=) Core.<$> deadLetterConfig,
                  ("Description" Core..=) Core.<$> description,
                  ("Environment" Core..=) Core.<$> environment,
                  ("FileSystemConfigs" Core..=) Core.<$> fileSystemConfigs,
                  ("KMSKeyArn" Core..=) Core.<$> kMSKeyArn,
                  ("Layers" Core..=) Core.<$> layers,
                  ("MemorySize" Core..=) Core.<$> memorySize,
                  ("Publish" Core..=) Core.<$> publish,
                  ("Tags" Core..=) Core.<$> tags,
                  ("Timeout" Core..=) Core.<$> timeout,
                  ("TracingConfig" Core..=) Core.<$> tracingConfig,
                  ("VpcConfig" Core..=) Core.<$> vpcConfig])

instance Core.AWSRequest CreateFunction where
        type Rs CreateFunction = Types.FunctionConfiguration
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/2015-03-31/functions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
