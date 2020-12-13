{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateFunction (..),
    mkCreateFunction,

    -- ** Request lenses
    cfMemorySize,
    cfRuntime,
    cfKMSKeyARN,
    cfFileSystemConfigs,
    cfEnvironment,
    cfDeadLetterConfig,
    cfCodeSigningConfigARN,
    cfRole,
    cfVPCConfig,
    cfFunctionName,
    cfCode,
    cfLayers,
    cfHandler,
    cfTimeout,
    cfTracingConfig,
    cfDescription,
    cfTags,
    cfPublish,

    -- * Destructuring the response
    FunctionConfiguration (..),
    mkFunctionConfiguration,

    -- ** Response lenses
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

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateFunction' smart constructor.
data CreateFunction = CreateFunction'
  { -- | The amount of memory that your function has access to. Increasing the function's memory also increases its CPU allocation. The default value is 128 MB. The value must be a multiple of 64 MB.
    memorySize :: Lude.Maybe Lude.Natural,
    -- | The identifier of the function's <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime> .
    runtime :: Runtime,
    -- | The ARN of the AWS Key Management Service (AWS KMS) key that's used to encrypt your function's environment variables. If it's not provided, AWS Lambda uses a default service key.
    kmsKeyARN :: Lude.Maybe Lude.Text,
    -- | Connection settings for an Amazon EFS file system.
    fileSystemConfigs :: Lude.Maybe [FileSystemConfig],
    -- | Environment variables that are accessible from function code during execution.
    environment :: Lude.Maybe Environment,
    -- | A dead letter queue configuration that specifies the queue or topic where Lambda sends asynchronous events when they fail processing. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq Dead Letter Queues> .
    deadLetterConfig :: Lude.Maybe DeadLetterConfig,
    -- | To enable code signing for this function, specify the ARN of a code-signing configuration. A code-signing configuration includes set set of signing profiles, which define the trusted publishers for this function.
    codeSigningConfigARN :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the function's execution role.
    role' :: Lude.Text,
    -- | For network connectivity to AWS resources in a VPC, specify a list of security groups and subnets in the VPC. When you connect a function to a VPC, it can only access resources and the internet through that VPC. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings> .
    vpcConfig :: Lude.Maybe VPCConfig,
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
    functionName :: Lude.Text,
    -- | The code for the function.
    code :: FunctionCode,
    -- | A list of <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers> to add to the function's execution environment. Specify each layer by its ARN, including the version.
    layers :: Lude.Maybe [Lude.Text],
    -- | The name of the method within your code that Lambda calls to execute your function. The format includes the file name. It can also include namespaces and other qualifiers, depending on the runtime. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model> .
    handler :: Lude.Text,
    -- | The amount of time that Lambda allows a function to run before stopping it. The default is 3 seconds. The maximum allowed value is 900 seconds.
    timeout :: Lude.Maybe Lude.Natural,
    -- | Set @Mode@ to @Active@ to sample and trace a subset of incoming requests with AWS X-Ray.
    tracingConfig :: Lude.Maybe TracingConfig,
    -- | A description of the function.
    description :: Lude.Maybe Lude.Text,
    -- | A list of <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> to apply to the function.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Set to true to publish the first version of the function during creation.
    publish :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFunction' with the minimum fields required to make a request.
--
-- * 'memorySize' - The amount of memory that your function has access to. Increasing the function's memory also increases its CPU allocation. The default value is 128 MB. The value must be a multiple of 64 MB.
-- * 'runtime' - The identifier of the function's <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime> .
-- * 'kmsKeyARN' - The ARN of the AWS Key Management Service (AWS KMS) key that's used to encrypt your function's environment variables. If it's not provided, AWS Lambda uses a default service key.
-- * 'fileSystemConfigs' - Connection settings for an Amazon EFS file system.
-- * 'environment' - Environment variables that are accessible from function code during execution.
-- * 'deadLetterConfig' - A dead letter queue configuration that specifies the queue or topic where Lambda sends asynchronous events when they fail processing. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq Dead Letter Queues> .
-- * 'codeSigningConfigARN' - To enable code signing for this function, specify the ARN of a code-signing configuration. A code-signing configuration includes set set of signing profiles, which define the trusted publishers for this function.
-- * 'role'' - The Amazon Resource Name (ARN) of the function's execution role.
-- * 'vpcConfig' - For network connectivity to AWS resources in a VPC, specify a list of security groups and subnets in the VPC. When you connect a function to a VPC, it can only access resources and the internet through that VPC. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings> .
-- * 'functionName' - The name of the Lambda function.
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
-- * 'code' - The code for the function.
-- * 'layers' - A list of <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers> to add to the function's execution environment. Specify each layer by its ARN, including the version.
-- * 'handler' - The name of the method within your code that Lambda calls to execute your function. The format includes the file name. It can also include namespaces and other qualifiers, depending on the runtime. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model> .
-- * 'timeout' - The amount of time that Lambda allows a function to run before stopping it. The default is 3 seconds. The maximum allowed value is 900 seconds.
-- * 'tracingConfig' - Set @Mode@ to @Active@ to sample and trace a subset of incoming requests with AWS X-Ray.
-- * 'description' - A description of the function.
-- * 'tags' - A list of <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> to apply to the function.
-- * 'publish' - Set to true to publish the first version of the function during creation.
mkCreateFunction ::
  -- | 'runtime'
  Runtime ->
  -- | 'role''
  Lude.Text ->
  -- | 'functionName'
  Lude.Text ->
  -- | 'code'
  FunctionCode ->
  -- | 'handler'
  Lude.Text ->
  CreateFunction
mkCreateFunction pRuntime_ pRole_ pFunctionName_ pCode_ pHandler_ =
  CreateFunction'
    { memorySize = Lude.Nothing,
      runtime = pRuntime_,
      kmsKeyARN = Lude.Nothing,
      fileSystemConfigs = Lude.Nothing,
      environment = Lude.Nothing,
      deadLetterConfig = Lude.Nothing,
      codeSigningConfigARN = Lude.Nothing,
      role' = pRole_,
      vpcConfig = Lude.Nothing,
      functionName = pFunctionName_,
      code = pCode_,
      layers = Lude.Nothing,
      handler = pHandler_,
      timeout = Lude.Nothing,
      tracingConfig = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      publish = Lude.Nothing
    }

-- | The amount of memory that your function has access to. Increasing the function's memory also increases its CPU allocation. The default value is 128 MB. The value must be a multiple of 64 MB.
--
-- /Note:/ Consider using 'memorySize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfMemorySize :: Lens.Lens' CreateFunction (Lude.Maybe Lude.Natural)
cfMemorySize = Lens.lens (memorySize :: CreateFunction -> Lude.Maybe Lude.Natural) (\s a -> s {memorySize = a} :: CreateFunction)
{-# DEPRECATED cfMemorySize "Use generic-lens or generic-optics with 'memorySize' instead." #-}

-- | The identifier of the function's <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime> .
--
-- /Note:/ Consider using 'runtime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRuntime :: Lens.Lens' CreateFunction Runtime
cfRuntime = Lens.lens (runtime :: CreateFunction -> Runtime) (\s a -> s {runtime = a} :: CreateFunction)
{-# DEPRECATED cfRuntime "Use generic-lens or generic-optics with 'runtime' instead." #-}

-- | The ARN of the AWS Key Management Service (AWS KMS) key that's used to encrypt your function's environment variables. If it's not provided, AWS Lambda uses a default service key.
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfKMSKeyARN :: Lens.Lens' CreateFunction (Lude.Maybe Lude.Text)
cfKMSKeyARN = Lens.lens (kmsKeyARN :: CreateFunction -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyARN = a} :: CreateFunction)
{-# DEPRECATED cfKMSKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead." #-}

-- | Connection settings for an Amazon EFS file system.
--
-- /Note:/ Consider using 'fileSystemConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfFileSystemConfigs :: Lens.Lens' CreateFunction (Lude.Maybe [FileSystemConfig])
cfFileSystemConfigs = Lens.lens (fileSystemConfigs :: CreateFunction -> Lude.Maybe [FileSystemConfig]) (\s a -> s {fileSystemConfigs = a} :: CreateFunction)
{-# DEPRECATED cfFileSystemConfigs "Use generic-lens or generic-optics with 'fileSystemConfigs' instead." #-}

-- | Environment variables that are accessible from function code during execution.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfEnvironment :: Lens.Lens' CreateFunction (Lude.Maybe Environment)
cfEnvironment = Lens.lens (environment :: CreateFunction -> Lude.Maybe Environment) (\s a -> s {environment = a} :: CreateFunction)
{-# DEPRECATED cfEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | A dead letter queue configuration that specifies the queue or topic where Lambda sends asynchronous events when they fail processing. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq Dead Letter Queues> .
--
-- /Note:/ Consider using 'deadLetterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDeadLetterConfig :: Lens.Lens' CreateFunction (Lude.Maybe DeadLetterConfig)
cfDeadLetterConfig = Lens.lens (deadLetterConfig :: CreateFunction -> Lude.Maybe DeadLetterConfig) (\s a -> s {deadLetterConfig = a} :: CreateFunction)
{-# DEPRECATED cfDeadLetterConfig "Use generic-lens or generic-optics with 'deadLetterConfig' instead." #-}

-- | To enable code signing for this function, specify the ARN of a code-signing configuration. A code-signing configuration includes set set of signing profiles, which define the trusted publishers for this function.
--
-- /Note:/ Consider using 'codeSigningConfigARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfCodeSigningConfigARN :: Lens.Lens' CreateFunction (Lude.Maybe Lude.Text)
cfCodeSigningConfigARN = Lens.lens (codeSigningConfigARN :: CreateFunction -> Lude.Maybe Lude.Text) (\s a -> s {codeSigningConfigARN = a} :: CreateFunction)
{-# DEPRECATED cfCodeSigningConfigARN "Use generic-lens or generic-optics with 'codeSigningConfigARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the function's execution role.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRole :: Lens.Lens' CreateFunction Lude.Text
cfRole = Lens.lens (role' :: CreateFunction -> Lude.Text) (\s a -> s {role' = a} :: CreateFunction)
{-# DEPRECATED cfRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | For network connectivity to AWS resources in a VPC, specify a list of security groups and subnets in the VPC. When you connect a function to a VPC, it can only access resources and the internet through that VPC. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfVPCConfig :: Lens.Lens' CreateFunction (Lude.Maybe VPCConfig)
cfVPCConfig = Lens.lens (vpcConfig :: CreateFunction -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: CreateFunction)
{-# DEPRECATED cfVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

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
cfFunctionName :: Lens.Lens' CreateFunction Lude.Text
cfFunctionName = Lens.lens (functionName :: CreateFunction -> Lude.Text) (\s a -> s {functionName = a} :: CreateFunction)
{-# DEPRECATED cfFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | The code for the function.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfCode :: Lens.Lens' CreateFunction FunctionCode
cfCode = Lens.lens (code :: CreateFunction -> FunctionCode) (\s a -> s {code = a} :: CreateFunction)
{-# DEPRECATED cfCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | A list of <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers> to add to the function's execution environment. Specify each layer by its ARN, including the version.
--
-- /Note:/ Consider using 'layers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLayers :: Lens.Lens' CreateFunction (Lude.Maybe [Lude.Text])
cfLayers = Lens.lens (layers :: CreateFunction -> Lude.Maybe [Lude.Text]) (\s a -> s {layers = a} :: CreateFunction)
{-# DEPRECATED cfLayers "Use generic-lens or generic-optics with 'layers' instead." #-}

-- | The name of the method within your code that Lambda calls to execute your function. The format includes the file name. It can also include namespaces and other qualifiers, depending on the runtime. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model> .
--
-- /Note:/ Consider using 'handler' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfHandler :: Lens.Lens' CreateFunction Lude.Text
cfHandler = Lens.lens (handler :: CreateFunction -> Lude.Text) (\s a -> s {handler = a} :: CreateFunction)
{-# DEPRECATED cfHandler "Use generic-lens or generic-optics with 'handler' instead." #-}

-- | The amount of time that Lambda allows a function to run before stopping it. The default is 3 seconds. The maximum allowed value is 900 seconds.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTimeout :: Lens.Lens' CreateFunction (Lude.Maybe Lude.Natural)
cfTimeout = Lens.lens (timeout :: CreateFunction -> Lude.Maybe Lude.Natural) (\s a -> s {timeout = a} :: CreateFunction)
{-# DEPRECATED cfTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | Set @Mode@ to @Active@ to sample and trace a subset of incoming requests with AWS X-Ray.
--
-- /Note:/ Consider using 'tracingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTracingConfig :: Lens.Lens' CreateFunction (Lude.Maybe TracingConfig)
cfTracingConfig = Lens.lens (tracingConfig :: CreateFunction -> Lude.Maybe TracingConfig) (\s a -> s {tracingConfig = a} :: CreateFunction)
{-# DEPRECATED cfTracingConfig "Use generic-lens or generic-optics with 'tracingConfig' instead." #-}

-- | A description of the function.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDescription :: Lens.Lens' CreateFunction (Lude.Maybe Lude.Text)
cfDescription = Lens.lens (description :: CreateFunction -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateFunction)
{-# DEPRECATED cfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> to apply to the function.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTags :: Lens.Lens' CreateFunction (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cfTags = Lens.lens (tags :: CreateFunction -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateFunction)
{-# DEPRECATED cfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Set to true to publish the first version of the function during creation.
--
-- /Note:/ Consider using 'publish' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfPublish :: Lens.Lens' CreateFunction (Lude.Maybe Lude.Bool)
cfPublish = Lens.lens (publish :: CreateFunction -> Lude.Maybe Lude.Bool) (\s a -> s {publish = a} :: CreateFunction)
{-# DEPRECATED cfPublish "Use generic-lens or generic-optics with 'publish' instead." #-}

instance Lude.AWSRequest CreateFunction where
  type Rs CreateFunction = FunctionConfiguration
  request = Req.postJSON lambdaService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateFunction where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateFunction where
  toJSON CreateFunction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MemorySize" Lude..=) Lude.<$> memorySize,
            Lude.Just ("Runtime" Lude..= runtime),
            ("KMSKeyArn" Lude..=) Lude.<$> kmsKeyARN,
            ("FileSystemConfigs" Lude..=) Lude.<$> fileSystemConfigs,
            ("Environment" Lude..=) Lude.<$> environment,
            ("DeadLetterConfig" Lude..=) Lude.<$> deadLetterConfig,
            ("CodeSigningConfigArn" Lude..=) Lude.<$> codeSigningConfigARN,
            Lude.Just ("Role" Lude..= role'),
            ("VpcConfig" Lude..=) Lude.<$> vpcConfig,
            Lude.Just ("FunctionName" Lude..= functionName),
            Lude.Just ("Code" Lude..= code),
            ("Layers" Lude..=) Lude.<$> layers,
            Lude.Just ("Handler" Lude..= handler),
            ("Timeout" Lude..=) Lude.<$> timeout,
            ("TracingConfig" Lude..=) Lude.<$> tracingConfig,
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            ("Publish" Lude..=) Lude.<$> publish
          ]
      )

instance Lude.ToPath CreateFunction where
  toPath = Lude.const "/2015-03-31/functions"

instance Lude.ToQuery CreateFunction where
  toQuery = Lude.const Lude.mempty
