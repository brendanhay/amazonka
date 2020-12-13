{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.UpdateFunctionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify the version-specific settings of a Lambda function.
--
-- When you update a function, Lambda provisions an instance of the function and its supporting resources. If your function connects to a VPC, this process can take a minute. During this time, you can't modify the function, but you can still invoke it. The @LastUpdateStatus@ , @LastUpdateStatusReason@ , and @LastUpdateStatusReasonCode@ fields in the response from 'GetFunctionConfiguration' indicate when the update is complete and the function is processing events with the new configuration. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/functions-states.html Function States> .
-- These settings can vary between versions of a function and are locked when you publish a version. You can't modify the configuration of a published version, only the unpublished version.
-- To configure function concurrency, use 'PutFunctionConcurrency' . To grant invoke permissions to an account or AWS service, use 'AddPermission' .
module Network.AWS.Lambda.UpdateFunctionConfiguration
  ( -- * Creating a request
    UpdateFunctionConfiguration (..),
    mkUpdateFunctionConfiguration,

    -- ** Request lenses
    uMemorySize,
    uRuntime,
    uKMSKeyARN,
    uFileSystemConfigs,
    uEnvironment,
    uDeadLetterConfig,
    uRole,
    uVPCConfig,
    uFunctionName,
    uLayers,
    uHandler,
    uTimeout,
    uTracingConfig,
    uDescription,
    uRevisionId,

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

-- | /See:/ 'mkUpdateFunctionConfiguration' smart constructor.
data UpdateFunctionConfiguration = UpdateFunctionConfiguration'
  { -- | The amount of memory that your function has access to. Increasing the function's memory also increases its CPU allocation. The default value is 128 MB. The value must be a multiple of 64 MB.
    memorySize :: Lude.Maybe Lude.Natural,
    -- | The identifier of the function's <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime> .
    runtime :: Lude.Maybe Runtime,
    -- | The ARN of the AWS Key Management Service (AWS KMS) key that's used to encrypt your function's environment variables. If it's not provided, AWS Lambda uses a default service key.
    kmsKeyARN :: Lude.Maybe Lude.Text,
    -- | Connection settings for an Amazon EFS file system.
    fileSystemConfigs :: Lude.Maybe [FileSystemConfig],
    -- | Environment variables that are accessible from function code during execution.
    environment :: Lude.Maybe Environment,
    -- | A dead letter queue configuration that specifies the queue or topic where Lambda sends asynchronous events when they fail processing. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq Dead Letter Queues> .
    deadLetterConfig :: Lude.Maybe DeadLetterConfig,
    -- | The Amazon Resource Name (ARN) of the function's execution role.
    role' :: Lude.Maybe Lude.Text,
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
    -- | A list of <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers> to add to the function's execution environment. Specify each layer by its ARN, including the version.
    layers :: Lude.Maybe [Lude.Text],
    -- | The name of the method within your code that Lambda calls to execute your function. The format includes the file name. It can also include namespaces and other qualifiers, depending on the runtime. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model> .
    handler :: Lude.Maybe Lude.Text,
    -- | The amount of time that Lambda allows a function to run before stopping it. The default is 3 seconds. The maximum allowed value is 900 seconds.
    timeout :: Lude.Maybe Lude.Natural,
    -- | Set @Mode@ to @Active@ to sample and trace a subset of incoming requests with AWS X-Ray.
    tracingConfig :: Lude.Maybe TracingConfig,
    -- | A description of the function.
    description :: Lude.Maybe Lude.Text,
    -- | Only update the function if the revision ID matches the ID that's specified. Use this option to avoid modifying a function that has changed since you last read it.
    revisionId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFunctionConfiguration' with the minimum fields required to make a request.
--
-- * 'memorySize' - The amount of memory that your function has access to. Increasing the function's memory also increases its CPU allocation. The default value is 128 MB. The value must be a multiple of 64 MB.
-- * 'runtime' - The identifier of the function's <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime> .
-- * 'kmsKeyARN' - The ARN of the AWS Key Management Service (AWS KMS) key that's used to encrypt your function's environment variables. If it's not provided, AWS Lambda uses a default service key.
-- * 'fileSystemConfigs' - Connection settings for an Amazon EFS file system.
-- * 'environment' - Environment variables that are accessible from function code during execution.
-- * 'deadLetterConfig' - A dead letter queue configuration that specifies the queue or topic where Lambda sends asynchronous events when they fail processing. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq Dead Letter Queues> .
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
-- * 'layers' - A list of <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers> to add to the function's execution environment. Specify each layer by its ARN, including the version.
-- * 'handler' - The name of the method within your code that Lambda calls to execute your function. The format includes the file name. It can also include namespaces and other qualifiers, depending on the runtime. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model> .
-- * 'timeout' - The amount of time that Lambda allows a function to run before stopping it. The default is 3 seconds. The maximum allowed value is 900 seconds.
-- * 'tracingConfig' - Set @Mode@ to @Active@ to sample and trace a subset of incoming requests with AWS X-Ray.
-- * 'description' - A description of the function.
-- * 'revisionId' - Only update the function if the revision ID matches the ID that's specified. Use this option to avoid modifying a function that has changed since you last read it.
mkUpdateFunctionConfiguration ::
  -- | 'functionName'
  Lude.Text ->
  UpdateFunctionConfiguration
mkUpdateFunctionConfiguration pFunctionName_ =
  UpdateFunctionConfiguration'
    { memorySize = Lude.Nothing,
      runtime = Lude.Nothing,
      kmsKeyARN = Lude.Nothing,
      fileSystemConfigs = Lude.Nothing,
      environment = Lude.Nothing,
      deadLetterConfig = Lude.Nothing,
      role' = Lude.Nothing,
      vpcConfig = Lude.Nothing,
      functionName = pFunctionName_,
      layers = Lude.Nothing,
      handler = Lude.Nothing,
      timeout = Lude.Nothing,
      tracingConfig = Lude.Nothing,
      description = Lude.Nothing,
      revisionId = Lude.Nothing
    }

-- | The amount of memory that your function has access to. Increasing the function's memory also increases its CPU allocation. The default value is 128 MB. The value must be a multiple of 64 MB.
--
-- /Note:/ Consider using 'memorySize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uMemorySize :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe Lude.Natural)
uMemorySize = Lens.lens (memorySize :: UpdateFunctionConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {memorySize = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED uMemorySize "Use generic-lens or generic-optics with 'memorySize' instead." #-}

-- | The identifier of the function's <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime> .
--
-- /Note:/ Consider using 'runtime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uRuntime :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe Runtime)
uRuntime = Lens.lens (runtime :: UpdateFunctionConfiguration -> Lude.Maybe Runtime) (\s a -> s {runtime = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED uRuntime "Use generic-lens or generic-optics with 'runtime' instead." #-}

-- | The ARN of the AWS Key Management Service (AWS KMS) key that's used to encrypt your function's environment variables. If it's not provided, AWS Lambda uses a default service key.
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uKMSKeyARN :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe Lude.Text)
uKMSKeyARN = Lens.lens (kmsKeyARN :: UpdateFunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyARN = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED uKMSKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead." #-}

-- | Connection settings for an Amazon EFS file system.
--
-- /Note:/ Consider using 'fileSystemConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uFileSystemConfigs :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe [FileSystemConfig])
uFileSystemConfigs = Lens.lens (fileSystemConfigs :: UpdateFunctionConfiguration -> Lude.Maybe [FileSystemConfig]) (\s a -> s {fileSystemConfigs = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED uFileSystemConfigs "Use generic-lens or generic-optics with 'fileSystemConfigs' instead." #-}

-- | Environment variables that are accessible from function code during execution.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uEnvironment :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe Environment)
uEnvironment = Lens.lens (environment :: UpdateFunctionConfiguration -> Lude.Maybe Environment) (\s a -> s {environment = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED uEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | A dead letter queue configuration that specifies the queue or topic where Lambda sends asynchronous events when they fail processing. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq Dead Letter Queues> .
--
-- /Note:/ Consider using 'deadLetterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDeadLetterConfig :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe DeadLetterConfig)
uDeadLetterConfig = Lens.lens (deadLetterConfig :: UpdateFunctionConfiguration -> Lude.Maybe DeadLetterConfig) (\s a -> s {deadLetterConfig = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED uDeadLetterConfig "Use generic-lens or generic-optics with 'deadLetterConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the function's execution role.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uRole :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe Lude.Text)
uRole = Lens.lens (role' :: UpdateFunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {role' = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED uRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | For network connectivity to AWS resources in a VPC, specify a list of security groups and subnets in the VPC. When you connect a function to a VPC, it can only access resources and the internet through that VPC. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uVPCConfig :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe VPCConfig)
uVPCConfig = Lens.lens (vpcConfig :: UpdateFunctionConfiguration -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED uVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

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
uFunctionName :: Lens.Lens' UpdateFunctionConfiguration Lude.Text
uFunctionName = Lens.lens (functionName :: UpdateFunctionConfiguration -> Lude.Text) (\s a -> s {functionName = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED uFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | A list of <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers> to add to the function's execution environment. Specify each layer by its ARN, including the version.
--
-- /Note:/ Consider using 'layers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uLayers :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe [Lude.Text])
uLayers = Lens.lens (layers :: UpdateFunctionConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {layers = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED uLayers "Use generic-lens or generic-optics with 'layers' instead." #-}

-- | The name of the method within your code that Lambda calls to execute your function. The format includes the file name. It can also include namespaces and other qualifiers, depending on the runtime. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model> .
--
-- /Note:/ Consider using 'handler' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uHandler :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe Lude.Text)
uHandler = Lens.lens (handler :: UpdateFunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {handler = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED uHandler "Use generic-lens or generic-optics with 'handler' instead." #-}

-- | The amount of time that Lambda allows a function to run before stopping it. The default is 3 seconds. The maximum allowed value is 900 seconds.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uTimeout :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe Lude.Natural)
uTimeout = Lens.lens (timeout :: UpdateFunctionConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {timeout = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED uTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | Set @Mode@ to @Active@ to sample and trace a subset of incoming requests with AWS X-Ray.
--
-- /Note:/ Consider using 'tracingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uTracingConfig :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe TracingConfig)
uTracingConfig = Lens.lens (tracingConfig :: UpdateFunctionConfiguration -> Lude.Maybe TracingConfig) (\s a -> s {tracingConfig = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED uTracingConfig "Use generic-lens or generic-optics with 'tracingConfig' instead." #-}

-- | A description of the function.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDescription :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe Lude.Text)
uDescription = Lens.lens (description :: UpdateFunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED uDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Only update the function if the revision ID matches the ID that's specified. Use this option to avoid modifying a function that has changed since you last read it.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uRevisionId :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe Lude.Text)
uRevisionId = Lens.lens (revisionId :: UpdateFunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {revisionId = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED uRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

instance Lude.AWSRequest UpdateFunctionConfiguration where
  type Rs UpdateFunctionConfiguration = FunctionConfiguration
  request = Req.putJSON lambdaService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateFunctionConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateFunctionConfiguration where
  toJSON UpdateFunctionConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MemorySize" Lude..=) Lude.<$> memorySize,
            ("Runtime" Lude..=) Lude.<$> runtime,
            ("KMSKeyArn" Lude..=) Lude.<$> kmsKeyARN,
            ("FileSystemConfigs" Lude..=) Lude.<$> fileSystemConfigs,
            ("Environment" Lude..=) Lude.<$> environment,
            ("DeadLetterConfig" Lude..=) Lude.<$> deadLetterConfig,
            ("Role" Lude..=) Lude.<$> role',
            ("VpcConfig" Lude..=) Lude.<$> vpcConfig,
            ("Layers" Lude..=) Lude.<$> layers,
            ("Handler" Lude..=) Lude.<$> handler,
            ("Timeout" Lude..=) Lude.<$> timeout,
            ("TracingConfig" Lude..=) Lude.<$> tracingConfig,
            ("Description" Lude..=) Lude.<$> description,
            ("RevisionId" Lude..=) Lude.<$> revisionId
          ]
      )

instance Lude.ToPath UpdateFunctionConfiguration where
  toPath UpdateFunctionConfiguration' {..} =
    Lude.mconcat
      [ "/2015-03-31/functions/",
        Lude.toBS functionName,
        "/configuration"
      ]

instance Lude.ToQuery UpdateFunctionConfiguration where
  toQuery = Lude.const Lude.mempty
