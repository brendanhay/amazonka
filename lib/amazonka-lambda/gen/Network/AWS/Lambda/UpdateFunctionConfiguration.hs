{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ufcMemorySize,
    ufcRuntime,
    ufcKMSKeyARN,
    ufcFileSystemConfigs,
    ufcEnvironment,
    ufcDeadLetterConfig,
    ufcRole,
    ufcVPCConfig,
    ufcLayers,
    ufcHandler,
    ufcTimeout,
    ufcTracingConfig,
    ufcDescription,
    ufcRevisionId,
    ufcFunctionName,

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
  { memorySize ::
      Lude.Maybe Lude.Natural,
    runtime :: Lude.Maybe Runtime,
    kmsKeyARN :: Lude.Maybe Lude.Text,
    fileSystemConfigs ::
      Lude.Maybe [FileSystemConfig],
    environment ::
      Lude.Maybe Environment,
    deadLetterConfig ::
      Lude.Maybe DeadLetterConfig,
    role' :: Lude.Maybe Lude.Text,
    vpcConfig :: Lude.Maybe VPCConfig,
    layers :: Lude.Maybe [Lude.Text],
    handler :: Lude.Maybe Lude.Text,
    timeout :: Lude.Maybe Lude.Natural,
    tracingConfig ::
      Lude.Maybe TracingConfig,
    description :: Lude.Maybe Lude.Text,
    revisionId :: Lude.Maybe Lude.Text,
    functionName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFunctionConfiguration' with the minimum fields required to make a request.
--
-- * 'deadLetterConfig' - A dead letter queue configuration that specifies the queue or topic where Lambda sends asynchronous events when they fail processing. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq Dead Letter Queues> .
-- * 'description' - A description of the function.
-- * 'environment' - Environment variables that are accessible from function code during execution.
-- * 'fileSystemConfigs' - Connection settings for an Amazon EFS file system.
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
-- * 'handler' - The name of the method within your code that Lambda calls to execute your function. The format includes the file name. It can also include namespaces and other qualifiers, depending on the runtime. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model> .
-- * 'kmsKeyARN' - The ARN of the AWS Key Management Service (AWS KMS) key that's used to encrypt your function's environment variables. If it's not provided, AWS Lambda uses a default service key.
-- * 'layers' - A list of <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers> to add to the function's execution environment. Specify each layer by its ARN, including the version.
-- * 'memorySize' - The amount of memory that your function has access to. Increasing the function's memory also increases its CPU allocation. The default value is 128 MB. The value must be a multiple of 64 MB.
-- * 'revisionId' - Only update the function if the revision ID matches the ID that's specified. Use this option to avoid modifying a function that has changed since you last read it.
-- * 'role'' - The Amazon Resource Name (ARN) of the function's execution role.
-- * 'runtime' - The identifier of the function's <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime> .
-- * 'timeout' - The amount of time that Lambda allows a function to run before stopping it. The default is 3 seconds. The maximum allowed value is 900 seconds.
-- * 'tracingConfig' - Set @Mode@ to @Active@ to sample and trace a subset of incoming requests with AWS X-Ray.
-- * 'vpcConfig' - For network connectivity to AWS resources in a VPC, specify a list of security groups and subnets in the VPC. When you connect a function to a VPC, it can only access resources and the internet through that VPC. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings> .
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
      layers = Lude.Nothing,
      handler = Lude.Nothing,
      timeout = Lude.Nothing,
      tracingConfig = Lude.Nothing,
      description = Lude.Nothing,
      revisionId = Lude.Nothing,
      functionName = pFunctionName_
    }

-- | The amount of memory that your function has access to. Increasing the function's memory also increases its CPU allocation. The default value is 128 MB. The value must be a multiple of 64 MB.
--
-- /Note:/ Consider using 'memorySize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcMemorySize :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe Lude.Natural)
ufcMemorySize = Lens.lens (memorySize :: UpdateFunctionConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {memorySize = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED ufcMemorySize "Use generic-lens or generic-optics with 'memorySize' instead." #-}

-- | The identifier of the function's <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime> .
--
-- /Note:/ Consider using 'runtime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcRuntime :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe Runtime)
ufcRuntime = Lens.lens (runtime :: UpdateFunctionConfiguration -> Lude.Maybe Runtime) (\s a -> s {runtime = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED ufcRuntime "Use generic-lens or generic-optics with 'runtime' instead." #-}

-- | The ARN of the AWS Key Management Service (AWS KMS) key that's used to encrypt your function's environment variables. If it's not provided, AWS Lambda uses a default service key.
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcKMSKeyARN :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe Lude.Text)
ufcKMSKeyARN = Lens.lens (kmsKeyARN :: UpdateFunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyARN = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED ufcKMSKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead." #-}

-- | Connection settings for an Amazon EFS file system.
--
-- /Note:/ Consider using 'fileSystemConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcFileSystemConfigs :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe [FileSystemConfig])
ufcFileSystemConfigs = Lens.lens (fileSystemConfigs :: UpdateFunctionConfiguration -> Lude.Maybe [FileSystemConfig]) (\s a -> s {fileSystemConfigs = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED ufcFileSystemConfigs "Use generic-lens or generic-optics with 'fileSystemConfigs' instead." #-}

-- | Environment variables that are accessible from function code during execution.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcEnvironment :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe Environment)
ufcEnvironment = Lens.lens (environment :: UpdateFunctionConfiguration -> Lude.Maybe Environment) (\s a -> s {environment = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED ufcEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | A dead letter queue configuration that specifies the queue or topic where Lambda sends asynchronous events when they fail processing. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq Dead Letter Queues> .
--
-- /Note:/ Consider using 'deadLetterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcDeadLetterConfig :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe DeadLetterConfig)
ufcDeadLetterConfig = Lens.lens (deadLetterConfig :: UpdateFunctionConfiguration -> Lude.Maybe DeadLetterConfig) (\s a -> s {deadLetterConfig = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED ufcDeadLetterConfig "Use generic-lens or generic-optics with 'deadLetterConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the function's execution role.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcRole :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe Lude.Text)
ufcRole = Lens.lens (role' :: UpdateFunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {role' = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED ufcRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | For network connectivity to AWS resources in a VPC, specify a list of security groups and subnets in the VPC. When you connect a function to a VPC, it can only access resources and the internet through that VPC. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcVPCConfig :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe VPCConfig)
ufcVPCConfig = Lens.lens (vpcConfig :: UpdateFunctionConfiguration -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED ufcVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | A list of <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers> to add to the function's execution environment. Specify each layer by its ARN, including the version.
--
-- /Note:/ Consider using 'layers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcLayers :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe [Lude.Text])
ufcLayers = Lens.lens (layers :: UpdateFunctionConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {layers = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED ufcLayers "Use generic-lens or generic-optics with 'layers' instead." #-}

-- | The name of the method within your code that Lambda calls to execute your function. The format includes the file name. It can also include namespaces and other qualifiers, depending on the runtime. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model> .
--
-- /Note:/ Consider using 'handler' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcHandler :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe Lude.Text)
ufcHandler = Lens.lens (handler :: UpdateFunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {handler = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED ufcHandler "Use generic-lens or generic-optics with 'handler' instead." #-}

-- | The amount of time that Lambda allows a function to run before stopping it. The default is 3 seconds. The maximum allowed value is 900 seconds.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcTimeout :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe Lude.Natural)
ufcTimeout = Lens.lens (timeout :: UpdateFunctionConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {timeout = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED ufcTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | Set @Mode@ to @Active@ to sample and trace a subset of incoming requests with AWS X-Ray.
--
-- /Note:/ Consider using 'tracingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcTracingConfig :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe TracingConfig)
ufcTracingConfig = Lens.lens (tracingConfig :: UpdateFunctionConfiguration -> Lude.Maybe TracingConfig) (\s a -> s {tracingConfig = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED ufcTracingConfig "Use generic-lens or generic-optics with 'tracingConfig' instead." #-}

-- | A description of the function.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcDescription :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe Lude.Text)
ufcDescription = Lens.lens (description :: UpdateFunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED ufcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Only update the function if the revision ID matches the ID that's specified. Use this option to avoid modifying a function that has changed since you last read it.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcRevisionId :: Lens.Lens' UpdateFunctionConfiguration (Lude.Maybe Lude.Text)
ufcRevisionId = Lens.lens (revisionId :: UpdateFunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {revisionId = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED ufcRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

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
ufcFunctionName :: Lens.Lens' UpdateFunctionConfiguration Lude.Text
ufcFunctionName = Lens.lens (functionName :: UpdateFunctionConfiguration -> Lude.Text) (\s a -> s {functionName = a} :: UpdateFunctionConfiguration)
{-# DEPRECATED ufcFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

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
