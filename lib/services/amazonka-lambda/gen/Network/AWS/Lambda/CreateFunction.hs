{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.CreateFunction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Lambda function. To create a function, you need a
-- <https://docs.aws.amazon.com/lambda/latest/dg/gettingstarted-package.html deployment package>
-- and an
-- <https://docs.aws.amazon.com/lambda/latest/dg/intro-permission-model.html#lambda-intro-execution-role execution role>.
-- The deployment package is a .zip file archive or container image that
-- contains your function code. The execution role grants the function
-- permission to use Amazon Web Services services, such as Amazon
-- CloudWatch Logs for log streaming and X-Ray for request tracing.
--
-- You set the package type to @Image@ if the deployment package is a
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-images.html container image>.
-- For a container image, the code property must include the URI of a
-- container image in the Amazon ECR registry. You do not need to specify
-- the handler and runtime properties.
--
-- You set the package type to @Zip@ if the deployment package is a
-- <https://docs.aws.amazon.com/lambda/latest/dg/gettingstarted-package.html#gettingstarted-package-zip .zip file archive>.
-- For a .zip file archive, the code property specifies the location of the
-- .zip file. You must also specify the handler and runtime properties. The
-- code in the deployment package must be compatible with the target
-- instruction set architecture of the function (@x86-64@ or @arm64@). If
-- you do not specify the architecture, the default value is @x86-64@.
--
-- When you create a function, Lambda provisions an instance of the
-- function and its supporting resources. If your function connects to a
-- VPC, this process can take a minute or so. During this time, you can\'t
-- invoke or modify the function. The @State@, @StateReason@, and
-- @StateReasonCode@ fields in the response from GetFunctionConfiguration
-- indicate when the function is ready to invoke. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/functions-states.html Function States>.
--
-- A function has an unpublished version, and can have published versions
-- and aliases. The unpublished version changes when you update your
-- function\'s code and configuration. A published version is a snapshot of
-- your function code and configuration that can\'t be changed. An alias is
-- a named resource that maps to a version, and can be changed to map to a
-- different version. Use the @Publish@ parameter to create version @1@ of
-- your function from its initial configuration.
--
-- The other parameters let you configure version-specific and
-- function-level settings. You can modify version-specific settings later
-- with UpdateFunctionConfiguration. Function-level settings apply to both
-- the unpublished and published versions of the function, and include tags
-- (TagResource) and per-function concurrency limits
-- (PutFunctionConcurrency).
--
-- You can use code signing if your deployment package is a .zip file
-- archive. To enable code signing for this function, specify the ARN of a
-- code-signing configuration. When a user attempts to deploy a code
-- package with UpdateFunctionCode, Lambda checks that the code package has
-- a valid signature from a trusted publisher. The code-signing
-- configuration includes set set of signing profiles, which define the
-- trusted publishers for this function.
--
-- If another account or an Amazon Web Services service invokes your
-- function, use AddPermission to grant permission by creating a
-- resource-based IAM policy. You can grant permissions at the function
-- level, on a version, or on an alias.
--
-- To invoke your function directly, use Invoke. To invoke your function in
-- response to events in other Amazon Web Services services, create an
-- event source mapping (CreateEventSourceMapping), or configure a function
-- trigger in the other service. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-invocation.html Invoking Functions>.
module Network.AWS.Lambda.CreateFunction
  ( -- * Creating a Request
    CreateFunction (..),
    newCreateFunction,

    -- * Request Lenses
    createFunction_memorySize,
    createFunction_runtime,
    createFunction_kmsKeyArn,
    createFunction_packageType,
    createFunction_fileSystemConfigs,
    createFunction_environment,
    createFunction_imageConfig,
    createFunction_deadLetterConfig,
    createFunction_architectures,
    createFunction_codeSigningConfigArn,
    createFunction_vpcConfig,
    createFunction_layers,
    createFunction_handler,
    createFunction_timeout,
    createFunction_tracingConfig,
    createFunction_description,
    createFunction_tags,
    createFunction_publish,
    createFunction_functionName,
    createFunction_role,
    createFunction_code,

    -- * Destructuring the Response
    FunctionConfiguration (..),
    newFunctionConfiguration,

    -- * Response Lenses
    functionConfiguration_memorySize,
    functionConfiguration_runtime,
    functionConfiguration_state,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_functionArn,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_packageType,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_environment,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_architectures,
    functionConfiguration_signingJobArn,
    functionConfiguration_role,
    functionConfiguration_vpcConfig,
    functionConfiguration_version,
    functionConfiguration_functionName,
    functionConfiguration_layers,
    functionConfiguration_codeSize,
    functionConfiguration_handler,
    functionConfiguration_timeout,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_stateReason,
    functionConfiguration_lastModified,
    functionConfiguration_codeSha256,
    functionConfiguration_tracingConfig,
    functionConfiguration_stateReasonCode,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_description,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_revisionId,
    functionConfiguration_masterArn,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateFunction' smart constructor.
data CreateFunction = CreateFunction'
  { -- | The amount of
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-memory.html memory available to the function>
    -- at runtime. Increasing the function memory also increases its CPU
    -- allocation. The default value is 128 MB. The value can be any multiple
    -- of 1 MB.
    memorySize :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the function\'s
    -- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime>.
    runtime :: Prelude.Maybe Runtime,
    -- | The ARN of the Amazon Web Services Key Management Service (KMS) key
    -- that\'s used to encrypt your function\'s environment variables. If it\'s
    -- not provided, Lambda uses a default service key.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The type of deployment package. Set to @Image@ for container image and
    -- set @Zip@ for ZIP archive.
    packageType :: Prelude.Maybe PackageType,
    -- | Connection settings for an Amazon EFS file system.
    fileSystemConfigs :: Prelude.Maybe [FileSystemConfig],
    -- | Environment variables that are accessible from function code during
    -- execution.
    environment :: Prelude.Maybe Environment,
    -- | Container image
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-images.html#configuration-images-settings configuration values>
    -- that override the values in the container image Dockerfile.
    imageConfig :: Prelude.Maybe ImageConfig,
    -- | A dead letter queue configuration that specifies the queue or topic
    -- where Lambda sends asynchronous events when they fail processing. For
    -- more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq Dead Letter Queues>.
    deadLetterConfig :: Prelude.Maybe DeadLetterConfig,
    -- | The instruction set architecture that the function supports. Enter a
    -- string array with one of the valid values. The default value is
    -- @x86_64@.
    architectures :: Prelude.Maybe (Prelude.NonEmpty Architecture),
    -- | To enable code signing for this function, specify the ARN of a
    -- code-signing configuration. A code-signing configuration includes a set
    -- of signing profiles, which define the trusted publishers for this
    -- function.
    codeSigningConfigArn :: Prelude.Maybe Prelude.Text,
    -- | For network connectivity to Amazon Web Services resources in a VPC,
    -- specify a list of security groups and subnets in the VPC. When you
    -- connect a function to a VPC, it can only access resources and the
    -- internet through that VPC. For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | A list of
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers>
    -- to add to the function\'s execution environment. Specify each layer by
    -- its ARN, including the version.
    layers :: Prelude.Maybe [Prelude.Text],
    -- | The name of the method within your code that Lambda calls to execute
    -- your function. The format includes the file name. It can also include
    -- namespaces and other qualifiers, depending on the runtime. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model>.
    handler :: Prelude.Maybe Prelude.Text,
    -- | The amount of time that Lambda allows a function to run before stopping
    -- it. The default is 3 seconds. The maximum allowed value is 900 seconds.
    -- For additional information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-context.html Lambda execution environment>.
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | Set @Mode@ to @Active@ to sample and trace a subset of incoming requests
    -- with
    -- <https://docs.aws.amazon.com/lambda/latest/dg/services-xray.html X-Ray>.
    tracingConfig :: Prelude.Maybe TracingConfig,
    -- | A description of the function.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of
    -- <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> to
    -- apply to the function.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Set to true to publish the first version of the function during
    -- creation.
    publish :: Prelude.Maybe Prelude.Bool,
    -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @my-function@.
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ - @123456789012:function:my-function@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the function\'s execution role.
    role' :: Prelude.Text,
    -- | The code for the function.
    code :: FunctionCode
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memorySize', 'createFunction_memorySize' - The amount of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-memory.html memory available to the function>
-- at runtime. Increasing the function memory also increases its CPU
-- allocation. The default value is 128 MB. The value can be any multiple
-- of 1 MB.
--
-- 'runtime', 'createFunction_runtime' - The identifier of the function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime>.
--
-- 'kmsKeyArn', 'createFunction_kmsKeyArn' - The ARN of the Amazon Web Services Key Management Service (KMS) key
-- that\'s used to encrypt your function\'s environment variables. If it\'s
-- not provided, Lambda uses a default service key.
--
-- 'packageType', 'createFunction_packageType' - The type of deployment package. Set to @Image@ for container image and
-- set @Zip@ for ZIP archive.
--
-- 'fileSystemConfigs', 'createFunction_fileSystemConfigs' - Connection settings for an Amazon EFS file system.
--
-- 'environment', 'createFunction_environment' - Environment variables that are accessible from function code during
-- execution.
--
-- 'imageConfig', 'createFunction_imageConfig' - Container image
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-images.html#configuration-images-settings configuration values>
-- that override the values in the container image Dockerfile.
--
-- 'deadLetterConfig', 'createFunction_deadLetterConfig' - A dead letter queue configuration that specifies the queue or topic
-- where Lambda sends asynchronous events when they fail processing. For
-- more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq Dead Letter Queues>.
--
-- 'architectures', 'createFunction_architectures' - The instruction set architecture that the function supports. Enter a
-- string array with one of the valid values. The default value is
-- @x86_64@.
--
-- 'codeSigningConfigArn', 'createFunction_codeSigningConfigArn' - To enable code signing for this function, specify the ARN of a
-- code-signing configuration. A code-signing configuration includes a set
-- of signing profiles, which define the trusted publishers for this
-- function.
--
-- 'vpcConfig', 'createFunction_vpcConfig' - For network connectivity to Amazon Web Services resources in a VPC,
-- specify a list of security groups and subnets in the VPC. When you
-- connect a function to a VPC, it can only access resources and the
-- internet through that VPC. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings>.
--
-- 'layers', 'createFunction_layers' - A list of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers>
-- to add to the function\'s execution environment. Specify each layer by
-- its ARN, including the version.
--
-- 'handler', 'createFunction_handler' - The name of the method within your code that Lambda calls to execute
-- your function. The format includes the file name. It can also include
-- namespaces and other qualifiers, depending on the runtime. For more
-- information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model>.
--
-- 'timeout', 'createFunction_timeout' - The amount of time that Lambda allows a function to run before stopping
-- it. The default is 3 seconds. The maximum allowed value is 900 seconds.
-- For additional information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-context.html Lambda execution environment>.
--
-- 'tracingConfig', 'createFunction_tracingConfig' - Set @Mode@ to @Active@ to sample and trace a subset of incoming requests
-- with
-- <https://docs.aws.amazon.com/lambda/latest/dg/services-xray.html X-Ray>.
--
-- 'description', 'createFunction_description' - A description of the function.
--
-- 'tags', 'createFunction_tags' - A list of
-- <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> to
-- apply to the function.
--
-- 'publish', 'createFunction_publish' - Set to true to publish the first version of the function during
-- creation.
--
-- 'functionName', 'createFunction_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
--
-- 'role'', 'createFunction_role' - The Amazon Resource Name (ARN) of the function\'s execution role.
--
-- 'code', 'createFunction_code' - The code for the function.
newCreateFunction ::
  -- | 'functionName'
  Prelude.Text ->
  -- | 'role''
  Prelude.Text ->
  -- | 'code'
  FunctionCode ->
  CreateFunction
newCreateFunction pFunctionName_ pRole_ pCode_ =
  CreateFunction'
    { memorySize = Prelude.Nothing,
      runtime = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      packageType = Prelude.Nothing,
      fileSystemConfigs = Prelude.Nothing,
      environment = Prelude.Nothing,
      imageConfig = Prelude.Nothing,
      deadLetterConfig = Prelude.Nothing,
      architectures = Prelude.Nothing,
      codeSigningConfigArn = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      layers = Prelude.Nothing,
      handler = Prelude.Nothing,
      timeout = Prelude.Nothing,
      tracingConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      publish = Prelude.Nothing,
      functionName = pFunctionName_,
      role' = pRole_,
      code = pCode_
    }

-- | The amount of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-memory.html memory available to the function>
-- at runtime. Increasing the function memory also increases its CPU
-- allocation. The default value is 128 MB. The value can be any multiple
-- of 1 MB.
createFunction_memorySize :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Natural)
createFunction_memorySize = Lens.lens (\CreateFunction' {memorySize} -> memorySize) (\s@CreateFunction' {} a -> s {memorySize = a} :: CreateFunction)

-- | The identifier of the function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime>.
createFunction_runtime :: Lens.Lens' CreateFunction (Prelude.Maybe Runtime)
createFunction_runtime = Lens.lens (\CreateFunction' {runtime} -> runtime) (\s@CreateFunction' {} a -> s {runtime = a} :: CreateFunction)

-- | The ARN of the Amazon Web Services Key Management Service (KMS) key
-- that\'s used to encrypt your function\'s environment variables. If it\'s
-- not provided, Lambda uses a default service key.
createFunction_kmsKeyArn :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Text)
createFunction_kmsKeyArn = Lens.lens (\CreateFunction' {kmsKeyArn} -> kmsKeyArn) (\s@CreateFunction' {} a -> s {kmsKeyArn = a} :: CreateFunction)

-- | The type of deployment package. Set to @Image@ for container image and
-- set @Zip@ for ZIP archive.
createFunction_packageType :: Lens.Lens' CreateFunction (Prelude.Maybe PackageType)
createFunction_packageType = Lens.lens (\CreateFunction' {packageType} -> packageType) (\s@CreateFunction' {} a -> s {packageType = a} :: CreateFunction)

-- | Connection settings for an Amazon EFS file system.
createFunction_fileSystemConfigs :: Lens.Lens' CreateFunction (Prelude.Maybe [FileSystemConfig])
createFunction_fileSystemConfigs = Lens.lens (\CreateFunction' {fileSystemConfigs} -> fileSystemConfigs) (\s@CreateFunction' {} a -> s {fileSystemConfigs = a} :: CreateFunction) Prelude.. Lens.mapping Lens.coerced

-- | Environment variables that are accessible from function code during
-- execution.
createFunction_environment :: Lens.Lens' CreateFunction (Prelude.Maybe Environment)
createFunction_environment = Lens.lens (\CreateFunction' {environment} -> environment) (\s@CreateFunction' {} a -> s {environment = a} :: CreateFunction)

-- | Container image
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-images.html#configuration-images-settings configuration values>
-- that override the values in the container image Dockerfile.
createFunction_imageConfig :: Lens.Lens' CreateFunction (Prelude.Maybe ImageConfig)
createFunction_imageConfig = Lens.lens (\CreateFunction' {imageConfig} -> imageConfig) (\s@CreateFunction' {} a -> s {imageConfig = a} :: CreateFunction)

-- | A dead letter queue configuration that specifies the queue or topic
-- where Lambda sends asynchronous events when they fail processing. For
-- more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq Dead Letter Queues>.
createFunction_deadLetterConfig :: Lens.Lens' CreateFunction (Prelude.Maybe DeadLetterConfig)
createFunction_deadLetterConfig = Lens.lens (\CreateFunction' {deadLetterConfig} -> deadLetterConfig) (\s@CreateFunction' {} a -> s {deadLetterConfig = a} :: CreateFunction)

-- | The instruction set architecture that the function supports. Enter a
-- string array with one of the valid values. The default value is
-- @x86_64@.
createFunction_architectures :: Lens.Lens' CreateFunction (Prelude.Maybe (Prelude.NonEmpty Architecture))
createFunction_architectures = Lens.lens (\CreateFunction' {architectures} -> architectures) (\s@CreateFunction' {} a -> s {architectures = a} :: CreateFunction) Prelude.. Lens.mapping Lens.coerced

-- | To enable code signing for this function, specify the ARN of a
-- code-signing configuration. A code-signing configuration includes a set
-- of signing profiles, which define the trusted publishers for this
-- function.
createFunction_codeSigningConfigArn :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Text)
createFunction_codeSigningConfigArn = Lens.lens (\CreateFunction' {codeSigningConfigArn} -> codeSigningConfigArn) (\s@CreateFunction' {} a -> s {codeSigningConfigArn = a} :: CreateFunction)

-- | For network connectivity to Amazon Web Services resources in a VPC,
-- specify a list of security groups and subnets in the VPC. When you
-- connect a function to a VPC, it can only access resources and the
-- internet through that VPC. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings>.
createFunction_vpcConfig :: Lens.Lens' CreateFunction (Prelude.Maybe VpcConfig)
createFunction_vpcConfig = Lens.lens (\CreateFunction' {vpcConfig} -> vpcConfig) (\s@CreateFunction' {} a -> s {vpcConfig = a} :: CreateFunction)

-- | A list of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers>
-- to add to the function\'s execution environment. Specify each layer by
-- its ARN, including the version.
createFunction_layers :: Lens.Lens' CreateFunction (Prelude.Maybe [Prelude.Text])
createFunction_layers = Lens.lens (\CreateFunction' {layers} -> layers) (\s@CreateFunction' {} a -> s {layers = a} :: CreateFunction) Prelude.. Lens.mapping Lens.coerced

-- | The name of the method within your code that Lambda calls to execute
-- your function. The format includes the file name. It can also include
-- namespaces and other qualifiers, depending on the runtime. For more
-- information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model>.
createFunction_handler :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Text)
createFunction_handler = Lens.lens (\CreateFunction' {handler} -> handler) (\s@CreateFunction' {} a -> s {handler = a} :: CreateFunction)

-- | The amount of time that Lambda allows a function to run before stopping
-- it. The default is 3 seconds. The maximum allowed value is 900 seconds.
-- For additional information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-context.html Lambda execution environment>.
createFunction_timeout :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Natural)
createFunction_timeout = Lens.lens (\CreateFunction' {timeout} -> timeout) (\s@CreateFunction' {} a -> s {timeout = a} :: CreateFunction)

-- | Set @Mode@ to @Active@ to sample and trace a subset of incoming requests
-- with
-- <https://docs.aws.amazon.com/lambda/latest/dg/services-xray.html X-Ray>.
createFunction_tracingConfig :: Lens.Lens' CreateFunction (Prelude.Maybe TracingConfig)
createFunction_tracingConfig = Lens.lens (\CreateFunction' {tracingConfig} -> tracingConfig) (\s@CreateFunction' {} a -> s {tracingConfig = a} :: CreateFunction)

-- | A description of the function.
createFunction_description :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Text)
createFunction_description = Lens.lens (\CreateFunction' {description} -> description) (\s@CreateFunction' {} a -> s {description = a} :: CreateFunction)

-- | A list of
-- <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> to
-- apply to the function.
createFunction_tags :: Lens.Lens' CreateFunction (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createFunction_tags = Lens.lens (\CreateFunction' {tags} -> tags) (\s@CreateFunction' {} a -> s {tags = a} :: CreateFunction) Prelude.. Lens.mapping Lens.coerced

-- | Set to true to publish the first version of the function during
-- creation.
createFunction_publish :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Bool)
createFunction_publish = Lens.lens (\CreateFunction' {publish} -> publish) (\s@CreateFunction' {} a -> s {publish = a} :: CreateFunction)

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
createFunction_functionName :: Lens.Lens' CreateFunction Prelude.Text
createFunction_functionName = Lens.lens (\CreateFunction' {functionName} -> functionName) (\s@CreateFunction' {} a -> s {functionName = a} :: CreateFunction)

-- | The Amazon Resource Name (ARN) of the function\'s execution role.
createFunction_role :: Lens.Lens' CreateFunction Prelude.Text
createFunction_role = Lens.lens (\CreateFunction' {role'} -> role') (\s@CreateFunction' {} a -> s {role' = a} :: CreateFunction)

-- | The code for the function.
createFunction_code :: Lens.Lens' CreateFunction FunctionCode
createFunction_code = Lens.lens (\CreateFunction' {code} -> code) (\s@CreateFunction' {} a -> s {code = a} :: CreateFunction)

instance Core.AWSRequest CreateFunction where
  type
    AWSResponse CreateFunction =
      FunctionConfiguration
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable CreateFunction

instance Prelude.NFData CreateFunction

instance Core.ToHeaders CreateFunction where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateFunction where
  toJSON CreateFunction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MemorySize" Core..=) Prelude.<$> memorySize,
            ("Runtime" Core..=) Prelude.<$> runtime,
            ("KMSKeyArn" Core..=) Prelude.<$> kmsKeyArn,
            ("PackageType" Core..=) Prelude.<$> packageType,
            ("FileSystemConfigs" Core..=)
              Prelude.<$> fileSystemConfigs,
            ("Environment" Core..=) Prelude.<$> environment,
            ("ImageConfig" Core..=) Prelude.<$> imageConfig,
            ("DeadLetterConfig" Core..=)
              Prelude.<$> deadLetterConfig,
            ("Architectures" Core..=) Prelude.<$> architectures,
            ("CodeSigningConfigArn" Core..=)
              Prelude.<$> codeSigningConfigArn,
            ("VpcConfig" Core..=) Prelude.<$> vpcConfig,
            ("Layers" Core..=) Prelude.<$> layers,
            ("Handler" Core..=) Prelude.<$> handler,
            ("Timeout" Core..=) Prelude.<$> timeout,
            ("TracingConfig" Core..=) Prelude.<$> tracingConfig,
            ("Description" Core..=) Prelude.<$> description,
            ("Tags" Core..=) Prelude.<$> tags,
            ("Publish" Core..=) Prelude.<$> publish,
            Prelude.Just ("FunctionName" Core..= functionName),
            Prelude.Just ("Role" Core..= role'),
            Prelude.Just ("Code" Core..= code)
          ]
      )

instance Core.ToPath CreateFunction where
  toPath = Prelude.const "/2015-03-31/functions"

instance Core.ToQuery CreateFunction where
  toQuery = Prelude.const Prelude.mempty
