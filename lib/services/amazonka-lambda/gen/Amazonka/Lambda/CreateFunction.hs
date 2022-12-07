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
-- Module      : Amazonka.Lambda.CreateFunction
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Lambda.CreateFunction
  ( -- * Creating a Request
    CreateFunction (..),
    newCreateFunction,

    -- * Request Lenses
    createFunction_tracingConfig,
    createFunction_tags,
    createFunction_fileSystemConfigs,
    createFunction_timeout,
    createFunction_ephemeralStorage,
    createFunction_memorySize,
    createFunction_publish,
    createFunction_imageConfig,
    createFunction_environment,
    createFunction_vpcConfig,
    createFunction_codeSigningConfigArn,
    createFunction_runtime,
    createFunction_description,
    createFunction_kmsKeyArn,
    createFunction_handler,
    createFunction_layers,
    createFunction_packageType,
    createFunction_architectures,
    createFunction_deadLetterConfig,
    createFunction_functionName,
    createFunction_role,
    createFunction_code,

    -- * Destructuring the Response
    FunctionConfiguration (..),
    newFunctionConfiguration,

    -- * Response Lenses
    functionConfiguration_tracingConfig,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_masterArn,
    functionConfiguration_functionArn,
    functionConfiguration_timeout,
    functionConfiguration_ephemeralStorage,
    functionConfiguration_memorySize,
    functionConfiguration_codeSha256,
    functionConfiguration_environment,
    functionConfiguration_vpcConfig,
    functionConfiguration_state,
    functionConfiguration_functionName,
    functionConfiguration_runtime,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_description,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_handler,
    functionConfiguration_layers,
    functionConfiguration_stateReasonCode,
    functionConfiguration_packageType,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_revisionId,
    functionConfiguration_signingJobArn,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_lastModified,
    functionConfiguration_role,
    functionConfiguration_architectures,
    functionConfiguration_stateReason,
    functionConfiguration_version,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_codeSize,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFunction' smart constructor.
data CreateFunction = CreateFunction'
  { -- | Set @Mode@ to @Active@ to sample and trace a subset of incoming requests
    -- with
    -- <https://docs.aws.amazon.com/lambda/latest/dg/services-xray.html X-Ray>.
    tracingConfig :: Prelude.Maybe TracingConfig,
    -- | A list of
    -- <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> to
    -- apply to the function.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Connection settings for an Amazon EFS file system.
    fileSystemConfigs :: Prelude.Maybe [FileSystemConfig],
    -- | The amount of time (in seconds) that Lambda allows a function to run
    -- before stopping it. The default is 3 seconds. The maximum allowed value
    -- is 900 seconds. For additional information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-context.html Lambda execution environment>.
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The size of the function’s \/tmp directory in MB. The default value is
    -- 512, but can be any whole number between 512 and 10240 MB.
    ephemeralStorage :: Prelude.Maybe EphemeralStorage,
    -- | The amount of
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-memory.html memory available to the function>
    -- at runtime. Increasing the function memory also increases its CPU
    -- allocation. The default value is 128 MB. The value can be any multiple
    -- of 1 MB.
    memorySize :: Prelude.Maybe Prelude.Natural,
    -- | Set to true to publish the first version of the function during
    -- creation.
    publish :: Prelude.Maybe Prelude.Bool,
    -- | Container image
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-images.html#configuration-images-settings configuration values>
    -- that override the values in the container image Dockerfile.
    imageConfig :: Prelude.Maybe ImageConfig,
    -- | Environment variables that are accessible from function code during
    -- execution.
    environment :: Prelude.Maybe Environment,
    -- | For network connectivity to Amazon Web Services resources in a VPC,
    -- specify a list of security groups and subnets in the VPC. When you
    -- connect a function to a VPC, it can only access resources and the
    -- internet through that VPC. For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | To enable code signing for this function, specify the ARN of a
    -- code-signing configuration. A code-signing configuration includes a set
    -- of signing profiles, which define the trusted publishers for this
    -- function.
    codeSigningConfigArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the function\'s
    -- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime>.
    -- Runtime is required if the deployment package is a .zip file archive.
    runtime :: Prelude.Maybe Runtime,
    -- | A description of the function.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Amazon Web Services Key Management Service (KMS) key
    -- that\'s used to encrypt your function\'s environment variables. If it\'s
    -- not provided, Lambda uses a default service key.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the method within your code that Lambda calls to execute
    -- your function. Handler is required if the deployment package is a .zip
    -- file archive. The format includes the file name. It can also include
    -- namespaces and other qualifiers, depending on the runtime. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model>.
    handler :: Prelude.Maybe Prelude.Text,
    -- | A list of
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers>
    -- to add to the function\'s execution environment. Specify each layer by
    -- its ARN, including the version.
    layers :: Prelude.Maybe [Prelude.Text],
    -- | The type of deployment package. Set to @Image@ for container image and
    -- set @Zip@ for ZIP archive.
    packageType :: Prelude.Maybe PackageType,
    -- | The instruction set architecture that the function supports. Enter a
    -- string array with one of the valid values (arm64 or x86_64). The default
    -- value is @x86_64@.
    architectures :: Prelude.Maybe (Prelude.NonEmpty Architecture),
    -- | A dead letter queue configuration that specifies the queue or topic
    -- where Lambda sends asynchronous events when they fail processing. For
    -- more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq Dead Letter Queues>.
    deadLetterConfig :: Prelude.Maybe DeadLetterConfig,
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
-- 'tracingConfig', 'createFunction_tracingConfig' - Set @Mode@ to @Active@ to sample and trace a subset of incoming requests
-- with
-- <https://docs.aws.amazon.com/lambda/latest/dg/services-xray.html X-Ray>.
--
-- 'tags', 'createFunction_tags' - A list of
-- <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> to
-- apply to the function.
--
-- 'fileSystemConfigs', 'createFunction_fileSystemConfigs' - Connection settings for an Amazon EFS file system.
--
-- 'timeout', 'createFunction_timeout' - The amount of time (in seconds) that Lambda allows a function to run
-- before stopping it. The default is 3 seconds. The maximum allowed value
-- is 900 seconds. For additional information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-context.html Lambda execution environment>.
--
-- 'ephemeralStorage', 'createFunction_ephemeralStorage' - The size of the function’s \/tmp directory in MB. The default value is
-- 512, but can be any whole number between 512 and 10240 MB.
--
-- 'memorySize', 'createFunction_memorySize' - The amount of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-memory.html memory available to the function>
-- at runtime. Increasing the function memory also increases its CPU
-- allocation. The default value is 128 MB. The value can be any multiple
-- of 1 MB.
--
-- 'publish', 'createFunction_publish' - Set to true to publish the first version of the function during
-- creation.
--
-- 'imageConfig', 'createFunction_imageConfig' - Container image
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-images.html#configuration-images-settings configuration values>
-- that override the values in the container image Dockerfile.
--
-- 'environment', 'createFunction_environment' - Environment variables that are accessible from function code during
-- execution.
--
-- 'vpcConfig', 'createFunction_vpcConfig' - For network connectivity to Amazon Web Services resources in a VPC,
-- specify a list of security groups and subnets in the VPC. When you
-- connect a function to a VPC, it can only access resources and the
-- internet through that VPC. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings>.
--
-- 'codeSigningConfigArn', 'createFunction_codeSigningConfigArn' - To enable code signing for this function, specify the ARN of a
-- code-signing configuration. A code-signing configuration includes a set
-- of signing profiles, which define the trusted publishers for this
-- function.
--
-- 'runtime', 'createFunction_runtime' - The identifier of the function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime>.
-- Runtime is required if the deployment package is a .zip file archive.
--
-- 'description', 'createFunction_description' - A description of the function.
--
-- 'kmsKeyArn', 'createFunction_kmsKeyArn' - The ARN of the Amazon Web Services Key Management Service (KMS) key
-- that\'s used to encrypt your function\'s environment variables. If it\'s
-- not provided, Lambda uses a default service key.
--
-- 'handler', 'createFunction_handler' - The name of the method within your code that Lambda calls to execute
-- your function. Handler is required if the deployment package is a .zip
-- file archive. The format includes the file name. It can also include
-- namespaces and other qualifiers, depending on the runtime. For more
-- information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model>.
--
-- 'layers', 'createFunction_layers' - A list of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers>
-- to add to the function\'s execution environment. Specify each layer by
-- its ARN, including the version.
--
-- 'packageType', 'createFunction_packageType' - The type of deployment package. Set to @Image@ for container image and
-- set @Zip@ for ZIP archive.
--
-- 'architectures', 'createFunction_architectures' - The instruction set architecture that the function supports. Enter a
-- string array with one of the valid values (arm64 or x86_64). The default
-- value is @x86_64@.
--
-- 'deadLetterConfig', 'createFunction_deadLetterConfig' - A dead letter queue configuration that specifies the queue or topic
-- where Lambda sends asynchronous events when they fail processing. For
-- more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq Dead Letter Queues>.
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
    { tracingConfig = Prelude.Nothing,
      tags = Prelude.Nothing,
      fileSystemConfigs = Prelude.Nothing,
      timeout = Prelude.Nothing,
      ephemeralStorage = Prelude.Nothing,
      memorySize = Prelude.Nothing,
      publish = Prelude.Nothing,
      imageConfig = Prelude.Nothing,
      environment = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      codeSigningConfigArn = Prelude.Nothing,
      runtime = Prelude.Nothing,
      description = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      handler = Prelude.Nothing,
      layers = Prelude.Nothing,
      packageType = Prelude.Nothing,
      architectures = Prelude.Nothing,
      deadLetterConfig = Prelude.Nothing,
      functionName = pFunctionName_,
      role' = pRole_,
      code = pCode_
    }

-- | Set @Mode@ to @Active@ to sample and trace a subset of incoming requests
-- with
-- <https://docs.aws.amazon.com/lambda/latest/dg/services-xray.html X-Ray>.
createFunction_tracingConfig :: Lens.Lens' CreateFunction (Prelude.Maybe TracingConfig)
createFunction_tracingConfig = Lens.lens (\CreateFunction' {tracingConfig} -> tracingConfig) (\s@CreateFunction' {} a -> s {tracingConfig = a} :: CreateFunction)

-- | A list of
-- <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> to
-- apply to the function.
createFunction_tags :: Lens.Lens' CreateFunction (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createFunction_tags = Lens.lens (\CreateFunction' {tags} -> tags) (\s@CreateFunction' {} a -> s {tags = a} :: CreateFunction) Prelude.. Lens.mapping Lens.coerced

-- | Connection settings for an Amazon EFS file system.
createFunction_fileSystemConfigs :: Lens.Lens' CreateFunction (Prelude.Maybe [FileSystemConfig])
createFunction_fileSystemConfigs = Lens.lens (\CreateFunction' {fileSystemConfigs} -> fileSystemConfigs) (\s@CreateFunction' {} a -> s {fileSystemConfigs = a} :: CreateFunction) Prelude.. Lens.mapping Lens.coerced

-- | The amount of time (in seconds) that Lambda allows a function to run
-- before stopping it. The default is 3 seconds. The maximum allowed value
-- is 900 seconds. For additional information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-context.html Lambda execution environment>.
createFunction_timeout :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Natural)
createFunction_timeout = Lens.lens (\CreateFunction' {timeout} -> timeout) (\s@CreateFunction' {} a -> s {timeout = a} :: CreateFunction)

-- | The size of the function’s \/tmp directory in MB. The default value is
-- 512, but can be any whole number between 512 and 10240 MB.
createFunction_ephemeralStorage :: Lens.Lens' CreateFunction (Prelude.Maybe EphemeralStorage)
createFunction_ephemeralStorage = Lens.lens (\CreateFunction' {ephemeralStorage} -> ephemeralStorage) (\s@CreateFunction' {} a -> s {ephemeralStorage = a} :: CreateFunction)

-- | The amount of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-memory.html memory available to the function>
-- at runtime. Increasing the function memory also increases its CPU
-- allocation. The default value is 128 MB. The value can be any multiple
-- of 1 MB.
createFunction_memorySize :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Natural)
createFunction_memorySize = Lens.lens (\CreateFunction' {memorySize} -> memorySize) (\s@CreateFunction' {} a -> s {memorySize = a} :: CreateFunction)

-- | Set to true to publish the first version of the function during
-- creation.
createFunction_publish :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Bool)
createFunction_publish = Lens.lens (\CreateFunction' {publish} -> publish) (\s@CreateFunction' {} a -> s {publish = a} :: CreateFunction)

-- | Container image
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-images.html#configuration-images-settings configuration values>
-- that override the values in the container image Dockerfile.
createFunction_imageConfig :: Lens.Lens' CreateFunction (Prelude.Maybe ImageConfig)
createFunction_imageConfig = Lens.lens (\CreateFunction' {imageConfig} -> imageConfig) (\s@CreateFunction' {} a -> s {imageConfig = a} :: CreateFunction)

-- | Environment variables that are accessible from function code during
-- execution.
createFunction_environment :: Lens.Lens' CreateFunction (Prelude.Maybe Environment)
createFunction_environment = Lens.lens (\CreateFunction' {environment} -> environment) (\s@CreateFunction' {} a -> s {environment = a} :: CreateFunction)

-- | For network connectivity to Amazon Web Services resources in a VPC,
-- specify a list of security groups and subnets in the VPC. When you
-- connect a function to a VPC, it can only access resources and the
-- internet through that VPC. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings>.
createFunction_vpcConfig :: Lens.Lens' CreateFunction (Prelude.Maybe VpcConfig)
createFunction_vpcConfig = Lens.lens (\CreateFunction' {vpcConfig} -> vpcConfig) (\s@CreateFunction' {} a -> s {vpcConfig = a} :: CreateFunction)

-- | To enable code signing for this function, specify the ARN of a
-- code-signing configuration. A code-signing configuration includes a set
-- of signing profiles, which define the trusted publishers for this
-- function.
createFunction_codeSigningConfigArn :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Text)
createFunction_codeSigningConfigArn = Lens.lens (\CreateFunction' {codeSigningConfigArn} -> codeSigningConfigArn) (\s@CreateFunction' {} a -> s {codeSigningConfigArn = a} :: CreateFunction)

-- | The identifier of the function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime>.
-- Runtime is required if the deployment package is a .zip file archive.
createFunction_runtime :: Lens.Lens' CreateFunction (Prelude.Maybe Runtime)
createFunction_runtime = Lens.lens (\CreateFunction' {runtime} -> runtime) (\s@CreateFunction' {} a -> s {runtime = a} :: CreateFunction)

-- | A description of the function.
createFunction_description :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Text)
createFunction_description = Lens.lens (\CreateFunction' {description} -> description) (\s@CreateFunction' {} a -> s {description = a} :: CreateFunction)

-- | The ARN of the Amazon Web Services Key Management Service (KMS) key
-- that\'s used to encrypt your function\'s environment variables. If it\'s
-- not provided, Lambda uses a default service key.
createFunction_kmsKeyArn :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Text)
createFunction_kmsKeyArn = Lens.lens (\CreateFunction' {kmsKeyArn} -> kmsKeyArn) (\s@CreateFunction' {} a -> s {kmsKeyArn = a} :: CreateFunction)

-- | The name of the method within your code that Lambda calls to execute
-- your function. Handler is required if the deployment package is a .zip
-- file archive. The format includes the file name. It can also include
-- namespaces and other qualifiers, depending on the runtime. For more
-- information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model>.
createFunction_handler :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Text)
createFunction_handler = Lens.lens (\CreateFunction' {handler} -> handler) (\s@CreateFunction' {} a -> s {handler = a} :: CreateFunction)

-- | A list of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers>
-- to add to the function\'s execution environment. Specify each layer by
-- its ARN, including the version.
createFunction_layers :: Lens.Lens' CreateFunction (Prelude.Maybe [Prelude.Text])
createFunction_layers = Lens.lens (\CreateFunction' {layers} -> layers) (\s@CreateFunction' {} a -> s {layers = a} :: CreateFunction) Prelude.. Lens.mapping Lens.coerced

-- | The type of deployment package. Set to @Image@ for container image and
-- set @Zip@ for ZIP archive.
createFunction_packageType :: Lens.Lens' CreateFunction (Prelude.Maybe PackageType)
createFunction_packageType = Lens.lens (\CreateFunction' {packageType} -> packageType) (\s@CreateFunction' {} a -> s {packageType = a} :: CreateFunction)

-- | The instruction set architecture that the function supports. Enter a
-- string array with one of the valid values (arm64 or x86_64). The default
-- value is @x86_64@.
createFunction_architectures :: Lens.Lens' CreateFunction (Prelude.Maybe (Prelude.NonEmpty Architecture))
createFunction_architectures = Lens.lens (\CreateFunction' {architectures} -> architectures) (\s@CreateFunction' {} a -> s {architectures = a} :: CreateFunction) Prelude.. Lens.mapping Lens.coerced

-- | A dead letter queue configuration that specifies the queue or topic
-- where Lambda sends asynchronous events when they fail processing. For
-- more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq Dead Letter Queues>.
createFunction_deadLetterConfig :: Lens.Lens' CreateFunction (Prelude.Maybe DeadLetterConfig)
createFunction_deadLetterConfig = Lens.lens (\CreateFunction' {deadLetterConfig} -> deadLetterConfig) (\s@CreateFunction' {} a -> s {deadLetterConfig = a} :: CreateFunction)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateFunction where
  hashWithSalt _salt CreateFunction' {..} =
    _salt `Prelude.hashWithSalt` tracingConfig
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` fileSystemConfigs
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` ephemeralStorage
      `Prelude.hashWithSalt` memorySize
      `Prelude.hashWithSalt` publish
      `Prelude.hashWithSalt` imageConfig
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` codeSigningConfigArn
      `Prelude.hashWithSalt` runtime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` handler
      `Prelude.hashWithSalt` layers
      `Prelude.hashWithSalt` packageType
      `Prelude.hashWithSalt` architectures
      `Prelude.hashWithSalt` deadLetterConfig
      `Prelude.hashWithSalt` functionName
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` code

instance Prelude.NFData CreateFunction where
  rnf CreateFunction' {..} =
    Prelude.rnf tracingConfig
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf fileSystemConfigs
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf ephemeralStorage
      `Prelude.seq` Prelude.rnf memorySize
      `Prelude.seq` Prelude.rnf publish
      `Prelude.seq` Prelude.rnf imageConfig
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf codeSigningConfigArn
      `Prelude.seq` Prelude.rnf runtime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf handler
      `Prelude.seq` Prelude.rnf layers
      `Prelude.seq` Prelude.rnf packageType
      `Prelude.seq` Prelude.rnf architectures
      `Prelude.seq` Prelude.rnf deadLetterConfig
      `Prelude.seq` Prelude.rnf functionName
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf code

instance Data.ToHeaders CreateFunction where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateFunction where
  toJSON CreateFunction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TracingConfig" Data..=) Prelude.<$> tracingConfig,
            ("Tags" Data..=) Prelude.<$> tags,
            ("FileSystemConfigs" Data..=)
              Prelude.<$> fileSystemConfigs,
            ("Timeout" Data..=) Prelude.<$> timeout,
            ("EphemeralStorage" Data..=)
              Prelude.<$> ephemeralStorage,
            ("MemorySize" Data..=) Prelude.<$> memorySize,
            ("Publish" Data..=) Prelude.<$> publish,
            ("ImageConfig" Data..=) Prelude.<$> imageConfig,
            ("Environment" Data..=) Prelude.<$> environment,
            ("VpcConfig" Data..=) Prelude.<$> vpcConfig,
            ("CodeSigningConfigArn" Data..=)
              Prelude.<$> codeSigningConfigArn,
            ("Runtime" Data..=) Prelude.<$> runtime,
            ("Description" Data..=) Prelude.<$> description,
            ("KMSKeyArn" Data..=) Prelude.<$> kmsKeyArn,
            ("Handler" Data..=) Prelude.<$> handler,
            ("Layers" Data..=) Prelude.<$> layers,
            ("PackageType" Data..=) Prelude.<$> packageType,
            ("Architectures" Data..=) Prelude.<$> architectures,
            ("DeadLetterConfig" Data..=)
              Prelude.<$> deadLetterConfig,
            Prelude.Just ("FunctionName" Data..= functionName),
            Prelude.Just ("Role" Data..= role'),
            Prelude.Just ("Code" Data..= code)
          ]
      )

instance Data.ToPath CreateFunction where
  toPath = Prelude.const "/2015-03-31/functions"

instance Data.ToQuery CreateFunction where
  toQuery = Prelude.const Prelude.mempty
