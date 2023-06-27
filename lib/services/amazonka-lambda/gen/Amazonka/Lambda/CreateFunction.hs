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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- permission to use Amazon Web Services, such as Amazon CloudWatch Logs
-- for log streaming and X-Ray for request tracing.
--
-- If the deployment package is a
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-images.html container image>,
-- then you set the package type to @Image@. For a container image, the
-- code property must include the URI of a container image in the Amazon
-- ECR registry. You do not need to specify the handler and runtime
-- properties.
--
-- If the deployment package is a
-- <https://docs.aws.amazon.com/lambda/latest/dg/gettingstarted-package.html#gettingstarted-package-zip .zip file archive>,
-- then you set the package type to @Zip@. For a .zip file archive, the
-- code property specifies the location of the .zip file. You must also
-- specify the handler and runtime properties. The code in the deployment
-- package must be compatible with the target instruction set architecture
-- of the function (@x86-64@ or @arm64@). If you do not specify the
-- architecture, then the default value is @x86-64@.
--
-- When you create a function, Lambda provisions an instance of the
-- function and its supporting resources. If your function connects to a
-- VPC, this process can take a minute or so. During this time, you can\'t
-- invoke or modify the function. The @State@, @StateReason@, and
-- @StateReasonCode@ fields in the response from GetFunctionConfiguration
-- indicate when the function is ready to invoke. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/functions-states.html Lambda function states>.
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
-- configuration includes set of signing profiles, which define the trusted
-- publishers for this function.
--
-- If another Amazon Web Services account or an Amazon Web Service invokes
-- your function, use AddPermission to grant permission by creating a
-- resource-based Identity and Access Management (IAM) policy. You can
-- grant permissions at the function level, on a version, or on an alias.
--
-- To invoke your function directly, use Invoke. To invoke your function in
-- response to events in other Amazon Web Services, create an event source
-- mapping (CreateEventSourceMapping), or configure a function trigger in
-- the other service. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-invocation.html Invoking Lambda functions>.
module Amazonka.Lambda.CreateFunction
  ( -- * Creating a Request
    CreateFunction (..),
    newCreateFunction,

    -- * Request Lenses
    createFunction_architectures,
    createFunction_codeSigningConfigArn,
    createFunction_deadLetterConfig,
    createFunction_description,
    createFunction_environment,
    createFunction_ephemeralStorage,
    createFunction_fileSystemConfigs,
    createFunction_handler,
    createFunction_imageConfig,
    createFunction_kmsKeyArn,
    createFunction_layers,
    createFunction_memorySize,
    createFunction_packageType,
    createFunction_publish,
    createFunction_runtime,
    createFunction_snapStart,
    createFunction_tags,
    createFunction_timeout,
    createFunction_tracingConfig,
    createFunction_vpcConfig,
    createFunction_functionName,
    createFunction_role,
    createFunction_code,

    -- * Destructuring the Response
    FunctionConfiguration (..),
    newFunctionConfiguration,

    -- * Response Lenses
    functionConfiguration_architectures,
    functionConfiguration_codeSha256,
    functionConfiguration_codeSize,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_description,
    functionConfiguration_environment,
    functionConfiguration_ephemeralStorage,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_functionArn,
    functionConfiguration_functionName,
    functionConfiguration_handler,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_lastModified,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_layers,
    functionConfiguration_masterArn,
    functionConfiguration_memorySize,
    functionConfiguration_packageType,
    functionConfiguration_revisionId,
    functionConfiguration_role,
    functionConfiguration_runtime,
    functionConfiguration_runtimeVersionConfig,
    functionConfiguration_signingJobArn,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_snapStart,
    functionConfiguration_state,
    functionConfiguration_stateReason,
    functionConfiguration_stateReasonCode,
    functionConfiguration_timeout,
    functionConfiguration_tracingConfig,
    functionConfiguration_version,
    functionConfiguration_vpcConfig,
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
  { -- | The instruction set architecture that the function supports. Enter a
    -- string array with one of the valid values (arm64 or x86_64). The default
    -- value is @x86_64@.
    architectures :: Prelude.Maybe (Prelude.NonEmpty Architecture),
    -- | To enable code signing for this function, specify the ARN of a
    -- code-signing configuration. A code-signing configuration includes a set
    -- of signing profiles, which define the trusted publishers for this
    -- function.
    codeSigningConfigArn :: Prelude.Maybe Prelude.Text,
    -- | A dead-letter queue configuration that specifies the queue or topic
    -- where Lambda sends asynchronous events when they fail processing. For
    -- more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#invocation-dlq Dead-letter queues>.
    deadLetterConfig :: Prelude.Maybe DeadLetterConfig,
    -- | A description of the function.
    description :: Prelude.Maybe Prelude.Text,
    -- | Environment variables that are accessible from function code during
    -- execution.
    environment :: Prelude.Maybe Environment,
    -- | The size of the function\'s @\/tmp@ directory in MB. The default value
    -- is 512, but can be any whole number between 512 and 10,240 MB.
    ephemeralStorage :: Prelude.Maybe EphemeralStorage,
    -- | Connection settings for an Amazon EFS file system.
    fileSystemConfigs :: Prelude.Maybe [FileSystemConfig],
    -- | The name of the method within your code that Lambda calls to run your
    -- function. Handler is required if the deployment package is a .zip file
    -- archive. The format includes the file name. It can also include
    -- namespaces and other qualifiers, depending on the runtime. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-progmodel.html Lambda programming model>.
    handler :: Prelude.Maybe Prelude.Text,
    -- | Container image
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-images.html#configuration-images-settings configuration values>
    -- that override the values in the container image Dockerfile.
    imageConfig :: Prelude.Maybe ImageConfig,
    -- | The ARN of the Key Management Service (KMS) customer managed key that\'s
    -- used to encrypt your function\'s
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-envvars.html#configuration-envvars-encryption environment variables>.
    -- When
    -- <https://docs.aws.amazon.com/lambda/latest/dg/snapstart-security.html Lambda SnapStart>
    -- is activated, Lambda also uses this key is to encrypt your function\'s
    -- snapshot. If you deploy your function using a container image, Lambda
    -- also uses this key to encrypt your function when it\'s deployed. Note
    -- that this is not the same key that\'s used to protect your container
    -- image in the Amazon Elastic Container Registry (Amazon ECR). If you
    -- don\'t provide a customer managed key, Lambda uses a default service
    -- key.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | A list of
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers>
    -- to add to the function\'s execution environment. Specify each layer by
    -- its ARN, including the version.
    layers :: Prelude.Maybe [Prelude.Text],
    -- | The amount of
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-function-common.html#configuration-memory-console memory available to the function>
    -- at runtime. Increasing the function memory also increases its CPU
    -- allocation. The default value is 128 MB. The value can be any multiple
    -- of 1 MB.
    memorySize :: Prelude.Maybe Prelude.Natural,
    -- | The type of deployment package. Set to @Image@ for container image and
    -- set to @Zip@ for .zip file archive.
    packageType :: Prelude.Maybe PackageType,
    -- | Set to true to publish the first version of the function during
    -- creation.
    publish :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the function\'s
    -- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime>.
    -- Runtime is required if the deployment package is a .zip file archive.
    --
    -- The following list includes deprecated runtimes. For more information,
    -- see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html#runtime-support-policy Runtime deprecation policy>.
    runtime :: Prelude.Maybe Runtime,
    -- | The function\'s
    -- <https://docs.aws.amazon.com/lambda/latest/dg/snapstart.html SnapStart>
    -- setting.
    snapStart :: Prelude.Maybe SnapStart,
    -- | A list of
    -- <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> to
    -- apply to the function.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The amount of time (in seconds) that Lambda allows a function to run
    -- before stopping it. The default is 3 seconds. The maximum allowed value
    -- is 900 seconds. For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-context.html Lambda execution environment>.
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | Set @Mode@ to @Active@ to sample and trace a subset of incoming requests
    -- with
    -- <https://docs.aws.amazon.com/lambda/latest/dg/services-xray.html X-Ray>.
    tracingConfig :: Prelude.Maybe TracingConfig,
    -- | For network connectivity to Amazon Web Services resources in a VPC,
    -- specify a list of security groups and subnets in the VPC. When you
    -- connect a function to a VPC, it can access resources and the internet
    -- only through that VPC. For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html Configuring a Lambda function to access resources in a VPC>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ – @my-function@.
    --
    -- -   __Function ARN__ –
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ – @123456789012:function:my-function@.
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
-- 'architectures', 'createFunction_architectures' - The instruction set architecture that the function supports. Enter a
-- string array with one of the valid values (arm64 or x86_64). The default
-- value is @x86_64@.
--
-- 'codeSigningConfigArn', 'createFunction_codeSigningConfigArn' - To enable code signing for this function, specify the ARN of a
-- code-signing configuration. A code-signing configuration includes a set
-- of signing profiles, which define the trusted publishers for this
-- function.
--
-- 'deadLetterConfig', 'createFunction_deadLetterConfig' - A dead-letter queue configuration that specifies the queue or topic
-- where Lambda sends asynchronous events when they fail processing. For
-- more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#invocation-dlq Dead-letter queues>.
--
-- 'description', 'createFunction_description' - A description of the function.
--
-- 'environment', 'createFunction_environment' - Environment variables that are accessible from function code during
-- execution.
--
-- 'ephemeralStorage', 'createFunction_ephemeralStorage' - The size of the function\'s @\/tmp@ directory in MB. The default value
-- is 512, but can be any whole number between 512 and 10,240 MB.
--
-- 'fileSystemConfigs', 'createFunction_fileSystemConfigs' - Connection settings for an Amazon EFS file system.
--
-- 'handler', 'createFunction_handler' - The name of the method within your code that Lambda calls to run your
-- function. Handler is required if the deployment package is a .zip file
-- archive. The format includes the file name. It can also include
-- namespaces and other qualifiers, depending on the runtime. For more
-- information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-progmodel.html Lambda programming model>.
--
-- 'imageConfig', 'createFunction_imageConfig' - Container image
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-images.html#configuration-images-settings configuration values>
-- that override the values in the container image Dockerfile.
--
-- 'kmsKeyArn', 'createFunction_kmsKeyArn' - The ARN of the Key Management Service (KMS) customer managed key that\'s
-- used to encrypt your function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-envvars.html#configuration-envvars-encryption environment variables>.
-- When
-- <https://docs.aws.amazon.com/lambda/latest/dg/snapstart-security.html Lambda SnapStart>
-- is activated, Lambda also uses this key is to encrypt your function\'s
-- snapshot. If you deploy your function using a container image, Lambda
-- also uses this key to encrypt your function when it\'s deployed. Note
-- that this is not the same key that\'s used to protect your container
-- image in the Amazon Elastic Container Registry (Amazon ECR). If you
-- don\'t provide a customer managed key, Lambda uses a default service
-- key.
--
-- 'layers', 'createFunction_layers' - A list of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers>
-- to add to the function\'s execution environment. Specify each layer by
-- its ARN, including the version.
--
-- 'memorySize', 'createFunction_memorySize' - The amount of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-function-common.html#configuration-memory-console memory available to the function>
-- at runtime. Increasing the function memory also increases its CPU
-- allocation. The default value is 128 MB. The value can be any multiple
-- of 1 MB.
--
-- 'packageType', 'createFunction_packageType' - The type of deployment package. Set to @Image@ for container image and
-- set to @Zip@ for .zip file archive.
--
-- 'publish', 'createFunction_publish' - Set to true to publish the first version of the function during
-- creation.
--
-- 'runtime', 'createFunction_runtime' - The identifier of the function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime>.
-- Runtime is required if the deployment package is a .zip file archive.
--
-- The following list includes deprecated runtimes. For more information,
-- see
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html#runtime-support-policy Runtime deprecation policy>.
--
-- 'snapStart', 'createFunction_snapStart' - The function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/snapstart.html SnapStart>
-- setting.
--
-- 'tags', 'createFunction_tags' - A list of
-- <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> to
-- apply to the function.
--
-- 'timeout', 'createFunction_timeout' - The amount of time (in seconds) that Lambda allows a function to run
-- before stopping it. The default is 3 seconds. The maximum allowed value
-- is 900 seconds. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-context.html Lambda execution environment>.
--
-- 'tracingConfig', 'createFunction_tracingConfig' - Set @Mode@ to @Active@ to sample and trace a subset of incoming requests
-- with
-- <https://docs.aws.amazon.com/lambda/latest/dg/services-xray.html X-Ray>.
--
-- 'vpcConfig', 'createFunction_vpcConfig' - For network connectivity to Amazon Web Services resources in a VPC,
-- specify a list of security groups and subnets in the VPC. When you
-- connect a function to a VPC, it can access resources and the internet
-- only through that VPC. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html Configuring a Lambda function to access resources in a VPC>.
--
-- 'functionName', 'createFunction_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@.
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
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
    { architectures = Prelude.Nothing,
      codeSigningConfigArn = Prelude.Nothing,
      deadLetterConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      environment = Prelude.Nothing,
      ephemeralStorage = Prelude.Nothing,
      fileSystemConfigs = Prelude.Nothing,
      handler = Prelude.Nothing,
      imageConfig = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      layers = Prelude.Nothing,
      memorySize = Prelude.Nothing,
      packageType = Prelude.Nothing,
      publish = Prelude.Nothing,
      runtime = Prelude.Nothing,
      snapStart = Prelude.Nothing,
      tags = Prelude.Nothing,
      timeout = Prelude.Nothing,
      tracingConfig = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      functionName = pFunctionName_,
      role' = pRole_,
      code = pCode_
    }

-- | The instruction set architecture that the function supports. Enter a
-- string array with one of the valid values (arm64 or x86_64). The default
-- value is @x86_64@.
createFunction_architectures :: Lens.Lens' CreateFunction (Prelude.Maybe (Prelude.NonEmpty Architecture))
createFunction_architectures = Lens.lens (\CreateFunction' {architectures} -> architectures) (\s@CreateFunction' {} a -> s {architectures = a} :: CreateFunction) Prelude.. Lens.mapping Lens.coerced

-- | To enable code signing for this function, specify the ARN of a
-- code-signing configuration. A code-signing configuration includes a set
-- of signing profiles, which define the trusted publishers for this
-- function.
createFunction_codeSigningConfigArn :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Text)
createFunction_codeSigningConfigArn = Lens.lens (\CreateFunction' {codeSigningConfigArn} -> codeSigningConfigArn) (\s@CreateFunction' {} a -> s {codeSigningConfigArn = a} :: CreateFunction)

-- | A dead-letter queue configuration that specifies the queue or topic
-- where Lambda sends asynchronous events when they fail processing. For
-- more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#invocation-dlq Dead-letter queues>.
createFunction_deadLetterConfig :: Lens.Lens' CreateFunction (Prelude.Maybe DeadLetterConfig)
createFunction_deadLetterConfig = Lens.lens (\CreateFunction' {deadLetterConfig} -> deadLetterConfig) (\s@CreateFunction' {} a -> s {deadLetterConfig = a} :: CreateFunction)

-- | A description of the function.
createFunction_description :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Text)
createFunction_description = Lens.lens (\CreateFunction' {description} -> description) (\s@CreateFunction' {} a -> s {description = a} :: CreateFunction)

-- | Environment variables that are accessible from function code during
-- execution.
createFunction_environment :: Lens.Lens' CreateFunction (Prelude.Maybe Environment)
createFunction_environment = Lens.lens (\CreateFunction' {environment} -> environment) (\s@CreateFunction' {} a -> s {environment = a} :: CreateFunction)

-- | The size of the function\'s @\/tmp@ directory in MB. The default value
-- is 512, but can be any whole number between 512 and 10,240 MB.
createFunction_ephemeralStorage :: Lens.Lens' CreateFunction (Prelude.Maybe EphemeralStorage)
createFunction_ephemeralStorage = Lens.lens (\CreateFunction' {ephemeralStorage} -> ephemeralStorage) (\s@CreateFunction' {} a -> s {ephemeralStorage = a} :: CreateFunction)

-- | Connection settings for an Amazon EFS file system.
createFunction_fileSystemConfigs :: Lens.Lens' CreateFunction (Prelude.Maybe [FileSystemConfig])
createFunction_fileSystemConfigs = Lens.lens (\CreateFunction' {fileSystemConfigs} -> fileSystemConfigs) (\s@CreateFunction' {} a -> s {fileSystemConfigs = a} :: CreateFunction) Prelude.. Lens.mapping Lens.coerced

-- | The name of the method within your code that Lambda calls to run your
-- function. Handler is required if the deployment package is a .zip file
-- archive. The format includes the file name. It can also include
-- namespaces and other qualifiers, depending on the runtime. For more
-- information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-progmodel.html Lambda programming model>.
createFunction_handler :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Text)
createFunction_handler = Lens.lens (\CreateFunction' {handler} -> handler) (\s@CreateFunction' {} a -> s {handler = a} :: CreateFunction)

-- | Container image
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-images.html#configuration-images-settings configuration values>
-- that override the values in the container image Dockerfile.
createFunction_imageConfig :: Lens.Lens' CreateFunction (Prelude.Maybe ImageConfig)
createFunction_imageConfig = Lens.lens (\CreateFunction' {imageConfig} -> imageConfig) (\s@CreateFunction' {} a -> s {imageConfig = a} :: CreateFunction)

-- | The ARN of the Key Management Service (KMS) customer managed key that\'s
-- used to encrypt your function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-envvars.html#configuration-envvars-encryption environment variables>.
-- When
-- <https://docs.aws.amazon.com/lambda/latest/dg/snapstart-security.html Lambda SnapStart>
-- is activated, Lambda also uses this key is to encrypt your function\'s
-- snapshot. If you deploy your function using a container image, Lambda
-- also uses this key to encrypt your function when it\'s deployed. Note
-- that this is not the same key that\'s used to protect your container
-- image in the Amazon Elastic Container Registry (Amazon ECR). If you
-- don\'t provide a customer managed key, Lambda uses a default service
-- key.
createFunction_kmsKeyArn :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Text)
createFunction_kmsKeyArn = Lens.lens (\CreateFunction' {kmsKeyArn} -> kmsKeyArn) (\s@CreateFunction' {} a -> s {kmsKeyArn = a} :: CreateFunction)

-- | A list of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers>
-- to add to the function\'s execution environment. Specify each layer by
-- its ARN, including the version.
createFunction_layers :: Lens.Lens' CreateFunction (Prelude.Maybe [Prelude.Text])
createFunction_layers = Lens.lens (\CreateFunction' {layers} -> layers) (\s@CreateFunction' {} a -> s {layers = a} :: CreateFunction) Prelude.. Lens.mapping Lens.coerced

-- | The amount of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-function-common.html#configuration-memory-console memory available to the function>
-- at runtime. Increasing the function memory also increases its CPU
-- allocation. The default value is 128 MB. The value can be any multiple
-- of 1 MB.
createFunction_memorySize :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Natural)
createFunction_memorySize = Lens.lens (\CreateFunction' {memorySize} -> memorySize) (\s@CreateFunction' {} a -> s {memorySize = a} :: CreateFunction)

-- | The type of deployment package. Set to @Image@ for container image and
-- set to @Zip@ for .zip file archive.
createFunction_packageType :: Lens.Lens' CreateFunction (Prelude.Maybe PackageType)
createFunction_packageType = Lens.lens (\CreateFunction' {packageType} -> packageType) (\s@CreateFunction' {} a -> s {packageType = a} :: CreateFunction)

-- | Set to true to publish the first version of the function during
-- creation.
createFunction_publish :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Bool)
createFunction_publish = Lens.lens (\CreateFunction' {publish} -> publish) (\s@CreateFunction' {} a -> s {publish = a} :: CreateFunction)

-- | The identifier of the function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime>.
-- Runtime is required if the deployment package is a .zip file archive.
--
-- The following list includes deprecated runtimes. For more information,
-- see
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html#runtime-support-policy Runtime deprecation policy>.
createFunction_runtime :: Lens.Lens' CreateFunction (Prelude.Maybe Runtime)
createFunction_runtime = Lens.lens (\CreateFunction' {runtime} -> runtime) (\s@CreateFunction' {} a -> s {runtime = a} :: CreateFunction)

-- | The function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/snapstart.html SnapStart>
-- setting.
createFunction_snapStart :: Lens.Lens' CreateFunction (Prelude.Maybe SnapStart)
createFunction_snapStart = Lens.lens (\CreateFunction' {snapStart} -> snapStart) (\s@CreateFunction' {} a -> s {snapStart = a} :: CreateFunction)

-- | A list of
-- <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> to
-- apply to the function.
createFunction_tags :: Lens.Lens' CreateFunction (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createFunction_tags = Lens.lens (\CreateFunction' {tags} -> tags) (\s@CreateFunction' {} a -> s {tags = a} :: CreateFunction) Prelude.. Lens.mapping Lens.coerced

-- | The amount of time (in seconds) that Lambda allows a function to run
-- before stopping it. The default is 3 seconds. The maximum allowed value
-- is 900 seconds. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-context.html Lambda execution environment>.
createFunction_timeout :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Natural)
createFunction_timeout = Lens.lens (\CreateFunction' {timeout} -> timeout) (\s@CreateFunction' {} a -> s {timeout = a} :: CreateFunction)

-- | Set @Mode@ to @Active@ to sample and trace a subset of incoming requests
-- with
-- <https://docs.aws.amazon.com/lambda/latest/dg/services-xray.html X-Ray>.
createFunction_tracingConfig :: Lens.Lens' CreateFunction (Prelude.Maybe TracingConfig)
createFunction_tracingConfig = Lens.lens (\CreateFunction' {tracingConfig} -> tracingConfig) (\s@CreateFunction' {} a -> s {tracingConfig = a} :: CreateFunction)

-- | For network connectivity to Amazon Web Services resources in a VPC,
-- specify a list of security groups and subnets in the VPC. When you
-- connect a function to a VPC, it can access resources and the internet
-- only through that VPC. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html Configuring a Lambda function to access resources in a VPC>.
createFunction_vpcConfig :: Lens.Lens' CreateFunction (Prelude.Maybe VpcConfig)
createFunction_vpcConfig = Lens.lens (\CreateFunction' {vpcConfig} -> vpcConfig) (\s@CreateFunction' {} a -> s {vpcConfig = a} :: CreateFunction)

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@.
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
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
    _salt
      `Prelude.hashWithSalt` architectures
      `Prelude.hashWithSalt` codeSigningConfigArn
      `Prelude.hashWithSalt` deadLetterConfig
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` ephemeralStorage
      `Prelude.hashWithSalt` fileSystemConfigs
      `Prelude.hashWithSalt` handler
      `Prelude.hashWithSalt` imageConfig
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` layers
      `Prelude.hashWithSalt` memorySize
      `Prelude.hashWithSalt` packageType
      `Prelude.hashWithSalt` publish
      `Prelude.hashWithSalt` runtime
      `Prelude.hashWithSalt` snapStart
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` tracingConfig
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` functionName
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` code

instance Prelude.NFData CreateFunction where
  rnf CreateFunction' {..} =
    Prelude.rnf architectures
      `Prelude.seq` Prelude.rnf codeSigningConfigArn
      `Prelude.seq` Prelude.rnf deadLetterConfig
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf ephemeralStorage
      `Prelude.seq` Prelude.rnf fileSystemConfigs
      `Prelude.seq` Prelude.rnf handler
      `Prelude.seq` Prelude.rnf imageConfig
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf layers
      `Prelude.seq` Prelude.rnf memorySize
      `Prelude.seq` Prelude.rnf packageType
      `Prelude.seq` Prelude.rnf publish
      `Prelude.seq` Prelude.rnf runtime
      `Prelude.seq` Prelude.rnf snapStart
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf tracingConfig
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf functionName
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf code

instance Data.ToHeaders CreateFunction where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateFunction where
  toJSON CreateFunction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Architectures" Data..=) Prelude.<$> architectures,
            ("CodeSigningConfigArn" Data..=)
              Prelude.<$> codeSigningConfigArn,
            ("DeadLetterConfig" Data..=)
              Prelude.<$> deadLetterConfig,
            ("Description" Data..=) Prelude.<$> description,
            ("Environment" Data..=) Prelude.<$> environment,
            ("EphemeralStorage" Data..=)
              Prelude.<$> ephemeralStorage,
            ("FileSystemConfigs" Data..=)
              Prelude.<$> fileSystemConfigs,
            ("Handler" Data..=) Prelude.<$> handler,
            ("ImageConfig" Data..=) Prelude.<$> imageConfig,
            ("KMSKeyArn" Data..=) Prelude.<$> kmsKeyArn,
            ("Layers" Data..=) Prelude.<$> layers,
            ("MemorySize" Data..=) Prelude.<$> memorySize,
            ("PackageType" Data..=) Prelude.<$> packageType,
            ("Publish" Data..=) Prelude.<$> publish,
            ("Runtime" Data..=) Prelude.<$> runtime,
            ("SnapStart" Data..=) Prelude.<$> snapStart,
            ("Tags" Data..=) Prelude.<$> tags,
            ("Timeout" Data..=) Prelude.<$> timeout,
            ("TracingConfig" Data..=) Prelude.<$> tracingConfig,
            ("VpcConfig" Data..=) Prelude.<$> vpcConfig,
            Prelude.Just ("FunctionName" Data..= functionName),
            Prelude.Just ("Role" Data..= role'),
            Prelude.Just ("Code" Data..= code)
          ]
      )

instance Data.ToPath CreateFunction where
  toPath = Prelude.const "/2015-03-31/functions"

instance Data.ToQuery CreateFunction where
  toQuery = Prelude.const Prelude.mempty
