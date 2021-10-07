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
-- Module      : Network.AWS.Lambda.UpdateFunctionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify the version-specific settings of a Lambda function.
--
-- When you update a function, Lambda provisions an instance of the
-- function and its supporting resources. If your function connects to a
-- VPC, this process can take a minute. During this time, you can\'t modify
-- the function, but you can still invoke it. The @LastUpdateStatus@,
-- @LastUpdateStatusReason@, and @LastUpdateStatusReasonCode@ fields in the
-- response from GetFunctionConfiguration indicate when the update is
-- complete and the function is processing events with the new
-- configuration. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/functions-states.html Function States>.
--
-- These settings can vary between versions of a function and are locked
-- when you publish a version. You can\'t modify the configuration of a
-- published version, only the unpublished version.
--
-- To configure function concurrency, use PutFunctionConcurrency. To grant
-- invoke permissions to an account or Amazon Web Services service, use
-- AddPermission.
module Network.AWS.Lambda.UpdateFunctionConfiguration
  ( -- * Creating a Request
    UpdateFunctionConfiguration (..),
    newUpdateFunctionConfiguration,

    -- * Request Lenses
    updateFunctionConfiguration_vpcConfig,
    updateFunctionConfiguration_memorySize,
    updateFunctionConfiguration_revisionId,
    updateFunctionConfiguration_timeout,
    updateFunctionConfiguration_handler,
    updateFunctionConfiguration_deadLetterConfig,
    updateFunctionConfiguration_imageConfig,
    updateFunctionConfiguration_environment,
    updateFunctionConfiguration_kmsKeyArn,
    updateFunctionConfiguration_runtime,
    updateFunctionConfiguration_role,
    updateFunctionConfiguration_description,
    updateFunctionConfiguration_tracingConfig,
    updateFunctionConfiguration_layers,
    updateFunctionConfiguration_fileSystemConfigs,
    updateFunctionConfiguration_functionName,

    -- * Destructuring the Response
    FunctionConfiguration (..),
    newFunctionConfiguration,

    -- * Response Lenses
    functionConfiguration_vpcConfig,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_memorySize,
    functionConfiguration_masterArn,
    functionConfiguration_revisionId,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_codeSha256,
    functionConfiguration_stateReason,
    functionConfiguration_timeout,
    functionConfiguration_handler,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_environment,
    functionConfiguration_functionName,
    functionConfiguration_version,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_state,
    functionConfiguration_functionArn,
    functionConfiguration_runtime,
    functionConfiguration_role,
    functionConfiguration_signingJobArn,
    functionConfiguration_stateReasonCode,
    functionConfiguration_description,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_tracingConfig,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_lastModified,
    functionConfiguration_codeSize,
    functionConfiguration_layers,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_packageType,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateFunctionConfiguration' smart constructor.
data UpdateFunctionConfiguration = UpdateFunctionConfiguration'
  { -- | For network connectivity to Amazon Web Services resources in a VPC,
    -- specify a list of security groups and subnets in the VPC. When you
    -- connect a function to a VPC, it can only access resources and the
    -- internet through that VPC. For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The amount of
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-memory.html memory available to the function>
    -- at runtime. Increasing the function memory also increases its CPU
    -- allocation. The default value is 128 MB. The value can be any multiple
    -- of 1 MB.
    memorySize :: Prelude.Maybe Prelude.Natural,
    -- | Only update the function if the revision ID matches the ID that\'s
    -- specified. Use this option to avoid modifying a function that has
    -- changed since you last read it.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The amount of time that Lambda allows a function to run before stopping
    -- it. The default is 3 seconds. The maximum allowed value is 900 seconds.
    -- For additional information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-context.html Lambda execution environment>.
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The name of the method within your code that Lambda calls to execute
    -- your function. The format includes the file name. It can also include
    -- namespaces and other qualifiers, depending on the runtime. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model>.
    handler :: Prelude.Maybe Prelude.Text,
    -- | A dead letter queue configuration that specifies the queue or topic
    -- where Lambda sends asynchronous events when they fail processing. For
    -- more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq Dead Letter Queues>.
    deadLetterConfig :: Prelude.Maybe DeadLetterConfig,
    -- | <https://docs.aws.amazon.com/lambda/latest/dg/images-parms.html Container image configuration values>
    -- that override the values in the container image Dockerfile.
    imageConfig :: Prelude.Maybe ImageConfig,
    -- | Environment variables that are accessible from function code during
    -- execution.
    environment :: Prelude.Maybe Environment,
    -- | The ARN of the Amazon Web Services Key Management Service (KMS) key
    -- that\'s used to encrypt your function\'s environment variables. If it\'s
    -- not provided, Lambda uses a default service key.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the function\'s
    -- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime>.
    runtime :: Prelude.Maybe Runtime,
    -- | The Amazon Resource Name (ARN) of the function\'s execution role.
    role' :: Prelude.Maybe Prelude.Text,
    -- | A description of the function.
    description :: Prelude.Maybe Prelude.Text,
    -- | Set @Mode@ to @Active@ to sample and trace a subset of incoming requests
    -- with
    -- <https://docs.aws.amazon.com/lambda/latest/dg/services-xray.html X-Ray>.
    tracingConfig :: Prelude.Maybe TracingConfig,
    -- | A list of
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers>
    -- to add to the function\'s execution environment. Specify each layer by
    -- its ARN, including the version.
    layers :: Prelude.Maybe [Prelude.Text],
    -- | Connection settings for an Amazon EFS file system.
    fileSystemConfigs :: Prelude.Maybe [FileSystemConfig],
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
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFunctionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'updateFunctionConfiguration_vpcConfig' - For network connectivity to Amazon Web Services resources in a VPC,
-- specify a list of security groups and subnets in the VPC. When you
-- connect a function to a VPC, it can only access resources and the
-- internet through that VPC. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings>.
--
-- 'memorySize', 'updateFunctionConfiguration_memorySize' - The amount of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-memory.html memory available to the function>
-- at runtime. Increasing the function memory also increases its CPU
-- allocation. The default value is 128 MB. The value can be any multiple
-- of 1 MB.
--
-- 'revisionId', 'updateFunctionConfiguration_revisionId' - Only update the function if the revision ID matches the ID that\'s
-- specified. Use this option to avoid modifying a function that has
-- changed since you last read it.
--
-- 'timeout', 'updateFunctionConfiguration_timeout' - The amount of time that Lambda allows a function to run before stopping
-- it. The default is 3 seconds. The maximum allowed value is 900 seconds.
-- For additional information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-context.html Lambda execution environment>.
--
-- 'handler', 'updateFunctionConfiguration_handler' - The name of the method within your code that Lambda calls to execute
-- your function. The format includes the file name. It can also include
-- namespaces and other qualifiers, depending on the runtime. For more
-- information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model>.
--
-- 'deadLetterConfig', 'updateFunctionConfiguration_deadLetterConfig' - A dead letter queue configuration that specifies the queue or topic
-- where Lambda sends asynchronous events when they fail processing. For
-- more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq Dead Letter Queues>.
--
-- 'imageConfig', 'updateFunctionConfiguration_imageConfig' - <https://docs.aws.amazon.com/lambda/latest/dg/images-parms.html Container image configuration values>
-- that override the values in the container image Dockerfile.
--
-- 'environment', 'updateFunctionConfiguration_environment' - Environment variables that are accessible from function code during
-- execution.
--
-- 'kmsKeyArn', 'updateFunctionConfiguration_kmsKeyArn' - The ARN of the Amazon Web Services Key Management Service (KMS) key
-- that\'s used to encrypt your function\'s environment variables. If it\'s
-- not provided, Lambda uses a default service key.
--
-- 'runtime', 'updateFunctionConfiguration_runtime' - The identifier of the function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime>.
--
-- 'role'', 'updateFunctionConfiguration_role' - The Amazon Resource Name (ARN) of the function\'s execution role.
--
-- 'description', 'updateFunctionConfiguration_description' - A description of the function.
--
-- 'tracingConfig', 'updateFunctionConfiguration_tracingConfig' - Set @Mode@ to @Active@ to sample and trace a subset of incoming requests
-- with
-- <https://docs.aws.amazon.com/lambda/latest/dg/services-xray.html X-Ray>.
--
-- 'layers', 'updateFunctionConfiguration_layers' - A list of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers>
-- to add to the function\'s execution environment. Specify each layer by
-- its ARN, including the version.
--
-- 'fileSystemConfigs', 'updateFunctionConfiguration_fileSystemConfigs' - Connection settings for an Amazon EFS file system.
--
-- 'functionName', 'updateFunctionConfiguration_functionName' - The name of the Lambda function.
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
newUpdateFunctionConfiguration ::
  -- | 'functionName'
  Prelude.Text ->
  UpdateFunctionConfiguration
newUpdateFunctionConfiguration pFunctionName_ =
  UpdateFunctionConfiguration'
    { vpcConfig =
        Prelude.Nothing,
      memorySize = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      timeout = Prelude.Nothing,
      handler = Prelude.Nothing,
      deadLetterConfig = Prelude.Nothing,
      imageConfig = Prelude.Nothing,
      environment = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      runtime = Prelude.Nothing,
      role' = Prelude.Nothing,
      description = Prelude.Nothing,
      tracingConfig = Prelude.Nothing,
      layers = Prelude.Nothing,
      fileSystemConfigs = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | For network connectivity to Amazon Web Services resources in a VPC,
-- specify a list of security groups and subnets in the VPC. When you
-- connect a function to a VPC, it can only access resources and the
-- internet through that VPC. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings>.
updateFunctionConfiguration_vpcConfig :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe VpcConfig)
updateFunctionConfiguration_vpcConfig = Lens.lens (\UpdateFunctionConfiguration' {vpcConfig} -> vpcConfig) (\s@UpdateFunctionConfiguration' {} a -> s {vpcConfig = a} :: UpdateFunctionConfiguration)

-- | The amount of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-memory.html memory available to the function>
-- at runtime. Increasing the function memory also increases its CPU
-- allocation. The default value is 128 MB. The value can be any multiple
-- of 1 MB.
updateFunctionConfiguration_memorySize :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Prelude.Natural)
updateFunctionConfiguration_memorySize = Lens.lens (\UpdateFunctionConfiguration' {memorySize} -> memorySize) (\s@UpdateFunctionConfiguration' {} a -> s {memorySize = a} :: UpdateFunctionConfiguration)

-- | Only update the function if the revision ID matches the ID that\'s
-- specified. Use this option to avoid modifying a function that has
-- changed since you last read it.
updateFunctionConfiguration_revisionId :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Prelude.Text)
updateFunctionConfiguration_revisionId = Lens.lens (\UpdateFunctionConfiguration' {revisionId} -> revisionId) (\s@UpdateFunctionConfiguration' {} a -> s {revisionId = a} :: UpdateFunctionConfiguration)

-- | The amount of time that Lambda allows a function to run before stopping
-- it. The default is 3 seconds. The maximum allowed value is 900 seconds.
-- For additional information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-context.html Lambda execution environment>.
updateFunctionConfiguration_timeout :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Prelude.Natural)
updateFunctionConfiguration_timeout = Lens.lens (\UpdateFunctionConfiguration' {timeout} -> timeout) (\s@UpdateFunctionConfiguration' {} a -> s {timeout = a} :: UpdateFunctionConfiguration)

-- | The name of the method within your code that Lambda calls to execute
-- your function. The format includes the file name. It can also include
-- namespaces and other qualifiers, depending on the runtime. For more
-- information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model>.
updateFunctionConfiguration_handler :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Prelude.Text)
updateFunctionConfiguration_handler = Lens.lens (\UpdateFunctionConfiguration' {handler} -> handler) (\s@UpdateFunctionConfiguration' {} a -> s {handler = a} :: UpdateFunctionConfiguration)

-- | A dead letter queue configuration that specifies the queue or topic
-- where Lambda sends asynchronous events when they fail processing. For
-- more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq Dead Letter Queues>.
updateFunctionConfiguration_deadLetterConfig :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe DeadLetterConfig)
updateFunctionConfiguration_deadLetterConfig = Lens.lens (\UpdateFunctionConfiguration' {deadLetterConfig} -> deadLetterConfig) (\s@UpdateFunctionConfiguration' {} a -> s {deadLetterConfig = a} :: UpdateFunctionConfiguration)

-- | <https://docs.aws.amazon.com/lambda/latest/dg/images-parms.html Container image configuration values>
-- that override the values in the container image Dockerfile.
updateFunctionConfiguration_imageConfig :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe ImageConfig)
updateFunctionConfiguration_imageConfig = Lens.lens (\UpdateFunctionConfiguration' {imageConfig} -> imageConfig) (\s@UpdateFunctionConfiguration' {} a -> s {imageConfig = a} :: UpdateFunctionConfiguration)

-- | Environment variables that are accessible from function code during
-- execution.
updateFunctionConfiguration_environment :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Environment)
updateFunctionConfiguration_environment = Lens.lens (\UpdateFunctionConfiguration' {environment} -> environment) (\s@UpdateFunctionConfiguration' {} a -> s {environment = a} :: UpdateFunctionConfiguration)

-- | The ARN of the Amazon Web Services Key Management Service (KMS) key
-- that\'s used to encrypt your function\'s environment variables. If it\'s
-- not provided, Lambda uses a default service key.
updateFunctionConfiguration_kmsKeyArn :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Prelude.Text)
updateFunctionConfiguration_kmsKeyArn = Lens.lens (\UpdateFunctionConfiguration' {kmsKeyArn} -> kmsKeyArn) (\s@UpdateFunctionConfiguration' {} a -> s {kmsKeyArn = a} :: UpdateFunctionConfiguration)

-- | The identifier of the function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime>.
updateFunctionConfiguration_runtime :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Runtime)
updateFunctionConfiguration_runtime = Lens.lens (\UpdateFunctionConfiguration' {runtime} -> runtime) (\s@UpdateFunctionConfiguration' {} a -> s {runtime = a} :: UpdateFunctionConfiguration)

-- | The Amazon Resource Name (ARN) of the function\'s execution role.
updateFunctionConfiguration_role :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Prelude.Text)
updateFunctionConfiguration_role = Lens.lens (\UpdateFunctionConfiguration' {role'} -> role') (\s@UpdateFunctionConfiguration' {} a -> s {role' = a} :: UpdateFunctionConfiguration)

-- | A description of the function.
updateFunctionConfiguration_description :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Prelude.Text)
updateFunctionConfiguration_description = Lens.lens (\UpdateFunctionConfiguration' {description} -> description) (\s@UpdateFunctionConfiguration' {} a -> s {description = a} :: UpdateFunctionConfiguration)

-- | Set @Mode@ to @Active@ to sample and trace a subset of incoming requests
-- with
-- <https://docs.aws.amazon.com/lambda/latest/dg/services-xray.html X-Ray>.
updateFunctionConfiguration_tracingConfig :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe TracingConfig)
updateFunctionConfiguration_tracingConfig = Lens.lens (\UpdateFunctionConfiguration' {tracingConfig} -> tracingConfig) (\s@UpdateFunctionConfiguration' {} a -> s {tracingConfig = a} :: UpdateFunctionConfiguration)

-- | A list of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers>
-- to add to the function\'s execution environment. Specify each layer by
-- its ARN, including the version.
updateFunctionConfiguration_layers :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe [Prelude.Text])
updateFunctionConfiguration_layers = Lens.lens (\UpdateFunctionConfiguration' {layers} -> layers) (\s@UpdateFunctionConfiguration' {} a -> s {layers = a} :: UpdateFunctionConfiguration) Prelude.. Lens.mapping Lens._Coerce

-- | Connection settings for an Amazon EFS file system.
updateFunctionConfiguration_fileSystemConfigs :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe [FileSystemConfig])
updateFunctionConfiguration_fileSystemConfigs = Lens.lens (\UpdateFunctionConfiguration' {fileSystemConfigs} -> fileSystemConfigs) (\s@UpdateFunctionConfiguration' {} a -> s {fileSystemConfigs = a} :: UpdateFunctionConfiguration) Prelude.. Lens.mapping Lens._Coerce

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
updateFunctionConfiguration_functionName :: Lens.Lens' UpdateFunctionConfiguration Prelude.Text
updateFunctionConfiguration_functionName = Lens.lens (\UpdateFunctionConfiguration' {functionName} -> functionName) (\s@UpdateFunctionConfiguration' {} a -> s {functionName = a} :: UpdateFunctionConfiguration)

instance Core.AWSRequest UpdateFunctionConfiguration where
  type
    AWSResponse UpdateFunctionConfiguration =
      FunctionConfiguration
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateFunctionConfiguration

instance Prelude.NFData UpdateFunctionConfiguration

instance Core.ToHeaders UpdateFunctionConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateFunctionConfiguration where
  toJSON UpdateFunctionConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VpcConfig" Core..=) Prelude.<$> vpcConfig,
            ("MemorySize" Core..=) Prelude.<$> memorySize,
            ("RevisionId" Core..=) Prelude.<$> revisionId,
            ("Timeout" Core..=) Prelude.<$> timeout,
            ("Handler" Core..=) Prelude.<$> handler,
            ("DeadLetterConfig" Core..=)
              Prelude.<$> deadLetterConfig,
            ("ImageConfig" Core..=) Prelude.<$> imageConfig,
            ("Environment" Core..=) Prelude.<$> environment,
            ("KMSKeyArn" Core..=) Prelude.<$> kmsKeyArn,
            ("Runtime" Core..=) Prelude.<$> runtime,
            ("Role" Core..=) Prelude.<$> role',
            ("Description" Core..=) Prelude.<$> description,
            ("TracingConfig" Core..=) Prelude.<$> tracingConfig,
            ("Layers" Core..=) Prelude.<$> layers,
            ("FileSystemConfigs" Core..=)
              Prelude.<$> fileSystemConfigs
          ]
      )

instance Core.ToPath UpdateFunctionConfiguration where
  toPath UpdateFunctionConfiguration' {..} =
    Prelude.mconcat
      [ "/2015-03-31/functions/",
        Core.toBS functionName,
        "/configuration"
      ]

instance Core.ToQuery UpdateFunctionConfiguration where
  toQuery = Prelude.const Prelude.mempty
