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
-- Module      : Amazonka.Lambda.UpdateFunctionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Lambda.UpdateFunctionConfiguration
  ( -- * Creating a Request
    UpdateFunctionConfiguration (..),
    newUpdateFunctionConfiguration,

    -- * Request Lenses
    updateFunctionConfiguration_tracingConfig,
    updateFunctionConfiguration_fileSystemConfigs,
    updateFunctionConfiguration_timeout,
    updateFunctionConfiguration_ephemeralStorage,
    updateFunctionConfiguration_memorySize,
    updateFunctionConfiguration_imageConfig,
    updateFunctionConfiguration_environment,
    updateFunctionConfiguration_vpcConfig,
    updateFunctionConfiguration_runtime,
    updateFunctionConfiguration_description,
    updateFunctionConfiguration_kmsKeyArn,
    updateFunctionConfiguration_handler,
    updateFunctionConfiguration_layers,
    updateFunctionConfiguration_revisionId,
    updateFunctionConfiguration_role,
    updateFunctionConfiguration_deadLetterConfig,
    updateFunctionConfiguration_functionName,

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
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFunctionConfiguration' smart constructor.
data UpdateFunctionConfiguration = UpdateFunctionConfiguration'
  { -- | Set @Mode@ to @Active@ to sample and trace a subset of incoming requests
    -- with
    -- <https://docs.aws.amazon.com/lambda/latest/dg/services-xray.html X-Ray>.
    tracingConfig :: Prelude.Maybe TracingConfig,
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
    -- | <https://docs.aws.amazon.com/lambda/latest/dg/images-parms.html Container image configuration values>
    -- that override the values in the container image Docker file.
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
    -- | Only update the function if the revision ID matches the ID that\'s
    -- specified. Use this option to avoid modifying a function that has
    -- changed since you last read it.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the function\'s execution role.
    role' :: Prelude.Maybe Prelude.Text,
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
-- 'tracingConfig', 'updateFunctionConfiguration_tracingConfig' - Set @Mode@ to @Active@ to sample and trace a subset of incoming requests
-- with
-- <https://docs.aws.amazon.com/lambda/latest/dg/services-xray.html X-Ray>.
--
-- 'fileSystemConfigs', 'updateFunctionConfiguration_fileSystemConfigs' - Connection settings for an Amazon EFS file system.
--
-- 'timeout', 'updateFunctionConfiguration_timeout' - The amount of time (in seconds) that Lambda allows a function to run
-- before stopping it. The default is 3 seconds. The maximum allowed value
-- is 900 seconds. For additional information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-context.html Lambda execution environment>.
--
-- 'ephemeralStorage', 'updateFunctionConfiguration_ephemeralStorage' - The size of the function’s \/tmp directory in MB. The default value is
-- 512, but can be any whole number between 512 and 10240 MB.
--
-- 'memorySize', 'updateFunctionConfiguration_memorySize' - The amount of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-memory.html memory available to the function>
-- at runtime. Increasing the function memory also increases its CPU
-- allocation. The default value is 128 MB. The value can be any multiple
-- of 1 MB.
--
-- 'imageConfig', 'updateFunctionConfiguration_imageConfig' - <https://docs.aws.amazon.com/lambda/latest/dg/images-parms.html Container image configuration values>
-- that override the values in the container image Docker file.
--
-- 'environment', 'updateFunctionConfiguration_environment' - Environment variables that are accessible from function code during
-- execution.
--
-- 'vpcConfig', 'updateFunctionConfiguration_vpcConfig' - For network connectivity to Amazon Web Services resources in a VPC,
-- specify a list of security groups and subnets in the VPC. When you
-- connect a function to a VPC, it can only access resources and the
-- internet through that VPC. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings>.
--
-- 'runtime', 'updateFunctionConfiguration_runtime' - The identifier of the function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime>.
-- Runtime is required if the deployment package is a .zip file archive.
--
-- 'description', 'updateFunctionConfiguration_description' - A description of the function.
--
-- 'kmsKeyArn', 'updateFunctionConfiguration_kmsKeyArn' - The ARN of the Amazon Web Services Key Management Service (KMS) key
-- that\'s used to encrypt your function\'s environment variables. If it\'s
-- not provided, Lambda uses a default service key.
--
-- 'handler', 'updateFunctionConfiguration_handler' - The name of the method within your code that Lambda calls to execute
-- your function. Handler is required if the deployment package is a .zip
-- file archive. The format includes the file name. It can also include
-- namespaces and other qualifiers, depending on the runtime. For more
-- information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model>.
--
-- 'layers', 'updateFunctionConfiguration_layers' - A list of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers>
-- to add to the function\'s execution environment. Specify each layer by
-- its ARN, including the version.
--
-- 'revisionId', 'updateFunctionConfiguration_revisionId' - Only update the function if the revision ID matches the ID that\'s
-- specified. Use this option to avoid modifying a function that has
-- changed since you last read it.
--
-- 'role'', 'updateFunctionConfiguration_role' - The Amazon Resource Name (ARN) of the function\'s execution role.
--
-- 'deadLetterConfig', 'updateFunctionConfiguration_deadLetterConfig' - A dead letter queue configuration that specifies the queue or topic
-- where Lambda sends asynchronous events when they fail processing. For
-- more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq Dead Letter Queues>.
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
    { tracingConfig =
        Prelude.Nothing,
      fileSystemConfigs = Prelude.Nothing,
      timeout = Prelude.Nothing,
      ephemeralStorage = Prelude.Nothing,
      memorySize = Prelude.Nothing,
      imageConfig = Prelude.Nothing,
      environment = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      runtime = Prelude.Nothing,
      description = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      handler = Prelude.Nothing,
      layers = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      role' = Prelude.Nothing,
      deadLetterConfig = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | Set @Mode@ to @Active@ to sample and trace a subset of incoming requests
-- with
-- <https://docs.aws.amazon.com/lambda/latest/dg/services-xray.html X-Ray>.
updateFunctionConfiguration_tracingConfig :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe TracingConfig)
updateFunctionConfiguration_tracingConfig = Lens.lens (\UpdateFunctionConfiguration' {tracingConfig} -> tracingConfig) (\s@UpdateFunctionConfiguration' {} a -> s {tracingConfig = a} :: UpdateFunctionConfiguration)

-- | Connection settings for an Amazon EFS file system.
updateFunctionConfiguration_fileSystemConfigs :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe [FileSystemConfig])
updateFunctionConfiguration_fileSystemConfigs = Lens.lens (\UpdateFunctionConfiguration' {fileSystemConfigs} -> fileSystemConfigs) (\s@UpdateFunctionConfiguration' {} a -> s {fileSystemConfigs = a} :: UpdateFunctionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The amount of time (in seconds) that Lambda allows a function to run
-- before stopping it. The default is 3 seconds. The maximum allowed value
-- is 900 seconds. For additional information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-context.html Lambda execution environment>.
updateFunctionConfiguration_timeout :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Prelude.Natural)
updateFunctionConfiguration_timeout = Lens.lens (\UpdateFunctionConfiguration' {timeout} -> timeout) (\s@UpdateFunctionConfiguration' {} a -> s {timeout = a} :: UpdateFunctionConfiguration)

-- | The size of the function’s \/tmp directory in MB. The default value is
-- 512, but can be any whole number between 512 and 10240 MB.
updateFunctionConfiguration_ephemeralStorage :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe EphemeralStorage)
updateFunctionConfiguration_ephemeralStorage = Lens.lens (\UpdateFunctionConfiguration' {ephemeralStorage} -> ephemeralStorage) (\s@UpdateFunctionConfiguration' {} a -> s {ephemeralStorage = a} :: UpdateFunctionConfiguration)

-- | The amount of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-memory.html memory available to the function>
-- at runtime. Increasing the function memory also increases its CPU
-- allocation. The default value is 128 MB. The value can be any multiple
-- of 1 MB.
updateFunctionConfiguration_memorySize :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Prelude.Natural)
updateFunctionConfiguration_memorySize = Lens.lens (\UpdateFunctionConfiguration' {memorySize} -> memorySize) (\s@UpdateFunctionConfiguration' {} a -> s {memorySize = a} :: UpdateFunctionConfiguration)

-- | <https://docs.aws.amazon.com/lambda/latest/dg/images-parms.html Container image configuration values>
-- that override the values in the container image Docker file.
updateFunctionConfiguration_imageConfig :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe ImageConfig)
updateFunctionConfiguration_imageConfig = Lens.lens (\UpdateFunctionConfiguration' {imageConfig} -> imageConfig) (\s@UpdateFunctionConfiguration' {} a -> s {imageConfig = a} :: UpdateFunctionConfiguration)

-- | Environment variables that are accessible from function code during
-- execution.
updateFunctionConfiguration_environment :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Environment)
updateFunctionConfiguration_environment = Lens.lens (\UpdateFunctionConfiguration' {environment} -> environment) (\s@UpdateFunctionConfiguration' {} a -> s {environment = a} :: UpdateFunctionConfiguration)

-- | For network connectivity to Amazon Web Services resources in a VPC,
-- specify a list of security groups and subnets in the VPC. When you
-- connect a function to a VPC, it can only access resources and the
-- internet through that VPC. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings>.
updateFunctionConfiguration_vpcConfig :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe VpcConfig)
updateFunctionConfiguration_vpcConfig = Lens.lens (\UpdateFunctionConfiguration' {vpcConfig} -> vpcConfig) (\s@UpdateFunctionConfiguration' {} a -> s {vpcConfig = a} :: UpdateFunctionConfiguration)

-- | The identifier of the function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime>.
-- Runtime is required if the deployment package is a .zip file archive.
updateFunctionConfiguration_runtime :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Runtime)
updateFunctionConfiguration_runtime = Lens.lens (\UpdateFunctionConfiguration' {runtime} -> runtime) (\s@UpdateFunctionConfiguration' {} a -> s {runtime = a} :: UpdateFunctionConfiguration)

-- | A description of the function.
updateFunctionConfiguration_description :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Prelude.Text)
updateFunctionConfiguration_description = Lens.lens (\UpdateFunctionConfiguration' {description} -> description) (\s@UpdateFunctionConfiguration' {} a -> s {description = a} :: UpdateFunctionConfiguration)

-- | The ARN of the Amazon Web Services Key Management Service (KMS) key
-- that\'s used to encrypt your function\'s environment variables. If it\'s
-- not provided, Lambda uses a default service key.
updateFunctionConfiguration_kmsKeyArn :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Prelude.Text)
updateFunctionConfiguration_kmsKeyArn = Lens.lens (\UpdateFunctionConfiguration' {kmsKeyArn} -> kmsKeyArn) (\s@UpdateFunctionConfiguration' {} a -> s {kmsKeyArn = a} :: UpdateFunctionConfiguration)

-- | The name of the method within your code that Lambda calls to execute
-- your function. Handler is required if the deployment package is a .zip
-- file archive. The format includes the file name. It can also include
-- namespaces and other qualifiers, depending on the runtime. For more
-- information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model>.
updateFunctionConfiguration_handler :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Prelude.Text)
updateFunctionConfiguration_handler = Lens.lens (\UpdateFunctionConfiguration' {handler} -> handler) (\s@UpdateFunctionConfiguration' {} a -> s {handler = a} :: UpdateFunctionConfiguration)

-- | A list of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers>
-- to add to the function\'s execution environment. Specify each layer by
-- its ARN, including the version.
updateFunctionConfiguration_layers :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe [Prelude.Text])
updateFunctionConfiguration_layers = Lens.lens (\UpdateFunctionConfiguration' {layers} -> layers) (\s@UpdateFunctionConfiguration' {} a -> s {layers = a} :: UpdateFunctionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Only update the function if the revision ID matches the ID that\'s
-- specified. Use this option to avoid modifying a function that has
-- changed since you last read it.
updateFunctionConfiguration_revisionId :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Prelude.Text)
updateFunctionConfiguration_revisionId = Lens.lens (\UpdateFunctionConfiguration' {revisionId} -> revisionId) (\s@UpdateFunctionConfiguration' {} a -> s {revisionId = a} :: UpdateFunctionConfiguration)

-- | The Amazon Resource Name (ARN) of the function\'s execution role.
updateFunctionConfiguration_role :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Prelude.Text)
updateFunctionConfiguration_role = Lens.lens (\UpdateFunctionConfiguration' {role'} -> role') (\s@UpdateFunctionConfiguration' {} a -> s {role' = a} :: UpdateFunctionConfiguration)

-- | A dead letter queue configuration that specifies the queue or topic
-- where Lambda sends asynchronous events when they fail processing. For
-- more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq Dead Letter Queues>.
updateFunctionConfiguration_deadLetterConfig :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe DeadLetterConfig)
updateFunctionConfiguration_deadLetterConfig = Lens.lens (\UpdateFunctionConfiguration' {deadLetterConfig} -> deadLetterConfig) (\s@UpdateFunctionConfiguration' {} a -> s {deadLetterConfig = a} :: UpdateFunctionConfiguration)

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
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateFunctionConfiguration where
  hashWithSalt _salt UpdateFunctionConfiguration' {..} =
    _salt `Prelude.hashWithSalt` tracingConfig
      `Prelude.hashWithSalt` fileSystemConfigs
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` ephemeralStorage
      `Prelude.hashWithSalt` memorySize
      `Prelude.hashWithSalt` imageConfig
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` runtime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` handler
      `Prelude.hashWithSalt` layers
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` deadLetterConfig
      `Prelude.hashWithSalt` functionName

instance Prelude.NFData UpdateFunctionConfiguration where
  rnf UpdateFunctionConfiguration' {..} =
    Prelude.rnf tracingConfig
      `Prelude.seq` Prelude.rnf fileSystemConfigs
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf ephemeralStorage
      `Prelude.seq` Prelude.rnf memorySize
      `Prelude.seq` Prelude.rnf imageConfig
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf runtime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf handler
      `Prelude.seq` Prelude.rnf layers
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf deadLetterConfig
      `Prelude.seq` Prelude.rnf functionName

instance Core.ToHeaders UpdateFunctionConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateFunctionConfiguration where
  toJSON UpdateFunctionConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TracingConfig" Core..=) Prelude.<$> tracingConfig,
            ("FileSystemConfigs" Core..=)
              Prelude.<$> fileSystemConfigs,
            ("Timeout" Core..=) Prelude.<$> timeout,
            ("EphemeralStorage" Core..=)
              Prelude.<$> ephemeralStorage,
            ("MemorySize" Core..=) Prelude.<$> memorySize,
            ("ImageConfig" Core..=) Prelude.<$> imageConfig,
            ("Environment" Core..=) Prelude.<$> environment,
            ("VpcConfig" Core..=) Prelude.<$> vpcConfig,
            ("Runtime" Core..=) Prelude.<$> runtime,
            ("Description" Core..=) Prelude.<$> description,
            ("KMSKeyArn" Core..=) Prelude.<$> kmsKeyArn,
            ("Handler" Core..=) Prelude.<$> handler,
            ("Layers" Core..=) Prelude.<$> layers,
            ("RevisionId" Core..=) Prelude.<$> revisionId,
            ("Role" Core..=) Prelude.<$> role',
            ("DeadLetterConfig" Core..=)
              Prelude.<$> deadLetterConfig
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
