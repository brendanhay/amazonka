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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- <https://docs.aws.amazon.com/lambda/latest/dg/functions-states.html Lambda function states>.
--
-- These settings can vary between versions of a function and are locked
-- when you publish a version. You can\'t modify the configuration of a
-- published version, only the unpublished version.
--
-- To configure function concurrency, use PutFunctionConcurrency. To grant
-- invoke permissions to an Amazon Web Services account or Amazon Web
-- Service, use AddPermission.
module Amazonka.Lambda.UpdateFunctionConfiguration
  ( -- * Creating a Request
    UpdateFunctionConfiguration (..),
    newUpdateFunctionConfiguration,

    -- * Request Lenses
    updateFunctionConfiguration_deadLetterConfig,
    updateFunctionConfiguration_description,
    updateFunctionConfiguration_environment,
    updateFunctionConfiguration_ephemeralStorage,
    updateFunctionConfiguration_fileSystemConfigs,
    updateFunctionConfiguration_handler,
    updateFunctionConfiguration_imageConfig,
    updateFunctionConfiguration_kmsKeyArn,
    updateFunctionConfiguration_layers,
    updateFunctionConfiguration_memorySize,
    updateFunctionConfiguration_revisionId,
    updateFunctionConfiguration_role,
    updateFunctionConfiguration_runtime,
    updateFunctionConfiguration_snapStart,
    updateFunctionConfiguration_timeout,
    updateFunctionConfiguration_tracingConfig,
    updateFunctionConfiguration_vpcConfig,
    updateFunctionConfiguration_functionName,

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

-- | /See:/ 'newUpdateFunctionConfiguration' smart constructor.
data UpdateFunctionConfiguration = UpdateFunctionConfiguration'
  { -- | A dead-letter queue configuration that specifies the queue or topic
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
    -- | <https://docs.aws.amazon.com/lambda/latest/dg/images-parms.html Container image configuration values>
    -- that override the values in the container image Docker file.
    imageConfig :: Prelude.Maybe ImageConfig,
    -- | The ARN of the Key Management Service (KMS) key that\'s used to encrypt
    -- your function\'s environment variables. If it\'s not provided, Lambda
    -- uses a default service key.
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
    -- | Update the function only if the revision ID matches the ID that\'s
    -- specified. Use this option to avoid modifying a function that has
    -- changed since you last read it.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the function\'s execution role.
    role' :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the function\'s
    -- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime>.
    -- Runtime is required if the deployment package is a .zip file archive.
    runtime :: Prelude.Maybe Runtime,
    -- | The function\'s
    -- <https://docs.aws.amazon.com/lambda/latest/dg/snapstart.html SnapStart>
    -- setting.
    snapStart :: Prelude.Maybe SnapStart,
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
-- 'deadLetterConfig', 'updateFunctionConfiguration_deadLetterConfig' - A dead-letter queue configuration that specifies the queue or topic
-- where Lambda sends asynchronous events when they fail processing. For
-- more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#invocation-dlq Dead-letter queues>.
--
-- 'description', 'updateFunctionConfiguration_description' - A description of the function.
--
-- 'environment', 'updateFunctionConfiguration_environment' - Environment variables that are accessible from function code during
-- execution.
--
-- 'ephemeralStorage', 'updateFunctionConfiguration_ephemeralStorage' - The size of the function\'s @\/tmp@ directory in MB. The default value
-- is 512, but can be any whole number between 512 and 10,240 MB.
--
-- 'fileSystemConfigs', 'updateFunctionConfiguration_fileSystemConfigs' - Connection settings for an Amazon EFS file system.
--
-- 'handler', 'updateFunctionConfiguration_handler' - The name of the method within your code that Lambda calls to run your
-- function. Handler is required if the deployment package is a .zip file
-- archive. The format includes the file name. It can also include
-- namespaces and other qualifiers, depending on the runtime. For more
-- information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-progmodel.html Lambda programming model>.
--
-- 'imageConfig', 'updateFunctionConfiguration_imageConfig' - <https://docs.aws.amazon.com/lambda/latest/dg/images-parms.html Container image configuration values>
-- that override the values in the container image Docker file.
--
-- 'kmsKeyArn', 'updateFunctionConfiguration_kmsKeyArn' - The ARN of the Key Management Service (KMS) key that\'s used to encrypt
-- your function\'s environment variables. If it\'s not provided, Lambda
-- uses a default service key.
--
-- 'layers', 'updateFunctionConfiguration_layers' - A list of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers>
-- to add to the function\'s execution environment. Specify each layer by
-- its ARN, including the version.
--
-- 'memorySize', 'updateFunctionConfiguration_memorySize' - The amount of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-function-common.html#configuration-memory-console memory available to the function>
-- at runtime. Increasing the function memory also increases its CPU
-- allocation. The default value is 128 MB. The value can be any multiple
-- of 1 MB.
--
-- 'revisionId', 'updateFunctionConfiguration_revisionId' - Update the function only if the revision ID matches the ID that\'s
-- specified. Use this option to avoid modifying a function that has
-- changed since you last read it.
--
-- 'role'', 'updateFunctionConfiguration_role' - The Amazon Resource Name (ARN) of the function\'s execution role.
--
-- 'runtime', 'updateFunctionConfiguration_runtime' - The identifier of the function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime>.
-- Runtime is required if the deployment package is a .zip file archive.
--
-- 'snapStart', 'updateFunctionConfiguration_snapStart' - The function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/snapstart.html SnapStart>
-- setting.
--
-- 'timeout', 'updateFunctionConfiguration_timeout' - The amount of time (in seconds) that Lambda allows a function to run
-- before stopping it. The default is 3 seconds. The maximum allowed value
-- is 900 seconds. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-context.html Lambda execution environment>.
--
-- 'tracingConfig', 'updateFunctionConfiguration_tracingConfig' - Set @Mode@ to @Active@ to sample and trace a subset of incoming requests
-- with
-- <https://docs.aws.amazon.com/lambda/latest/dg/services-xray.html X-Ray>.
--
-- 'vpcConfig', 'updateFunctionConfiguration_vpcConfig' - For network connectivity to Amazon Web Services resources in a VPC,
-- specify a list of security groups and subnets in the VPC. When you
-- connect a function to a VPC, it can access resources and the internet
-- only through that VPC. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html Configuring a Lambda function to access resources in a VPC>.
--
-- 'functionName', 'updateFunctionConfiguration_functionName' - The name of the Lambda function.
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
newUpdateFunctionConfiguration ::
  -- | 'functionName'
  Prelude.Text ->
  UpdateFunctionConfiguration
newUpdateFunctionConfiguration pFunctionName_ =
  UpdateFunctionConfiguration'
    { deadLetterConfig =
        Prelude.Nothing,
      description = Prelude.Nothing,
      environment = Prelude.Nothing,
      ephemeralStorage = Prelude.Nothing,
      fileSystemConfigs = Prelude.Nothing,
      handler = Prelude.Nothing,
      imageConfig = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      layers = Prelude.Nothing,
      memorySize = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      role' = Prelude.Nothing,
      runtime = Prelude.Nothing,
      snapStart = Prelude.Nothing,
      timeout = Prelude.Nothing,
      tracingConfig = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | A dead-letter queue configuration that specifies the queue or topic
-- where Lambda sends asynchronous events when they fail processing. For
-- more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#invocation-dlq Dead-letter queues>.
updateFunctionConfiguration_deadLetterConfig :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe DeadLetterConfig)
updateFunctionConfiguration_deadLetterConfig = Lens.lens (\UpdateFunctionConfiguration' {deadLetterConfig} -> deadLetterConfig) (\s@UpdateFunctionConfiguration' {} a -> s {deadLetterConfig = a} :: UpdateFunctionConfiguration)

-- | A description of the function.
updateFunctionConfiguration_description :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Prelude.Text)
updateFunctionConfiguration_description = Lens.lens (\UpdateFunctionConfiguration' {description} -> description) (\s@UpdateFunctionConfiguration' {} a -> s {description = a} :: UpdateFunctionConfiguration)

-- | Environment variables that are accessible from function code during
-- execution.
updateFunctionConfiguration_environment :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Environment)
updateFunctionConfiguration_environment = Lens.lens (\UpdateFunctionConfiguration' {environment} -> environment) (\s@UpdateFunctionConfiguration' {} a -> s {environment = a} :: UpdateFunctionConfiguration)

-- | The size of the function\'s @\/tmp@ directory in MB. The default value
-- is 512, but can be any whole number between 512 and 10,240 MB.
updateFunctionConfiguration_ephemeralStorage :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe EphemeralStorage)
updateFunctionConfiguration_ephemeralStorage = Lens.lens (\UpdateFunctionConfiguration' {ephemeralStorage} -> ephemeralStorage) (\s@UpdateFunctionConfiguration' {} a -> s {ephemeralStorage = a} :: UpdateFunctionConfiguration)

-- | Connection settings for an Amazon EFS file system.
updateFunctionConfiguration_fileSystemConfigs :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe [FileSystemConfig])
updateFunctionConfiguration_fileSystemConfigs = Lens.lens (\UpdateFunctionConfiguration' {fileSystemConfigs} -> fileSystemConfigs) (\s@UpdateFunctionConfiguration' {} a -> s {fileSystemConfigs = a} :: UpdateFunctionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the method within your code that Lambda calls to run your
-- function. Handler is required if the deployment package is a .zip file
-- archive. The format includes the file name. It can also include
-- namespaces and other qualifiers, depending on the runtime. For more
-- information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-progmodel.html Lambda programming model>.
updateFunctionConfiguration_handler :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Prelude.Text)
updateFunctionConfiguration_handler = Lens.lens (\UpdateFunctionConfiguration' {handler} -> handler) (\s@UpdateFunctionConfiguration' {} a -> s {handler = a} :: UpdateFunctionConfiguration)

-- | <https://docs.aws.amazon.com/lambda/latest/dg/images-parms.html Container image configuration values>
-- that override the values in the container image Docker file.
updateFunctionConfiguration_imageConfig :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe ImageConfig)
updateFunctionConfiguration_imageConfig = Lens.lens (\UpdateFunctionConfiguration' {imageConfig} -> imageConfig) (\s@UpdateFunctionConfiguration' {} a -> s {imageConfig = a} :: UpdateFunctionConfiguration)

-- | The ARN of the Key Management Service (KMS) key that\'s used to encrypt
-- your function\'s environment variables. If it\'s not provided, Lambda
-- uses a default service key.
updateFunctionConfiguration_kmsKeyArn :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Prelude.Text)
updateFunctionConfiguration_kmsKeyArn = Lens.lens (\UpdateFunctionConfiguration' {kmsKeyArn} -> kmsKeyArn) (\s@UpdateFunctionConfiguration' {} a -> s {kmsKeyArn = a} :: UpdateFunctionConfiguration)

-- | A list of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers>
-- to add to the function\'s execution environment. Specify each layer by
-- its ARN, including the version.
updateFunctionConfiguration_layers :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe [Prelude.Text])
updateFunctionConfiguration_layers = Lens.lens (\UpdateFunctionConfiguration' {layers} -> layers) (\s@UpdateFunctionConfiguration' {} a -> s {layers = a} :: UpdateFunctionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The amount of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-function-common.html#configuration-memory-console memory available to the function>
-- at runtime. Increasing the function memory also increases its CPU
-- allocation. The default value is 128 MB. The value can be any multiple
-- of 1 MB.
updateFunctionConfiguration_memorySize :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Prelude.Natural)
updateFunctionConfiguration_memorySize = Lens.lens (\UpdateFunctionConfiguration' {memorySize} -> memorySize) (\s@UpdateFunctionConfiguration' {} a -> s {memorySize = a} :: UpdateFunctionConfiguration)

-- | Update the function only if the revision ID matches the ID that\'s
-- specified. Use this option to avoid modifying a function that has
-- changed since you last read it.
updateFunctionConfiguration_revisionId :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Prelude.Text)
updateFunctionConfiguration_revisionId = Lens.lens (\UpdateFunctionConfiguration' {revisionId} -> revisionId) (\s@UpdateFunctionConfiguration' {} a -> s {revisionId = a} :: UpdateFunctionConfiguration)

-- | The Amazon Resource Name (ARN) of the function\'s execution role.
updateFunctionConfiguration_role :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Prelude.Text)
updateFunctionConfiguration_role = Lens.lens (\UpdateFunctionConfiguration' {role'} -> role') (\s@UpdateFunctionConfiguration' {} a -> s {role' = a} :: UpdateFunctionConfiguration)

-- | The identifier of the function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime>.
-- Runtime is required if the deployment package is a .zip file archive.
updateFunctionConfiguration_runtime :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Runtime)
updateFunctionConfiguration_runtime = Lens.lens (\UpdateFunctionConfiguration' {runtime} -> runtime) (\s@UpdateFunctionConfiguration' {} a -> s {runtime = a} :: UpdateFunctionConfiguration)

-- | The function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/snapstart.html SnapStart>
-- setting.
updateFunctionConfiguration_snapStart :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe SnapStart)
updateFunctionConfiguration_snapStart = Lens.lens (\UpdateFunctionConfiguration' {snapStart} -> snapStart) (\s@UpdateFunctionConfiguration' {} a -> s {snapStart = a} :: UpdateFunctionConfiguration)

-- | The amount of time (in seconds) that Lambda allows a function to run
-- before stopping it. The default is 3 seconds. The maximum allowed value
-- is 900 seconds. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-context.html Lambda execution environment>.
updateFunctionConfiguration_timeout :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe Prelude.Natural)
updateFunctionConfiguration_timeout = Lens.lens (\UpdateFunctionConfiguration' {timeout} -> timeout) (\s@UpdateFunctionConfiguration' {} a -> s {timeout = a} :: UpdateFunctionConfiguration)

-- | Set @Mode@ to @Active@ to sample and trace a subset of incoming requests
-- with
-- <https://docs.aws.amazon.com/lambda/latest/dg/services-xray.html X-Ray>.
updateFunctionConfiguration_tracingConfig :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe TracingConfig)
updateFunctionConfiguration_tracingConfig = Lens.lens (\UpdateFunctionConfiguration' {tracingConfig} -> tracingConfig) (\s@UpdateFunctionConfiguration' {} a -> s {tracingConfig = a} :: UpdateFunctionConfiguration)

-- | For network connectivity to Amazon Web Services resources in a VPC,
-- specify a list of security groups and subnets in the VPC. When you
-- connect a function to a VPC, it can access resources and the internet
-- only through that VPC. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html Configuring a Lambda function to access resources in a VPC>.
updateFunctionConfiguration_vpcConfig :: Lens.Lens' UpdateFunctionConfiguration (Prelude.Maybe VpcConfig)
updateFunctionConfiguration_vpcConfig = Lens.lens (\UpdateFunctionConfiguration' {vpcConfig} -> vpcConfig) (\s@UpdateFunctionConfiguration' {} a -> s {vpcConfig = a} :: UpdateFunctionConfiguration)

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
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateFunctionConfiguration where
  hashWithSalt _salt UpdateFunctionConfiguration' {..} =
    _salt `Prelude.hashWithSalt` deadLetterConfig
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` ephemeralStorage
      `Prelude.hashWithSalt` fileSystemConfigs
      `Prelude.hashWithSalt` handler
      `Prelude.hashWithSalt` imageConfig
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` layers
      `Prelude.hashWithSalt` memorySize
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` runtime
      `Prelude.hashWithSalt` snapStart
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` tracingConfig
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` functionName

instance Prelude.NFData UpdateFunctionConfiguration where
  rnf UpdateFunctionConfiguration' {..} =
    Prelude.rnf deadLetterConfig
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf ephemeralStorage
      `Prelude.seq` Prelude.rnf fileSystemConfigs
      `Prelude.seq` Prelude.rnf handler
      `Prelude.seq` Prelude.rnf imageConfig
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf layers
      `Prelude.seq` Prelude.rnf memorySize
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf runtime
      `Prelude.seq` Prelude.rnf snapStart
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf tracingConfig
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf functionName

instance Data.ToHeaders UpdateFunctionConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateFunctionConfiguration where
  toJSON UpdateFunctionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeadLetterConfig" Data..=)
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
            ("RevisionId" Data..=) Prelude.<$> revisionId,
            ("Role" Data..=) Prelude.<$> role',
            ("Runtime" Data..=) Prelude.<$> runtime,
            ("SnapStart" Data..=) Prelude.<$> snapStart,
            ("Timeout" Data..=) Prelude.<$> timeout,
            ("TracingConfig" Data..=) Prelude.<$> tracingConfig,
            ("VpcConfig" Data..=) Prelude.<$> vpcConfig
          ]
      )

instance Data.ToPath UpdateFunctionConfiguration where
  toPath UpdateFunctionConfiguration' {..} =
    Prelude.mconcat
      [ "/2015-03-31/functions/",
        Data.toBS functionName,
        "/configuration"
      ]

instance Data.ToQuery UpdateFunctionConfiguration where
  toQuery = Prelude.const Prelude.mempty
