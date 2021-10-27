{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecurityHub.Types.AwsLambdaFunctionDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsLambdaFunctionDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.AwsLambdaFunctionCode
import Network.AWS.SecurityHub.Types.AwsLambdaFunctionDeadLetterConfig
import Network.AWS.SecurityHub.Types.AwsLambdaFunctionEnvironment
import Network.AWS.SecurityHub.Types.AwsLambdaFunctionLayer
import Network.AWS.SecurityHub.Types.AwsLambdaFunctionTracingConfig
import Network.AWS.SecurityHub.Types.AwsLambdaFunctionVpcConfig

-- | Details about a function\'s configuration.
--
-- /See:/ 'newAwsLambdaFunctionDetails' smart constructor.
data AwsLambdaFunctionDetails = AwsLambdaFunctionDetails'
  { -- | The memory that is allocated to the function.
    memorySize :: Prelude.Maybe Prelude.Int,
    -- | The runtime environment for the Lambda function.
    runtime :: Prelude.Maybe Prelude.Text,
    -- | The KMS key that is used to encrypt the function\'s environment
    -- variables. This key is only returned if you\'ve configured a customer
    -- managed customer managed key.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The function\'s environment variables.
    environment :: Prelude.Maybe AwsLambdaFunctionEnvironment,
    -- | The function\'s dead letter queue.
    deadLetterConfig :: Prelude.Maybe AwsLambdaFunctionDeadLetterConfig,
    -- | The function\'s execution role.
    role' :: Prelude.Maybe Prelude.Text,
    -- | The function\'s networking configuration.
    vpcConfig :: Prelude.Maybe AwsLambdaFunctionVpcConfig,
    -- | The version of the Lambda function.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the function.
    functionName :: Prelude.Maybe Prelude.Text,
    -- | An @AwsLambdaFunctionCode@ object.
    code :: Prelude.Maybe AwsLambdaFunctionCode,
    -- | The function\'s layers.
    layers :: Prelude.Maybe [AwsLambdaFunctionLayer],
    -- | The function that Lambda calls to begin executing your function.
    handler :: Prelude.Maybe Prelude.Text,
    -- | The amount of time that Lambda allows a function to run before stopping
    -- it.
    timeout :: Prelude.Maybe Prelude.Int,
    -- | Indicates when the function was last updated.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    lastModified :: Prelude.Maybe Prelude.Text,
    -- | The SHA256 hash of the function\'s deployment package.
    codeSha256 :: Prelude.Maybe Prelude.Text,
    -- | The function\'s X-Ray tracing configuration.
    tracingConfig :: Prelude.Maybe AwsLambdaFunctionTracingConfig,
    -- | The latest updated revision of the function or alias.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | For Lambda\@Edge functions, the ARN of the master function.
    masterArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsLambdaFunctionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memorySize', 'awsLambdaFunctionDetails_memorySize' - The memory that is allocated to the function.
--
-- 'runtime', 'awsLambdaFunctionDetails_runtime' - The runtime environment for the Lambda function.
--
-- 'kmsKeyArn', 'awsLambdaFunctionDetails_kmsKeyArn' - The KMS key that is used to encrypt the function\'s environment
-- variables. This key is only returned if you\'ve configured a customer
-- managed customer managed key.
--
-- 'environment', 'awsLambdaFunctionDetails_environment' - The function\'s environment variables.
--
-- 'deadLetterConfig', 'awsLambdaFunctionDetails_deadLetterConfig' - The function\'s dead letter queue.
--
-- 'role'', 'awsLambdaFunctionDetails_role' - The function\'s execution role.
--
-- 'vpcConfig', 'awsLambdaFunctionDetails_vpcConfig' - The function\'s networking configuration.
--
-- 'version', 'awsLambdaFunctionDetails_version' - The version of the Lambda function.
--
-- 'functionName', 'awsLambdaFunctionDetails_functionName' - The name of the function.
--
-- 'code', 'awsLambdaFunctionDetails_code' - An @AwsLambdaFunctionCode@ object.
--
-- 'layers', 'awsLambdaFunctionDetails_layers' - The function\'s layers.
--
-- 'handler', 'awsLambdaFunctionDetails_handler' - The function that Lambda calls to begin executing your function.
--
-- 'timeout', 'awsLambdaFunctionDetails_timeout' - The amount of time that Lambda allows a function to run before stopping
-- it.
--
-- 'lastModified', 'awsLambdaFunctionDetails_lastModified' - Indicates when the function was last updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'codeSha256', 'awsLambdaFunctionDetails_codeSha256' - The SHA256 hash of the function\'s deployment package.
--
-- 'tracingConfig', 'awsLambdaFunctionDetails_tracingConfig' - The function\'s X-Ray tracing configuration.
--
-- 'revisionId', 'awsLambdaFunctionDetails_revisionId' - The latest updated revision of the function or alias.
--
-- 'masterArn', 'awsLambdaFunctionDetails_masterArn' - For Lambda\@Edge functions, the ARN of the master function.
newAwsLambdaFunctionDetails ::
  AwsLambdaFunctionDetails
newAwsLambdaFunctionDetails =
  AwsLambdaFunctionDetails'
    { memorySize =
        Prelude.Nothing,
      runtime = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      environment = Prelude.Nothing,
      deadLetterConfig = Prelude.Nothing,
      role' = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      version = Prelude.Nothing,
      functionName = Prelude.Nothing,
      code = Prelude.Nothing,
      layers = Prelude.Nothing,
      handler = Prelude.Nothing,
      timeout = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      codeSha256 = Prelude.Nothing,
      tracingConfig = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      masterArn = Prelude.Nothing
    }

-- | The memory that is allocated to the function.
awsLambdaFunctionDetails_memorySize :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Int)
awsLambdaFunctionDetails_memorySize = Lens.lens (\AwsLambdaFunctionDetails' {memorySize} -> memorySize) (\s@AwsLambdaFunctionDetails' {} a -> s {memorySize = a} :: AwsLambdaFunctionDetails)

-- | The runtime environment for the Lambda function.
awsLambdaFunctionDetails_runtime :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_runtime = Lens.lens (\AwsLambdaFunctionDetails' {runtime} -> runtime) (\s@AwsLambdaFunctionDetails' {} a -> s {runtime = a} :: AwsLambdaFunctionDetails)

-- | The KMS key that is used to encrypt the function\'s environment
-- variables. This key is only returned if you\'ve configured a customer
-- managed customer managed key.
awsLambdaFunctionDetails_kmsKeyArn :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_kmsKeyArn = Lens.lens (\AwsLambdaFunctionDetails' {kmsKeyArn} -> kmsKeyArn) (\s@AwsLambdaFunctionDetails' {} a -> s {kmsKeyArn = a} :: AwsLambdaFunctionDetails)

-- | The function\'s environment variables.
awsLambdaFunctionDetails_environment :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe AwsLambdaFunctionEnvironment)
awsLambdaFunctionDetails_environment = Lens.lens (\AwsLambdaFunctionDetails' {environment} -> environment) (\s@AwsLambdaFunctionDetails' {} a -> s {environment = a} :: AwsLambdaFunctionDetails)

-- | The function\'s dead letter queue.
awsLambdaFunctionDetails_deadLetterConfig :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe AwsLambdaFunctionDeadLetterConfig)
awsLambdaFunctionDetails_deadLetterConfig = Lens.lens (\AwsLambdaFunctionDetails' {deadLetterConfig} -> deadLetterConfig) (\s@AwsLambdaFunctionDetails' {} a -> s {deadLetterConfig = a} :: AwsLambdaFunctionDetails)

-- | The function\'s execution role.
awsLambdaFunctionDetails_role :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_role = Lens.lens (\AwsLambdaFunctionDetails' {role'} -> role') (\s@AwsLambdaFunctionDetails' {} a -> s {role' = a} :: AwsLambdaFunctionDetails)

-- | The function\'s networking configuration.
awsLambdaFunctionDetails_vpcConfig :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe AwsLambdaFunctionVpcConfig)
awsLambdaFunctionDetails_vpcConfig = Lens.lens (\AwsLambdaFunctionDetails' {vpcConfig} -> vpcConfig) (\s@AwsLambdaFunctionDetails' {} a -> s {vpcConfig = a} :: AwsLambdaFunctionDetails)

-- | The version of the Lambda function.
awsLambdaFunctionDetails_version :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_version = Lens.lens (\AwsLambdaFunctionDetails' {version} -> version) (\s@AwsLambdaFunctionDetails' {} a -> s {version = a} :: AwsLambdaFunctionDetails)

-- | The name of the function.
awsLambdaFunctionDetails_functionName :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_functionName = Lens.lens (\AwsLambdaFunctionDetails' {functionName} -> functionName) (\s@AwsLambdaFunctionDetails' {} a -> s {functionName = a} :: AwsLambdaFunctionDetails)

-- | An @AwsLambdaFunctionCode@ object.
awsLambdaFunctionDetails_code :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe AwsLambdaFunctionCode)
awsLambdaFunctionDetails_code = Lens.lens (\AwsLambdaFunctionDetails' {code} -> code) (\s@AwsLambdaFunctionDetails' {} a -> s {code = a} :: AwsLambdaFunctionDetails)

-- | The function\'s layers.
awsLambdaFunctionDetails_layers :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe [AwsLambdaFunctionLayer])
awsLambdaFunctionDetails_layers = Lens.lens (\AwsLambdaFunctionDetails' {layers} -> layers) (\s@AwsLambdaFunctionDetails' {} a -> s {layers = a} :: AwsLambdaFunctionDetails) Prelude.. Lens.mapping Lens.coerced

-- | The function that Lambda calls to begin executing your function.
awsLambdaFunctionDetails_handler :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_handler = Lens.lens (\AwsLambdaFunctionDetails' {handler} -> handler) (\s@AwsLambdaFunctionDetails' {} a -> s {handler = a} :: AwsLambdaFunctionDetails)

-- | The amount of time that Lambda allows a function to run before stopping
-- it.
awsLambdaFunctionDetails_timeout :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Int)
awsLambdaFunctionDetails_timeout = Lens.lens (\AwsLambdaFunctionDetails' {timeout} -> timeout) (\s@AwsLambdaFunctionDetails' {} a -> s {timeout = a} :: AwsLambdaFunctionDetails)

-- | Indicates when the function was last updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsLambdaFunctionDetails_lastModified :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_lastModified = Lens.lens (\AwsLambdaFunctionDetails' {lastModified} -> lastModified) (\s@AwsLambdaFunctionDetails' {} a -> s {lastModified = a} :: AwsLambdaFunctionDetails)

-- | The SHA256 hash of the function\'s deployment package.
awsLambdaFunctionDetails_codeSha256 :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_codeSha256 = Lens.lens (\AwsLambdaFunctionDetails' {codeSha256} -> codeSha256) (\s@AwsLambdaFunctionDetails' {} a -> s {codeSha256 = a} :: AwsLambdaFunctionDetails)

-- | The function\'s X-Ray tracing configuration.
awsLambdaFunctionDetails_tracingConfig :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe AwsLambdaFunctionTracingConfig)
awsLambdaFunctionDetails_tracingConfig = Lens.lens (\AwsLambdaFunctionDetails' {tracingConfig} -> tracingConfig) (\s@AwsLambdaFunctionDetails' {} a -> s {tracingConfig = a} :: AwsLambdaFunctionDetails)

-- | The latest updated revision of the function or alias.
awsLambdaFunctionDetails_revisionId :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_revisionId = Lens.lens (\AwsLambdaFunctionDetails' {revisionId} -> revisionId) (\s@AwsLambdaFunctionDetails' {} a -> s {revisionId = a} :: AwsLambdaFunctionDetails)

-- | For Lambda\@Edge functions, the ARN of the master function.
awsLambdaFunctionDetails_masterArn :: Lens.Lens' AwsLambdaFunctionDetails (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDetails_masterArn = Lens.lens (\AwsLambdaFunctionDetails' {masterArn} -> masterArn) (\s@AwsLambdaFunctionDetails' {} a -> s {masterArn = a} :: AwsLambdaFunctionDetails)

instance Core.FromJSON AwsLambdaFunctionDetails where
  parseJSON =
    Core.withObject
      "AwsLambdaFunctionDetails"
      ( \x ->
          AwsLambdaFunctionDetails'
            Prelude.<$> (x Core..:? "MemorySize")
            Prelude.<*> (x Core..:? "Runtime")
            Prelude.<*> (x Core..:? "KmsKeyArn")
            Prelude.<*> (x Core..:? "Environment")
            Prelude.<*> (x Core..:? "DeadLetterConfig")
            Prelude.<*> (x Core..:? "Role")
            Prelude.<*> (x Core..:? "VpcConfig")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "FunctionName")
            Prelude.<*> (x Core..:? "Code")
            Prelude.<*> (x Core..:? "Layers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Handler")
            Prelude.<*> (x Core..:? "Timeout")
            Prelude.<*> (x Core..:? "LastModified")
            Prelude.<*> (x Core..:? "CodeSha256")
            Prelude.<*> (x Core..:? "TracingConfig")
            Prelude.<*> (x Core..:? "RevisionId")
            Prelude.<*> (x Core..:? "MasterArn")
      )

instance Prelude.Hashable AwsLambdaFunctionDetails

instance Prelude.NFData AwsLambdaFunctionDetails

instance Core.ToJSON AwsLambdaFunctionDetails where
  toJSON AwsLambdaFunctionDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MemorySize" Core..=) Prelude.<$> memorySize,
            ("Runtime" Core..=) Prelude.<$> runtime,
            ("KmsKeyArn" Core..=) Prelude.<$> kmsKeyArn,
            ("Environment" Core..=) Prelude.<$> environment,
            ("DeadLetterConfig" Core..=)
              Prelude.<$> deadLetterConfig,
            ("Role" Core..=) Prelude.<$> role',
            ("VpcConfig" Core..=) Prelude.<$> vpcConfig,
            ("Version" Core..=) Prelude.<$> version,
            ("FunctionName" Core..=) Prelude.<$> functionName,
            ("Code" Core..=) Prelude.<$> code,
            ("Layers" Core..=) Prelude.<$> layers,
            ("Handler" Core..=) Prelude.<$> handler,
            ("Timeout" Core..=) Prelude.<$> timeout,
            ("LastModified" Core..=) Prelude.<$> lastModified,
            ("CodeSha256" Core..=) Prelude.<$> codeSha256,
            ("TracingConfig" Core..=) Prelude.<$> tracingConfig,
            ("RevisionId" Core..=) Prelude.<$> revisionId,
            ("MasterArn" Core..=) Prelude.<$> masterArn
          ]
      )
