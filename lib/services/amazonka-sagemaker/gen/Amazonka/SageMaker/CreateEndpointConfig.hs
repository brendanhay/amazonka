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
-- Module      : Amazonka.SageMaker.CreateEndpointConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint configuration that SageMaker hosting services uses
-- to deploy models. In the configuration, you identify one or more models,
-- created using the @CreateModel@ API, to deploy and the resources that
-- you want SageMaker to provision. Then you call the CreateEndpoint API.
--
-- Use this API if you want to use SageMaker hosting services to deploy
-- models into production.
--
-- In the request, you define a @ProductionVariant@, for each model that
-- you want to deploy. Each @ProductionVariant@ parameter also describes
-- the resources that you want SageMaker to provision. This includes the
-- number and type of ML compute instances to deploy.
--
-- If you are hosting multiple models, you also assign a @VariantWeight@ to
-- specify how much traffic you want to allocate to each model. For
-- example, suppose that you want to host two models, A and B, and you
-- assign traffic weight 2 for model A and 1 for model B. SageMaker
-- distributes two-thirds of the traffic to Model A, and one-third to model
-- B.
--
-- When you call CreateEndpoint, a load call is made to DynamoDB to verify
-- that your endpoint configuration exists. When you read data from a
-- DynamoDB table supporting
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadConsistency.html Eventually Consistent Reads>
-- , the response might not reflect the results of a recently completed
-- write operation. The response might include some stale data. If the
-- dependent entities are not yet in DynamoDB, this causes a validation
-- error. If you repeat your read request after a short time, the response
-- should return the latest data. So retry logic is recommended to handle
-- these possible issues. We also recommend that customers call
-- DescribeEndpointConfig before calling CreateEndpoint to minimize the
-- potential impact of a DynamoDB eventually consistent read.
module Amazonka.SageMaker.CreateEndpointConfig
  ( -- * Creating a Request
    CreateEndpointConfig (..),
    newCreateEndpointConfig,

    -- * Request Lenses
    createEndpointConfig_asyncInferenceConfig,
    createEndpointConfig_dataCaptureConfig,
    createEndpointConfig_explainerConfig,
    createEndpointConfig_kmsKeyId,
    createEndpointConfig_shadowProductionVariants,
    createEndpointConfig_tags,
    createEndpointConfig_endpointConfigName,
    createEndpointConfig_productionVariants,

    -- * Destructuring the Response
    CreateEndpointConfigResponse (..),
    newCreateEndpointConfigResponse,

    -- * Response Lenses
    createEndpointConfigResponse_httpStatus,
    createEndpointConfigResponse_endpointConfigArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateEndpointConfig' smart constructor.
data CreateEndpointConfig = CreateEndpointConfig'
  { -- | Specifies configuration for how an endpoint performs asynchronous
    -- inference. This is a required field in order for your Endpoint to be
    -- invoked using
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_runtime_InvokeEndpointAsync.html InvokeEndpointAsync>.
    asyncInferenceConfig :: Prelude.Maybe AsyncInferenceConfig,
    dataCaptureConfig :: Prelude.Maybe DataCaptureConfig,
    -- | A member of @CreateEndpointConfig@ that enables explainers.
    explainerConfig :: Prelude.Maybe ExplainerConfig,
    -- | The Amazon Resource Name (ARN) of a Amazon Web Services Key Management
    -- Service key that SageMaker uses to encrypt data on the storage volume
    -- attached to the ML compute instance that hosts the endpoint.
    --
    -- The KmsKeyId can be any of the following formats:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Alias name: @alias\/ExampleAlias@
    --
    -- -   Alias name ARN:
    --     @arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias@
    --
    -- The KMS key policy must grant permission to the IAM role that you
    -- specify in your @CreateEndpoint@, @UpdateEndpoint@ requests. For more
    -- information, refer to the Amazon Web Services Key Management Service
    -- section
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in Amazon Web Services KMS>
    --
    -- Certain Nitro-based instances include local storage, dependent on the
    -- instance type. Local storage volumes are encrypted using a hardware
    -- module on the instance. You can\'t request a @KmsKeyId@ when using an
    -- instance type with local storage. If any of the models that you specify
    -- in the @ProductionVariants@ parameter use nitro-based instances with
    -- local storage, do not specify a value for the @KmsKeyId@ parameter. If
    -- you specify a value for @KmsKeyId@ when using any nitro-based instances
    -- with local storage, the call to @CreateEndpointConfig@ fails.
    --
    -- For a list of instance types that support local instance storage, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html#instance-store-volumes Instance Store Volumes>.
    --
    -- For more information about local instance storage encryption, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ssd-instance-store.html SSD Instance Store Volumes>.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | An array of @ProductionVariant@ objects, one for each model that you
    -- want to host at this endpoint in shadow mode with production traffic
    -- replicated from the model specified on @ProductionVariants@. If you use
    -- this field, you can only specify one variant for @ProductionVariants@
    -- and one variant for @ShadowProductionVariants@.
    shadowProductionVariants :: Prelude.Maybe (Prelude.NonEmpty ProductionVariant),
    -- | An array of key-value pairs. You can use tags to categorize your Amazon
    -- Web Services resources in different ways, for example, by purpose,
    -- owner, or environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the endpoint configuration. You specify this name in a
    -- CreateEndpoint request.
    endpointConfigName :: Prelude.Text,
    -- | An array of @ProductionVariant@ objects, one for each model that you
    -- want to host at this endpoint.
    productionVariants :: Prelude.NonEmpty ProductionVariant
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEndpointConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'asyncInferenceConfig', 'createEndpointConfig_asyncInferenceConfig' - Specifies configuration for how an endpoint performs asynchronous
-- inference. This is a required field in order for your Endpoint to be
-- invoked using
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_runtime_InvokeEndpointAsync.html InvokeEndpointAsync>.
--
-- 'dataCaptureConfig', 'createEndpointConfig_dataCaptureConfig' - Undocumented member.
--
-- 'explainerConfig', 'createEndpointConfig_explainerConfig' - A member of @CreateEndpointConfig@ that enables explainers.
--
-- 'kmsKeyId', 'createEndpointConfig_kmsKeyId' - The Amazon Resource Name (ARN) of a Amazon Web Services Key Management
-- Service key that SageMaker uses to encrypt data on the storage volume
-- attached to the ML compute instance that hosts the endpoint.
--
-- The KmsKeyId can be any of the following formats:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias name ARN:
--     @arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias@
--
-- The KMS key policy must grant permission to the IAM role that you
-- specify in your @CreateEndpoint@, @UpdateEndpoint@ requests. For more
-- information, refer to the Amazon Web Services Key Management Service
-- section
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in Amazon Web Services KMS>
--
-- Certain Nitro-based instances include local storage, dependent on the
-- instance type. Local storage volumes are encrypted using a hardware
-- module on the instance. You can\'t request a @KmsKeyId@ when using an
-- instance type with local storage. If any of the models that you specify
-- in the @ProductionVariants@ parameter use nitro-based instances with
-- local storage, do not specify a value for the @KmsKeyId@ parameter. If
-- you specify a value for @KmsKeyId@ when using any nitro-based instances
-- with local storage, the call to @CreateEndpointConfig@ fails.
--
-- For a list of instance types that support local instance storage, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html#instance-store-volumes Instance Store Volumes>.
--
-- For more information about local instance storage encryption, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ssd-instance-store.html SSD Instance Store Volumes>.
--
-- 'shadowProductionVariants', 'createEndpointConfig_shadowProductionVariants' - An array of @ProductionVariant@ objects, one for each model that you
-- want to host at this endpoint in shadow mode with production traffic
-- replicated from the model specified on @ProductionVariants@. If you use
-- this field, you can only specify one variant for @ProductionVariants@
-- and one variant for @ShadowProductionVariants@.
--
-- 'tags', 'createEndpointConfig_tags' - An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
--
-- 'endpointConfigName', 'createEndpointConfig_endpointConfigName' - The name of the endpoint configuration. You specify this name in a
-- CreateEndpoint request.
--
-- 'productionVariants', 'createEndpointConfig_productionVariants' - An array of @ProductionVariant@ objects, one for each model that you
-- want to host at this endpoint.
newCreateEndpointConfig ::
  -- | 'endpointConfigName'
  Prelude.Text ->
  -- | 'productionVariants'
  Prelude.NonEmpty ProductionVariant ->
  CreateEndpointConfig
newCreateEndpointConfig
  pEndpointConfigName_
  pProductionVariants_ =
    CreateEndpointConfig'
      { asyncInferenceConfig =
          Prelude.Nothing,
        dataCaptureConfig = Prelude.Nothing,
        explainerConfig = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        shadowProductionVariants = Prelude.Nothing,
        tags = Prelude.Nothing,
        endpointConfigName = pEndpointConfigName_,
        productionVariants =
          Lens.coerced Lens.# pProductionVariants_
      }

-- | Specifies configuration for how an endpoint performs asynchronous
-- inference. This is a required field in order for your Endpoint to be
-- invoked using
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_runtime_InvokeEndpointAsync.html InvokeEndpointAsync>.
createEndpointConfig_asyncInferenceConfig :: Lens.Lens' CreateEndpointConfig (Prelude.Maybe AsyncInferenceConfig)
createEndpointConfig_asyncInferenceConfig = Lens.lens (\CreateEndpointConfig' {asyncInferenceConfig} -> asyncInferenceConfig) (\s@CreateEndpointConfig' {} a -> s {asyncInferenceConfig = a} :: CreateEndpointConfig)

-- | Undocumented member.
createEndpointConfig_dataCaptureConfig :: Lens.Lens' CreateEndpointConfig (Prelude.Maybe DataCaptureConfig)
createEndpointConfig_dataCaptureConfig = Lens.lens (\CreateEndpointConfig' {dataCaptureConfig} -> dataCaptureConfig) (\s@CreateEndpointConfig' {} a -> s {dataCaptureConfig = a} :: CreateEndpointConfig)

-- | A member of @CreateEndpointConfig@ that enables explainers.
createEndpointConfig_explainerConfig :: Lens.Lens' CreateEndpointConfig (Prelude.Maybe ExplainerConfig)
createEndpointConfig_explainerConfig = Lens.lens (\CreateEndpointConfig' {explainerConfig} -> explainerConfig) (\s@CreateEndpointConfig' {} a -> s {explainerConfig = a} :: CreateEndpointConfig)

-- | The Amazon Resource Name (ARN) of a Amazon Web Services Key Management
-- Service key that SageMaker uses to encrypt data on the storage volume
-- attached to the ML compute instance that hosts the endpoint.
--
-- The KmsKeyId can be any of the following formats:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias name ARN:
--     @arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias@
--
-- The KMS key policy must grant permission to the IAM role that you
-- specify in your @CreateEndpoint@, @UpdateEndpoint@ requests. For more
-- information, refer to the Amazon Web Services Key Management Service
-- section
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in Amazon Web Services KMS>
--
-- Certain Nitro-based instances include local storage, dependent on the
-- instance type. Local storage volumes are encrypted using a hardware
-- module on the instance. You can\'t request a @KmsKeyId@ when using an
-- instance type with local storage. If any of the models that you specify
-- in the @ProductionVariants@ parameter use nitro-based instances with
-- local storage, do not specify a value for the @KmsKeyId@ parameter. If
-- you specify a value for @KmsKeyId@ when using any nitro-based instances
-- with local storage, the call to @CreateEndpointConfig@ fails.
--
-- For a list of instance types that support local instance storage, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html#instance-store-volumes Instance Store Volumes>.
--
-- For more information about local instance storage encryption, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ssd-instance-store.html SSD Instance Store Volumes>.
createEndpointConfig_kmsKeyId :: Lens.Lens' CreateEndpointConfig (Prelude.Maybe Prelude.Text)
createEndpointConfig_kmsKeyId = Lens.lens (\CreateEndpointConfig' {kmsKeyId} -> kmsKeyId) (\s@CreateEndpointConfig' {} a -> s {kmsKeyId = a} :: CreateEndpointConfig)

-- | An array of @ProductionVariant@ objects, one for each model that you
-- want to host at this endpoint in shadow mode with production traffic
-- replicated from the model specified on @ProductionVariants@. If you use
-- this field, you can only specify one variant for @ProductionVariants@
-- and one variant for @ShadowProductionVariants@.
createEndpointConfig_shadowProductionVariants :: Lens.Lens' CreateEndpointConfig (Prelude.Maybe (Prelude.NonEmpty ProductionVariant))
createEndpointConfig_shadowProductionVariants = Lens.lens (\CreateEndpointConfig' {shadowProductionVariants} -> shadowProductionVariants) (\s@CreateEndpointConfig' {} a -> s {shadowProductionVariants = a} :: CreateEndpointConfig) Prelude.. Lens.mapping Lens.coerced

-- | An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
createEndpointConfig_tags :: Lens.Lens' CreateEndpointConfig (Prelude.Maybe [Tag])
createEndpointConfig_tags = Lens.lens (\CreateEndpointConfig' {tags} -> tags) (\s@CreateEndpointConfig' {} a -> s {tags = a} :: CreateEndpointConfig) Prelude.. Lens.mapping Lens.coerced

-- | The name of the endpoint configuration. You specify this name in a
-- CreateEndpoint request.
createEndpointConfig_endpointConfigName :: Lens.Lens' CreateEndpointConfig Prelude.Text
createEndpointConfig_endpointConfigName = Lens.lens (\CreateEndpointConfig' {endpointConfigName} -> endpointConfigName) (\s@CreateEndpointConfig' {} a -> s {endpointConfigName = a} :: CreateEndpointConfig)

-- | An array of @ProductionVariant@ objects, one for each model that you
-- want to host at this endpoint.
createEndpointConfig_productionVariants :: Lens.Lens' CreateEndpointConfig (Prelude.NonEmpty ProductionVariant)
createEndpointConfig_productionVariants = Lens.lens (\CreateEndpointConfig' {productionVariants} -> productionVariants) (\s@CreateEndpointConfig' {} a -> s {productionVariants = a} :: CreateEndpointConfig) Prelude.. Lens.coerced

instance Core.AWSRequest CreateEndpointConfig where
  type
    AWSResponse CreateEndpointConfig =
      CreateEndpointConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEndpointConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "EndpointConfigArn")
      )

instance Prelude.Hashable CreateEndpointConfig where
  hashWithSalt _salt CreateEndpointConfig' {..} =
    _salt
      `Prelude.hashWithSalt` asyncInferenceConfig
      `Prelude.hashWithSalt` dataCaptureConfig
      `Prelude.hashWithSalt` explainerConfig
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` shadowProductionVariants
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` endpointConfigName
      `Prelude.hashWithSalt` productionVariants

instance Prelude.NFData CreateEndpointConfig where
  rnf CreateEndpointConfig' {..} =
    Prelude.rnf asyncInferenceConfig
      `Prelude.seq` Prelude.rnf dataCaptureConfig
      `Prelude.seq` Prelude.rnf explainerConfig
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf shadowProductionVariants
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf endpointConfigName
      `Prelude.seq` Prelude.rnf productionVariants

instance Data.ToHeaders CreateEndpointConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreateEndpointConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEndpointConfig where
  toJSON CreateEndpointConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AsyncInferenceConfig" Data..=)
              Prelude.<$> asyncInferenceConfig,
            ("DataCaptureConfig" Data..=)
              Prelude.<$> dataCaptureConfig,
            ("ExplainerConfig" Data..=)
              Prelude.<$> explainerConfig,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("ShadowProductionVariants" Data..=)
              Prelude.<$> shadowProductionVariants,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("EndpointConfigName" Data..= endpointConfigName),
            Prelude.Just
              ("ProductionVariants" Data..= productionVariants)
          ]
      )

instance Data.ToPath CreateEndpointConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateEndpointConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEndpointConfigResponse' smart constructor.
data CreateEndpointConfigResponse = CreateEndpointConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the endpoint configuration.
    endpointConfigArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEndpointConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createEndpointConfigResponse_httpStatus' - The response's http status code.
--
-- 'endpointConfigArn', 'createEndpointConfigResponse_endpointConfigArn' - The Amazon Resource Name (ARN) of the endpoint configuration.
newCreateEndpointConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'endpointConfigArn'
  Prelude.Text ->
  CreateEndpointConfigResponse
newCreateEndpointConfigResponse
  pHttpStatus_
  pEndpointConfigArn_ =
    CreateEndpointConfigResponse'
      { httpStatus =
          pHttpStatus_,
        endpointConfigArn = pEndpointConfigArn_
      }

-- | The response's http status code.
createEndpointConfigResponse_httpStatus :: Lens.Lens' CreateEndpointConfigResponse Prelude.Int
createEndpointConfigResponse_httpStatus = Lens.lens (\CreateEndpointConfigResponse' {httpStatus} -> httpStatus) (\s@CreateEndpointConfigResponse' {} a -> s {httpStatus = a} :: CreateEndpointConfigResponse)

-- | The Amazon Resource Name (ARN) of the endpoint configuration.
createEndpointConfigResponse_endpointConfigArn :: Lens.Lens' CreateEndpointConfigResponse Prelude.Text
createEndpointConfigResponse_endpointConfigArn = Lens.lens (\CreateEndpointConfigResponse' {endpointConfigArn} -> endpointConfigArn) (\s@CreateEndpointConfigResponse' {} a -> s {endpointConfigArn = a} :: CreateEndpointConfigResponse)

instance Prelude.NFData CreateEndpointConfigResponse where
  rnf CreateEndpointConfigResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf endpointConfigArn
