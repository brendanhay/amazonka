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
-- Module      : Amazonka.SageMaker.CreateEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint using the endpoint configuration specified in the
-- request. SageMaker uses the endpoint to provision resources and deploy
-- models. You create the endpoint configuration with the
-- CreateEndpointConfig API.
--
-- Use this API to deploy models using SageMaker hosting services.
--
-- For an example that calls this method when deploying a model to
-- SageMaker hosting services, see the
-- <https://github.com/aws/amazon-sagemaker-examples/blob/master/sagemaker-fundamentals/create-endpoint/create_endpoint.ipynb Create Endpoint example notebook.>
--
-- You must not delete an @EndpointConfig@ that is in use by an endpoint
-- that is live or while the @UpdateEndpoint@ or @CreateEndpoint@
-- operations are being performed on the endpoint. To update an endpoint,
-- you must create a new @EndpointConfig@.
--
-- The endpoint name must be unique within an Amazon Web Services Region in
-- your Amazon Web Services account.
--
-- When it receives the request, SageMaker creates the endpoint, launches
-- the resources (ML compute instances), and deploys the model(s) on them.
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
--
-- When SageMaker receives the request, it sets the endpoint status to
-- @Creating@. After it creates the endpoint, it sets the status to
-- @InService@. SageMaker can then process incoming requests for
-- inferences. To check the status of an endpoint, use the DescribeEndpoint
-- API.
--
-- If any of the models hosted at this endpoint get model data from an
-- Amazon S3 location, SageMaker uses Amazon Web Services Security Token
-- Service to download model artifacts from the S3 path you provided.
-- Amazon Web Services STS is activated in your IAM user account by
-- default. If you previously deactivated Amazon Web Services STS for a
-- region, you need to reactivate Amazon Web Services STS for that region.
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating Amazon Web Services STS in an Amazon Web Services Region>
-- in the /Amazon Web Services Identity and Access Management User Guide/.
--
-- To add the IAM role policies for using this API operation, go to the
-- <https://console.aws.amazon.com/iam/ IAM console>, and choose Roles in
-- the left navigation pane. Search the IAM role that you want to grant
-- access to use the CreateEndpoint and CreateEndpointConfig API
-- operations, add the following policies to the role.
--
-- -   Option 1: For a full SageMaker access, search and attach the
--     @AmazonSageMakerFullAccess@ policy.
--
-- -   Option 2: For granting a limited access to an IAM role, paste the
--     following Action elements manually into the JSON file of the IAM
--     role:
--
--     @\"Action\": [\"sagemaker:CreateEndpoint\", \"sagemaker:CreateEndpointConfig\"]@
--
--     @\"Resource\": [@
--
--     @\"arn:aws:sagemaker:region:account-id:endpoint\/endpointName\"@
--
--     @\"arn:aws:sagemaker:region:account-id:endpoint-config\/endpointConfigName\"@
--
--     @]@
--
--     For more information, see
--     <https://docs.aws.amazon.com/sagemaker/latest/dg/api-permissions-reference.html SageMaker API Permissions: Actions, Permissions, and Resources Reference>.
module Amazonka.SageMaker.CreateEndpoint
  ( -- * Creating a Request
    CreateEndpoint (..),
    newCreateEndpoint,

    -- * Request Lenses
    createEndpoint_tags,
    createEndpoint_deploymentConfig,
    createEndpoint_endpointName,
    createEndpoint_endpointConfigName,

    -- * Destructuring the Response
    CreateEndpointResponse (..),
    newCreateEndpointResponse,

    -- * Response Lenses
    createEndpointResponse_httpStatus,
    createEndpointResponse_endpointArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
  { -- | An array of key-value pairs. You can use tags to categorize your Amazon
    -- Web Services resources in different ways, for example, by purpose,
    -- owner, or environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
    tags :: Prelude.Maybe [Tag],
    deploymentConfig :: Prelude.Maybe DeploymentConfig,
    -- | The name of the endpoint.The name must be unique within an Amazon Web
    -- Services Region in your Amazon Web Services account. The name is
    -- case-insensitive in @CreateEndpoint@, but the case is preserved and must
    -- be matched in .
    endpointName :: Prelude.Text,
    -- | The name of an endpoint configuration. For more information, see
    -- CreateEndpointConfig.
    endpointConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createEndpoint_tags' - An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
--
-- 'deploymentConfig', 'createEndpoint_deploymentConfig' - Undocumented member.
--
-- 'endpointName', 'createEndpoint_endpointName' - The name of the endpoint.The name must be unique within an Amazon Web
-- Services Region in your Amazon Web Services account. The name is
-- case-insensitive in @CreateEndpoint@, but the case is preserved and must
-- be matched in .
--
-- 'endpointConfigName', 'createEndpoint_endpointConfigName' - The name of an endpoint configuration. For more information, see
-- CreateEndpointConfig.
newCreateEndpoint ::
  -- | 'endpointName'
  Prelude.Text ->
  -- | 'endpointConfigName'
  Prelude.Text ->
  CreateEndpoint
newCreateEndpoint pEndpointName_ pEndpointConfigName_ =
  CreateEndpoint'
    { tags = Prelude.Nothing,
      deploymentConfig = Prelude.Nothing,
      endpointName = pEndpointName_,
      endpointConfigName = pEndpointConfigName_
    }

-- | An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
createEndpoint_tags :: Lens.Lens' CreateEndpoint (Prelude.Maybe [Tag])
createEndpoint_tags = Lens.lens (\CreateEndpoint' {tags} -> tags) (\s@CreateEndpoint' {} a -> s {tags = a} :: CreateEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createEndpoint_deploymentConfig :: Lens.Lens' CreateEndpoint (Prelude.Maybe DeploymentConfig)
createEndpoint_deploymentConfig = Lens.lens (\CreateEndpoint' {deploymentConfig} -> deploymentConfig) (\s@CreateEndpoint' {} a -> s {deploymentConfig = a} :: CreateEndpoint)

-- | The name of the endpoint.The name must be unique within an Amazon Web
-- Services Region in your Amazon Web Services account. The name is
-- case-insensitive in @CreateEndpoint@, but the case is preserved and must
-- be matched in .
createEndpoint_endpointName :: Lens.Lens' CreateEndpoint Prelude.Text
createEndpoint_endpointName = Lens.lens (\CreateEndpoint' {endpointName} -> endpointName) (\s@CreateEndpoint' {} a -> s {endpointName = a} :: CreateEndpoint)

-- | The name of an endpoint configuration. For more information, see
-- CreateEndpointConfig.
createEndpoint_endpointConfigName :: Lens.Lens' CreateEndpoint Prelude.Text
createEndpoint_endpointConfigName = Lens.lens (\CreateEndpoint' {endpointConfigName} -> endpointConfigName) (\s@CreateEndpoint' {} a -> s {endpointConfigName = a} :: CreateEndpoint)

instance Core.AWSRequest CreateEndpoint where
  type
    AWSResponse CreateEndpoint =
      CreateEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEndpointResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "EndpointArn")
      )

instance Prelude.Hashable CreateEndpoint where
  hashWithSalt _salt CreateEndpoint' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` deploymentConfig
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` endpointConfigName

instance Prelude.NFData CreateEndpoint where
  rnf CreateEndpoint' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf deploymentConfig
      `Prelude.seq` Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf endpointConfigName

instance Data.ToHeaders CreateEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.CreateEndpoint" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEndpoint where
  toJSON CreateEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("DeploymentConfig" Data..=)
              Prelude.<$> deploymentConfig,
            Prelude.Just ("EndpointName" Data..= endpointName),
            Prelude.Just
              ("EndpointConfigName" Data..= endpointConfigName)
          ]
      )

instance Data.ToPath CreateEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEndpointResponse' smart constructor.
data CreateEndpointResponse = CreateEndpointResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the endpoint.
    endpointArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createEndpointResponse_httpStatus' - The response's http status code.
--
-- 'endpointArn', 'createEndpointResponse_endpointArn' - The Amazon Resource Name (ARN) of the endpoint.
newCreateEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'endpointArn'
  Prelude.Text ->
  CreateEndpointResponse
newCreateEndpointResponse pHttpStatus_ pEndpointArn_ =
  CreateEndpointResponse'
    { httpStatus = pHttpStatus_,
      endpointArn = pEndpointArn_
    }

-- | The response's http status code.
createEndpointResponse_httpStatus :: Lens.Lens' CreateEndpointResponse Prelude.Int
createEndpointResponse_httpStatus = Lens.lens (\CreateEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateEndpointResponse' {} a -> s {httpStatus = a} :: CreateEndpointResponse)

-- | The Amazon Resource Name (ARN) of the endpoint.
createEndpointResponse_endpointArn :: Lens.Lens' CreateEndpointResponse Prelude.Text
createEndpointResponse_endpointArn = Lens.lens (\CreateEndpointResponse' {endpointArn} -> endpointArn) (\s@CreateEndpointResponse' {} a -> s {endpointArn = a} :: CreateEndpointResponse)

instance Prelude.NFData CreateEndpointResponse where
  rnf CreateEndpointResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf endpointArn
