{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.CreateEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint using the endpoint configuration specified in the
-- request. Amazon SageMaker uses the endpoint to provision resources and
-- deploy models. You create the endpoint configuration with the
-- CreateEndpointConfig API.
--
-- Use this API to deploy models using Amazon SageMaker hosting services.
--
-- For an example that calls this method when deploying a model to Amazon
-- SageMaker hosting services, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/ex1-deploy-model.html#ex1-deploy-model-boto Deploy the Model to Amazon SageMaker Hosting Services (AWS SDK for Python (Boto 3)).>
--
-- You must not delete an @EndpointConfig@ that is in use by an endpoint
-- that is live or while the @UpdateEndpoint@ or @CreateEndpoint@
-- operations are being performed on the endpoint. To update an endpoint,
-- you must create a new @EndpointConfig@.
--
-- The endpoint name must be unique within an AWS Region in your AWS
-- account.
--
-- When it receives the request, Amazon SageMaker creates the endpoint,
-- launches the resources (ML compute instances), and deploys the model(s)
-- on them.
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
-- When Amazon SageMaker receives the request, it sets the endpoint status
-- to @Creating@. After it creates the endpoint, it sets the status to
-- @InService@. Amazon SageMaker can then process incoming requests for
-- inferences. To check the status of an endpoint, use the DescribeEndpoint
-- API.
--
-- If any of the models hosted at this endpoint get model data from an
-- Amazon S3 location, Amazon SageMaker uses AWS Security Token Service to
-- download model artifacts from the S3 path you provided. AWS STS is
-- activated in your IAM user account by default. If you previously
-- deactivated AWS STS for a region, you need to reactivate AWS STS for
-- that region. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating AWS STS in an AWS Region>
-- in the /AWS Identity and Access Management User Guide/.
--
-- To add the IAM role policies for using this API operation, go to the
-- <https://console.aws.amazon.com/iam/ IAM console>, and choose Roles in
-- the left navigation pane. Search the IAM role that you want to grant
-- access to use the CreateEndpoint and CreateEndpointConfig API
-- operations, add the following policies to the role.
--
-- -   Option 1: For a full Amazon SageMaker access, search and attach the
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
--     <https://docs.aws.amazon.com/sagemaker/latest/dg/api-permissions-reference.html Amazon SageMaker API Permissions: Actions, Permissions, and Resources Reference>.
module Network.AWS.SageMaker.CreateEndpoint
  ( -- * Creating a Request
    CreateEndpoint (..),
    newCreateEndpoint,

    -- * Request Lenses
    createEndpoint_tags,
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
  { -- | An array of key-value pairs. You can use tags to categorize your AWS
    -- resources in different ways, for example, by purpose, owner, or
    -- environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the endpoint.The name must be unique within an AWS Region in
    -- your AWS account. The name is case-insensitive in @CreateEndpoint@, but
    -- the case is preserved and must be matched in .
    endpointName :: Prelude.Text,
    -- | The name of an endpoint configuration. For more information, see
    -- CreateEndpointConfig.
    endpointConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createEndpoint_tags' - An array of key-value pairs. You can use tags to categorize your AWS
-- resources in different ways, for example, by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
--
-- 'endpointName', 'createEndpoint_endpointName' - The name of the endpoint.The name must be unique within an AWS Region in
-- your AWS account. The name is case-insensitive in @CreateEndpoint@, but
-- the case is preserved and must be matched in .
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
      endpointName = pEndpointName_,
      endpointConfigName = pEndpointConfigName_
    }

-- | An array of key-value pairs. You can use tags to categorize your AWS
-- resources in different ways, for example, by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
createEndpoint_tags :: Lens.Lens' CreateEndpoint (Prelude.Maybe [Tag])
createEndpoint_tags = Lens.lens (\CreateEndpoint' {tags} -> tags) (\s@CreateEndpoint' {} a -> s {tags = a} :: CreateEndpoint) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the endpoint.The name must be unique within an AWS Region in
-- your AWS account. The name is case-insensitive in @CreateEndpoint@, but
-- the case is preserved and must be matched in .
createEndpoint_endpointName :: Lens.Lens' CreateEndpoint Prelude.Text
createEndpoint_endpointName = Lens.lens (\CreateEndpoint' {endpointName} -> endpointName) (\s@CreateEndpoint' {} a -> s {endpointName = a} :: CreateEndpoint)

-- | The name of an endpoint configuration. For more information, see
-- CreateEndpointConfig.
createEndpoint_endpointConfigName :: Lens.Lens' CreateEndpoint Prelude.Text
createEndpoint_endpointConfigName = Lens.lens (\CreateEndpoint' {endpointConfigName} -> endpointConfigName) (\s@CreateEndpoint' {} a -> s {endpointConfigName = a} :: CreateEndpoint)

instance Prelude.AWSRequest CreateEndpoint where
  type Rs CreateEndpoint = CreateEndpointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEndpointResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "EndpointArn")
      )

instance Prelude.Hashable CreateEndpoint

instance Prelude.NFData CreateEndpoint

instance Prelude.ToHeaders CreateEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.CreateEndpoint" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateEndpoint where
  toJSON CreateEndpoint' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Tags" Prelude..=) Prelude.<$> tags,
            Prelude.Just
              ("EndpointName" Prelude..= endpointName),
            Prelude.Just
              ( "EndpointConfigName"
                  Prelude..= endpointConfigName
              )
          ]
      )

instance Prelude.ToPath CreateEndpoint where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEndpointResponse' smart constructor.
data CreateEndpointResponse = CreateEndpointResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the endpoint.
    endpointArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData CreateEndpointResponse
