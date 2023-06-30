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
-- Module      : Amazonka.Comprehend.CreateEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a model-specific endpoint for synchronous inference for a
-- previously trained custom model For information about endpoints, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/manage-endpoints.html Managing endpoints>.
module Amazonka.Comprehend.CreateEndpoint
  ( -- * Creating a Request
    CreateEndpoint (..),
    newCreateEndpoint,

    -- * Request Lenses
    createEndpoint_clientRequestToken,
    createEndpoint_dataAccessRoleArn,
    createEndpoint_tags,
    createEndpoint_endpointName,
    createEndpoint_modelArn,
    createEndpoint_desiredInferenceUnits,

    -- * Destructuring the Response
    CreateEndpointResponse (..),
    newCreateEndpointResponse,

    -- * Response Lenses
    createEndpointResponse_endpointArn,
    createEndpointResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
  { -- | An idempotency token provided by the customer. If this token matches a
    -- previous endpoint creation request, Amazon Comprehend will not return a
    -- @ResourceInUseException@.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS identity and Access Management
    -- (IAM) role that grants Amazon Comprehend read access to trained custom
    -- models encrypted with a customer managed key (ModelKmsKeyId).
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Tags associated with the endpoint being created. A tag is a key-value
    -- pair that adds metadata to the endpoint. For example, a tag with
    -- \"Sales\" as the key might be added to an endpoint to indicate its use
    -- by the sales department.
    tags :: Prelude.Maybe [Tag],
    -- | This is the descriptive suffix that becomes part of the @EndpointArn@
    -- used for all subsequent requests to this resource.
    endpointName :: Prelude.Text,
    -- | The Amazon Resource Number (ARN) of the model to which the endpoint will
    -- be attached.
    modelArn :: Prelude.Text,
    -- | The desired number of inference units to be used by the model using this
    -- endpoint. Each inference unit represents of a throughput of 100
    -- characters per second.
    desiredInferenceUnits :: Prelude.Natural
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
-- 'clientRequestToken', 'createEndpoint_clientRequestToken' - An idempotency token provided by the customer. If this token matches a
-- previous endpoint creation request, Amazon Comprehend will not return a
-- @ResourceInUseException@.
--
-- 'dataAccessRoleArn', 'createEndpoint_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to trained custom
-- models encrypted with a customer managed key (ModelKmsKeyId).
--
-- 'tags', 'createEndpoint_tags' - Tags associated with the endpoint being created. A tag is a key-value
-- pair that adds metadata to the endpoint. For example, a tag with
-- \"Sales\" as the key might be added to an endpoint to indicate its use
-- by the sales department.
--
-- 'endpointName', 'createEndpoint_endpointName' - This is the descriptive suffix that becomes part of the @EndpointArn@
-- used for all subsequent requests to this resource.
--
-- 'modelArn', 'createEndpoint_modelArn' - The Amazon Resource Number (ARN) of the model to which the endpoint will
-- be attached.
--
-- 'desiredInferenceUnits', 'createEndpoint_desiredInferenceUnits' - The desired number of inference units to be used by the model using this
-- endpoint. Each inference unit represents of a throughput of 100
-- characters per second.
newCreateEndpoint ::
  -- | 'endpointName'
  Prelude.Text ->
  -- | 'modelArn'
  Prelude.Text ->
  -- | 'desiredInferenceUnits'
  Prelude.Natural ->
  CreateEndpoint
newCreateEndpoint
  pEndpointName_
  pModelArn_
  pDesiredInferenceUnits_ =
    CreateEndpoint'
      { clientRequestToken =
          Prelude.Nothing,
        dataAccessRoleArn = Prelude.Nothing,
        tags = Prelude.Nothing,
        endpointName = pEndpointName_,
        modelArn = pModelArn_,
        desiredInferenceUnits = pDesiredInferenceUnits_
      }

-- | An idempotency token provided by the customer. If this token matches a
-- previous endpoint creation request, Amazon Comprehend will not return a
-- @ResourceInUseException@.
createEndpoint_clientRequestToken :: Lens.Lens' CreateEndpoint (Prelude.Maybe Prelude.Text)
createEndpoint_clientRequestToken = Lens.lens (\CreateEndpoint' {clientRequestToken} -> clientRequestToken) (\s@CreateEndpoint' {} a -> s {clientRequestToken = a} :: CreateEndpoint)

-- | The Amazon Resource Name (ARN) of the AWS identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to trained custom
-- models encrypted with a customer managed key (ModelKmsKeyId).
createEndpoint_dataAccessRoleArn :: Lens.Lens' CreateEndpoint (Prelude.Maybe Prelude.Text)
createEndpoint_dataAccessRoleArn = Lens.lens (\CreateEndpoint' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@CreateEndpoint' {} a -> s {dataAccessRoleArn = a} :: CreateEndpoint)

-- | Tags associated with the endpoint being created. A tag is a key-value
-- pair that adds metadata to the endpoint. For example, a tag with
-- \"Sales\" as the key might be added to an endpoint to indicate its use
-- by the sales department.
createEndpoint_tags :: Lens.Lens' CreateEndpoint (Prelude.Maybe [Tag])
createEndpoint_tags = Lens.lens (\CreateEndpoint' {tags} -> tags) (\s@CreateEndpoint' {} a -> s {tags = a} :: CreateEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | This is the descriptive suffix that becomes part of the @EndpointArn@
-- used for all subsequent requests to this resource.
createEndpoint_endpointName :: Lens.Lens' CreateEndpoint Prelude.Text
createEndpoint_endpointName = Lens.lens (\CreateEndpoint' {endpointName} -> endpointName) (\s@CreateEndpoint' {} a -> s {endpointName = a} :: CreateEndpoint)

-- | The Amazon Resource Number (ARN) of the model to which the endpoint will
-- be attached.
createEndpoint_modelArn :: Lens.Lens' CreateEndpoint Prelude.Text
createEndpoint_modelArn = Lens.lens (\CreateEndpoint' {modelArn} -> modelArn) (\s@CreateEndpoint' {} a -> s {modelArn = a} :: CreateEndpoint)

-- | The desired number of inference units to be used by the model using this
-- endpoint. Each inference unit represents of a throughput of 100
-- characters per second.
createEndpoint_desiredInferenceUnits :: Lens.Lens' CreateEndpoint Prelude.Natural
createEndpoint_desiredInferenceUnits = Lens.lens (\CreateEndpoint' {desiredInferenceUnits} -> desiredInferenceUnits) (\s@CreateEndpoint' {} a -> s {desiredInferenceUnits = a} :: CreateEndpoint)

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
            Prelude.<$> (x Data..?> "EndpointArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEndpoint where
  hashWithSalt _salt CreateEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` modelArn
      `Prelude.hashWithSalt` desiredInferenceUnits

instance Prelude.NFData CreateEndpoint where
  rnf CreateEndpoint' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf modelArn
      `Prelude.seq` Prelude.rnf desiredInferenceUnits

instance Data.ToHeaders CreateEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.CreateEndpoint" ::
                          Prelude.ByteString
                      ),
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
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("DataAccessRoleArn" Data..=)
              Prelude.<$> dataAccessRoleArn,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("EndpointName" Data..= endpointName),
            Prelude.Just ("ModelArn" Data..= modelArn),
            Prelude.Just
              ( "DesiredInferenceUnits"
                  Data..= desiredInferenceUnits
              )
          ]
      )

instance Data.ToPath CreateEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEndpointResponse' smart constructor.
data CreateEndpointResponse = CreateEndpointResponse'
  { -- | The Amazon Resource Number (ARN) of the endpoint being created.
    endpointArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'endpointArn', 'createEndpointResponse_endpointArn' - The Amazon Resource Number (ARN) of the endpoint being created.
--
-- 'httpStatus', 'createEndpointResponse_httpStatus' - The response's http status code.
newCreateEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEndpointResponse
newCreateEndpointResponse pHttpStatus_ =
  CreateEndpointResponse'
    { endpointArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Number (ARN) of the endpoint being created.
createEndpointResponse_endpointArn :: Lens.Lens' CreateEndpointResponse (Prelude.Maybe Prelude.Text)
createEndpointResponse_endpointArn = Lens.lens (\CreateEndpointResponse' {endpointArn} -> endpointArn) (\s@CreateEndpointResponse' {} a -> s {endpointArn = a} :: CreateEndpointResponse)

-- | The response's http status code.
createEndpointResponse_httpStatus :: Lens.Lens' CreateEndpointResponse Prelude.Int
createEndpointResponse_httpStatus = Lens.lens (\CreateEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateEndpointResponse' {} a -> s {httpStatus = a} :: CreateEndpointResponse)

instance Prelude.NFData CreateEndpointResponse where
  rnf CreateEndpointResponse' {..} =
    Prelude.rnf endpointArn
      `Prelude.seq` Prelude.rnf httpStatus
