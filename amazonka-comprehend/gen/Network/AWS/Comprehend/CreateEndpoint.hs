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
-- Module      : Network.AWS.Comprehend.CreateEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a model-specific endpoint for synchronous inference for a
-- previously trained custom model
module Network.AWS.Comprehend.CreateEndpoint
  ( -- * Creating a Request
    CreateEndpoint (..),
    newCreateEndpoint,

    -- * Request Lenses
    createEndpoint_tags,
    createEndpoint_clientRequestToken,
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

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
  { -- | Tags associated with the endpoint being created. A tag is a key-value
    -- pair that adds metadata to the endpoint. For example, a tag with
    -- \"Sales\" as the key might be added to an endpoint to indicate its use
    -- by the sales department.
    tags :: Prelude.Maybe [Tag],
    -- | An idempotency token provided by the customer. If this token matches a
    -- previous endpoint creation request, Amazon Comprehend will not return a
    -- @ResourceInUseException@.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
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
-- 'tags', 'createEndpoint_tags' - Tags associated with the endpoint being created. A tag is a key-value
-- pair that adds metadata to the endpoint. For example, a tag with
-- \"Sales\" as the key might be added to an endpoint to indicate its use
-- by the sales department.
--
-- 'clientRequestToken', 'createEndpoint_clientRequestToken' - An idempotency token provided by the customer. If this token matches a
-- previous endpoint creation request, Amazon Comprehend will not return a
-- @ResourceInUseException@.
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
      { tags = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        endpointName = pEndpointName_,
        modelArn = pModelArn_,
        desiredInferenceUnits = pDesiredInferenceUnits_
      }

-- | Tags associated with the endpoint being created. A tag is a key-value
-- pair that adds metadata to the endpoint. For example, a tag with
-- \"Sales\" as the key might be added to an endpoint to indicate its use
-- by the sales department.
createEndpoint_tags :: Lens.Lens' CreateEndpoint (Prelude.Maybe [Tag])
createEndpoint_tags = Lens.lens (\CreateEndpoint' {tags} -> tags) (\s@CreateEndpoint' {} a -> s {tags = a} :: CreateEndpoint) Prelude.. Lens.mapping Lens._Coerce

-- | An idempotency token provided by the customer. If this token matches a
-- previous endpoint creation request, Amazon Comprehend will not return a
-- @ResourceInUseException@.
createEndpoint_clientRequestToken :: Lens.Lens' CreateEndpoint (Prelude.Maybe Prelude.Text)
createEndpoint_clientRequestToken = Lens.lens (\CreateEndpoint' {clientRequestToken} -> clientRequestToken) (\s@CreateEndpoint' {} a -> s {clientRequestToken = a} :: CreateEndpoint)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEndpointResponse'
            Prelude.<$> (x Core..?> "EndpointArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEndpoint

instance Prelude.NFData CreateEndpoint

instance Core.ToHeaders CreateEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.CreateEndpoint" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateEndpoint where
  toJSON CreateEndpoint' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("EndpointName" Core..= endpointName),
            Prelude.Just ("ModelArn" Core..= modelArn),
            Prelude.Just
              ( "DesiredInferenceUnits"
                  Core..= desiredInferenceUnits
              )
          ]
      )

instance Core.ToPath CreateEndpoint where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateEndpoint where
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

instance Prelude.NFData CreateEndpointResponse
