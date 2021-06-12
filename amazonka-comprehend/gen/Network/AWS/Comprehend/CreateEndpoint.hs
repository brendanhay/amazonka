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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
  { -- | Tags associated with the endpoint being created. A tag is a key-value
    -- pair that adds metadata to the endpoint. For example, a tag with
    -- \"Sales\" as the key might be added to an endpoint to indicate its use
    -- by the sales department.
    tags :: Core.Maybe [Tag],
    -- | An idempotency token provided by the customer. If this token matches a
    -- previous endpoint creation request, Amazon Comprehend will not return a
    -- @ResourceInUseException@.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | This is the descriptive suffix that becomes part of the @EndpointArn@
    -- used for all subsequent requests to this resource.
    endpointName :: Core.Text,
    -- | The Amazon Resource Number (ARN) of the model to which the endpoint will
    -- be attached.
    modelArn :: Core.Text,
    -- | The desired number of inference units to be used by the model using this
    -- endpoint. Each inference unit represents of a throughput of 100
    -- characters per second.
    desiredInferenceUnits :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'modelArn'
  Core.Text ->
  -- | 'desiredInferenceUnits'
  Core.Natural ->
  CreateEndpoint
newCreateEndpoint
  pEndpointName_
  pModelArn_
  pDesiredInferenceUnits_ =
    CreateEndpoint'
      { tags = Core.Nothing,
        clientRequestToken = Core.Nothing,
        endpointName = pEndpointName_,
        modelArn = pModelArn_,
        desiredInferenceUnits = pDesiredInferenceUnits_
      }

-- | Tags associated with the endpoint being created. A tag is a key-value
-- pair that adds metadata to the endpoint. For example, a tag with
-- \"Sales\" as the key might be added to an endpoint to indicate its use
-- by the sales department.
createEndpoint_tags :: Lens.Lens' CreateEndpoint (Core.Maybe [Tag])
createEndpoint_tags = Lens.lens (\CreateEndpoint' {tags} -> tags) (\s@CreateEndpoint' {} a -> s {tags = a} :: CreateEndpoint) Core.. Lens.mapping Lens._Coerce

-- | An idempotency token provided by the customer. If this token matches a
-- previous endpoint creation request, Amazon Comprehend will not return a
-- @ResourceInUseException@.
createEndpoint_clientRequestToken :: Lens.Lens' CreateEndpoint (Core.Maybe Core.Text)
createEndpoint_clientRequestToken = Lens.lens (\CreateEndpoint' {clientRequestToken} -> clientRequestToken) (\s@CreateEndpoint' {} a -> s {clientRequestToken = a} :: CreateEndpoint)

-- | This is the descriptive suffix that becomes part of the @EndpointArn@
-- used for all subsequent requests to this resource.
createEndpoint_endpointName :: Lens.Lens' CreateEndpoint Core.Text
createEndpoint_endpointName = Lens.lens (\CreateEndpoint' {endpointName} -> endpointName) (\s@CreateEndpoint' {} a -> s {endpointName = a} :: CreateEndpoint)

-- | The Amazon Resource Number (ARN) of the model to which the endpoint will
-- be attached.
createEndpoint_modelArn :: Lens.Lens' CreateEndpoint Core.Text
createEndpoint_modelArn = Lens.lens (\CreateEndpoint' {modelArn} -> modelArn) (\s@CreateEndpoint' {} a -> s {modelArn = a} :: CreateEndpoint)

-- | The desired number of inference units to be used by the model using this
-- endpoint. Each inference unit represents of a throughput of 100
-- characters per second.
createEndpoint_desiredInferenceUnits :: Lens.Lens' CreateEndpoint Core.Natural
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
            Core.<$> (x Core..?> "EndpointArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateEndpoint

instance Core.NFData CreateEndpoint

instance Core.ToHeaders CreateEndpoint where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.CreateEndpoint" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateEndpoint where
  toJSON CreateEndpoint' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            ("ClientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            Core.Just ("EndpointName" Core..= endpointName),
            Core.Just ("ModelArn" Core..= modelArn),
            Core.Just
              ( "DesiredInferenceUnits"
                  Core..= desiredInferenceUnits
              )
          ]
      )

instance Core.ToPath CreateEndpoint where
  toPath = Core.const "/"

instance Core.ToQuery CreateEndpoint where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateEndpointResponse' smart constructor.
data CreateEndpointResponse = CreateEndpointResponse'
  { -- | The Amazon Resource Number (ARN) of the endpoint being created.
    endpointArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateEndpointResponse
newCreateEndpointResponse pHttpStatus_ =
  CreateEndpointResponse'
    { endpointArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Number (ARN) of the endpoint being created.
createEndpointResponse_endpointArn :: Lens.Lens' CreateEndpointResponse (Core.Maybe Core.Text)
createEndpointResponse_endpointArn = Lens.lens (\CreateEndpointResponse' {endpointArn} -> endpointArn) (\s@CreateEndpointResponse' {} a -> s {endpointArn = a} :: CreateEndpointResponse)

-- | The response's http status code.
createEndpointResponse_httpStatus :: Lens.Lens' CreateEndpointResponse Core.Int
createEndpointResponse_httpStatus = Lens.lens (\CreateEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateEndpointResponse' {} a -> s {httpStatus = a} :: CreateEndpointResponse)

instance Core.NFData CreateEndpointResponse
