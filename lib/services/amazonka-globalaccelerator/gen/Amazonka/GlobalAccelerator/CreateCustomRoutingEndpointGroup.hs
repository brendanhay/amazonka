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
-- Module      : Amazonka.GlobalAccelerator.CreateCustomRoutingEndpointGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an endpoint group for the specified listener for a custom routing
-- accelerator. An endpoint group is a collection of endpoints in one
-- Amazon Web Services Region.
module Amazonka.GlobalAccelerator.CreateCustomRoutingEndpointGroup
  ( -- * Creating a Request
    CreateCustomRoutingEndpointGroup (..),
    newCreateCustomRoutingEndpointGroup,

    -- * Request Lenses
    createCustomRoutingEndpointGroup_listenerArn,
    createCustomRoutingEndpointGroup_endpointGroupRegion,
    createCustomRoutingEndpointGroup_destinationConfigurations,
    createCustomRoutingEndpointGroup_idempotencyToken,

    -- * Destructuring the Response
    CreateCustomRoutingEndpointGroupResponse (..),
    newCreateCustomRoutingEndpointGroupResponse,

    -- * Response Lenses
    createCustomRoutingEndpointGroupResponse_endpointGroup,
    createCustomRoutingEndpointGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCustomRoutingEndpointGroup' smart constructor.
data CreateCustomRoutingEndpointGroup = CreateCustomRoutingEndpointGroup'
  { -- | The Amazon Resource Name (ARN) of the listener for a custom routing
    -- endpoint.
    listenerArn :: Prelude.Text,
    -- | The Amazon Web Services Region where the endpoint group is located. A
    -- listener can have only one endpoint group in a specific Region.
    endpointGroupRegion :: Prelude.Text,
    -- | Sets the port range and protocol for all endpoints (virtual private
    -- cloud subnets) in a custom routing endpoint group to accept client
    -- traffic on.
    destinationConfigurations :: Prelude.NonEmpty CustomRoutingDestinationConfiguration,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency—that is, the uniqueness—of the request.
    idempotencyToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomRoutingEndpointGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listenerArn', 'createCustomRoutingEndpointGroup_listenerArn' - The Amazon Resource Name (ARN) of the listener for a custom routing
-- endpoint.
--
-- 'endpointGroupRegion', 'createCustomRoutingEndpointGroup_endpointGroupRegion' - The Amazon Web Services Region where the endpoint group is located. A
-- listener can have only one endpoint group in a specific Region.
--
-- 'destinationConfigurations', 'createCustomRoutingEndpointGroup_destinationConfigurations' - Sets the port range and protocol for all endpoints (virtual private
-- cloud subnets) in a custom routing endpoint group to accept client
-- traffic on.
--
-- 'idempotencyToken', 'createCustomRoutingEndpointGroup_idempotencyToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency—that is, the uniqueness—of the request.
newCreateCustomRoutingEndpointGroup ::
  -- | 'listenerArn'
  Prelude.Text ->
  -- | 'endpointGroupRegion'
  Prelude.Text ->
  -- | 'destinationConfigurations'
  Prelude.NonEmpty CustomRoutingDestinationConfiguration ->
  -- | 'idempotencyToken'
  Prelude.Text ->
  CreateCustomRoutingEndpointGroup
newCreateCustomRoutingEndpointGroup
  pListenerArn_
  pEndpointGroupRegion_
  pDestinationConfigurations_
  pIdempotencyToken_ =
    CreateCustomRoutingEndpointGroup'
      { listenerArn =
          pListenerArn_,
        endpointGroupRegion =
          pEndpointGroupRegion_,
        destinationConfigurations =
          Lens.coerced
            Lens.# pDestinationConfigurations_,
        idempotencyToken = pIdempotencyToken_
      }

-- | The Amazon Resource Name (ARN) of the listener for a custom routing
-- endpoint.
createCustomRoutingEndpointGroup_listenerArn :: Lens.Lens' CreateCustomRoutingEndpointGroup Prelude.Text
createCustomRoutingEndpointGroup_listenerArn = Lens.lens (\CreateCustomRoutingEndpointGroup' {listenerArn} -> listenerArn) (\s@CreateCustomRoutingEndpointGroup' {} a -> s {listenerArn = a} :: CreateCustomRoutingEndpointGroup)

-- | The Amazon Web Services Region where the endpoint group is located. A
-- listener can have only one endpoint group in a specific Region.
createCustomRoutingEndpointGroup_endpointGroupRegion :: Lens.Lens' CreateCustomRoutingEndpointGroup Prelude.Text
createCustomRoutingEndpointGroup_endpointGroupRegion = Lens.lens (\CreateCustomRoutingEndpointGroup' {endpointGroupRegion} -> endpointGroupRegion) (\s@CreateCustomRoutingEndpointGroup' {} a -> s {endpointGroupRegion = a} :: CreateCustomRoutingEndpointGroup)

-- | Sets the port range and protocol for all endpoints (virtual private
-- cloud subnets) in a custom routing endpoint group to accept client
-- traffic on.
createCustomRoutingEndpointGroup_destinationConfigurations :: Lens.Lens' CreateCustomRoutingEndpointGroup (Prelude.NonEmpty CustomRoutingDestinationConfiguration)
createCustomRoutingEndpointGroup_destinationConfigurations = Lens.lens (\CreateCustomRoutingEndpointGroup' {destinationConfigurations} -> destinationConfigurations) (\s@CreateCustomRoutingEndpointGroup' {} a -> s {destinationConfigurations = a} :: CreateCustomRoutingEndpointGroup) Prelude.. Lens.coerced

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency—that is, the uniqueness—of the request.
createCustomRoutingEndpointGroup_idempotencyToken :: Lens.Lens' CreateCustomRoutingEndpointGroup Prelude.Text
createCustomRoutingEndpointGroup_idempotencyToken = Lens.lens (\CreateCustomRoutingEndpointGroup' {idempotencyToken} -> idempotencyToken) (\s@CreateCustomRoutingEndpointGroup' {} a -> s {idempotencyToken = a} :: CreateCustomRoutingEndpointGroup)

instance
  Core.AWSRequest
    CreateCustomRoutingEndpointGroup
  where
  type
    AWSResponse CreateCustomRoutingEndpointGroup =
      CreateCustomRoutingEndpointGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCustomRoutingEndpointGroupResponse'
            Prelude.<$> (x Core..?> "EndpointGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateCustomRoutingEndpointGroup
  where
  hashWithSalt
    _salt
    CreateCustomRoutingEndpointGroup' {..} =
      _salt `Prelude.hashWithSalt` listenerArn
        `Prelude.hashWithSalt` endpointGroupRegion
        `Prelude.hashWithSalt` destinationConfigurations
        `Prelude.hashWithSalt` idempotencyToken

instance
  Prelude.NFData
    CreateCustomRoutingEndpointGroup
  where
  rnf CreateCustomRoutingEndpointGroup' {..} =
    Prelude.rnf listenerArn
      `Prelude.seq` Prelude.rnf endpointGroupRegion
      `Prelude.seq` Prelude.rnf destinationConfigurations
      `Prelude.seq` Prelude.rnf idempotencyToken

instance
  Core.ToHeaders
    CreateCustomRoutingEndpointGroup
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GlobalAccelerator_V20180706.CreateCustomRoutingEndpointGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateCustomRoutingEndpointGroup where
  toJSON CreateCustomRoutingEndpointGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ListenerArn" Core..= listenerArn),
            Prelude.Just
              ("EndpointGroupRegion" Core..= endpointGroupRegion),
            Prelude.Just
              ( "DestinationConfigurations"
                  Core..= destinationConfigurations
              ),
            Prelude.Just
              ("IdempotencyToken" Core..= idempotencyToken)
          ]
      )

instance Core.ToPath CreateCustomRoutingEndpointGroup where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    CreateCustomRoutingEndpointGroup
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCustomRoutingEndpointGroupResponse' smart constructor.
data CreateCustomRoutingEndpointGroupResponse = CreateCustomRoutingEndpointGroupResponse'
  { -- | The information about the endpoint group created for a custom routing
    -- accelerator.
    endpointGroup :: Prelude.Maybe CustomRoutingEndpointGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomRoutingEndpointGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointGroup', 'createCustomRoutingEndpointGroupResponse_endpointGroup' - The information about the endpoint group created for a custom routing
-- accelerator.
--
-- 'httpStatus', 'createCustomRoutingEndpointGroupResponse_httpStatus' - The response's http status code.
newCreateCustomRoutingEndpointGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCustomRoutingEndpointGroupResponse
newCreateCustomRoutingEndpointGroupResponse
  pHttpStatus_ =
    CreateCustomRoutingEndpointGroupResponse'
      { endpointGroup =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The information about the endpoint group created for a custom routing
-- accelerator.
createCustomRoutingEndpointGroupResponse_endpointGroup :: Lens.Lens' CreateCustomRoutingEndpointGroupResponse (Prelude.Maybe CustomRoutingEndpointGroup)
createCustomRoutingEndpointGroupResponse_endpointGroup = Lens.lens (\CreateCustomRoutingEndpointGroupResponse' {endpointGroup} -> endpointGroup) (\s@CreateCustomRoutingEndpointGroupResponse' {} a -> s {endpointGroup = a} :: CreateCustomRoutingEndpointGroupResponse)

-- | The response's http status code.
createCustomRoutingEndpointGroupResponse_httpStatus :: Lens.Lens' CreateCustomRoutingEndpointGroupResponse Prelude.Int
createCustomRoutingEndpointGroupResponse_httpStatus = Lens.lens (\CreateCustomRoutingEndpointGroupResponse' {httpStatus} -> httpStatus) (\s@CreateCustomRoutingEndpointGroupResponse' {} a -> s {httpStatus = a} :: CreateCustomRoutingEndpointGroupResponse)

instance
  Prelude.NFData
    CreateCustomRoutingEndpointGroupResponse
  where
  rnf CreateCustomRoutingEndpointGroupResponse' {..} =
    Prelude.rnf endpointGroup
      `Prelude.seq` Prelude.rnf httpStatus
