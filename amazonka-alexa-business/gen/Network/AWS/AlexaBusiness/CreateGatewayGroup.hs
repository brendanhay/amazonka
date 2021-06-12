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
-- Module      : Network.AWS.AlexaBusiness.CreateGatewayGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a gateway group with the specified details.
module Network.AWS.AlexaBusiness.CreateGatewayGroup
  ( -- * Creating a Request
    CreateGatewayGroup (..),
    newCreateGatewayGroup,

    -- * Request Lenses
    createGatewayGroup_tags,
    createGatewayGroup_description,
    createGatewayGroup_name,
    createGatewayGroup_clientRequestToken,

    -- * Destructuring the Response
    CreateGatewayGroupResponse (..),
    newCreateGatewayGroupResponse,

    -- * Response Lenses
    createGatewayGroupResponse_gatewayGroupArn,
    createGatewayGroupResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateGatewayGroup' smart constructor.
data CreateGatewayGroup = CreateGatewayGroup'
  { -- | The tags to be added to the specified resource. Do not provide system
    -- tags.
    tags :: Core.Maybe [Tag],
    -- | The description of the gateway group.
    description :: Core.Maybe Core.Text,
    -- | The name of the gateway group.
    name :: Core.Text,
    -- | A unique, user-specified identifier for the request that ensures
    -- idempotency.
    clientRequestToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateGatewayGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createGatewayGroup_tags' - The tags to be added to the specified resource. Do not provide system
-- tags.
--
-- 'description', 'createGatewayGroup_description' - The description of the gateway group.
--
-- 'name', 'createGatewayGroup_name' - The name of the gateway group.
--
-- 'clientRequestToken', 'createGatewayGroup_clientRequestToken' - A unique, user-specified identifier for the request that ensures
-- idempotency.
newCreateGatewayGroup ::
  -- | 'name'
  Core.Text ->
  -- | 'clientRequestToken'
  Core.Text ->
  CreateGatewayGroup
newCreateGatewayGroup pName_ pClientRequestToken_ =
  CreateGatewayGroup'
    { tags = Core.Nothing,
      description = Core.Nothing,
      name = pName_,
      clientRequestToken = pClientRequestToken_
    }

-- | The tags to be added to the specified resource. Do not provide system
-- tags.
createGatewayGroup_tags :: Lens.Lens' CreateGatewayGroup (Core.Maybe [Tag])
createGatewayGroup_tags = Lens.lens (\CreateGatewayGroup' {tags} -> tags) (\s@CreateGatewayGroup' {} a -> s {tags = a} :: CreateGatewayGroup) Core.. Lens.mapping Lens._Coerce

-- | The description of the gateway group.
createGatewayGroup_description :: Lens.Lens' CreateGatewayGroup (Core.Maybe Core.Text)
createGatewayGroup_description = Lens.lens (\CreateGatewayGroup' {description} -> description) (\s@CreateGatewayGroup' {} a -> s {description = a} :: CreateGatewayGroup)

-- | The name of the gateway group.
createGatewayGroup_name :: Lens.Lens' CreateGatewayGroup Core.Text
createGatewayGroup_name = Lens.lens (\CreateGatewayGroup' {name} -> name) (\s@CreateGatewayGroup' {} a -> s {name = a} :: CreateGatewayGroup)

-- | A unique, user-specified identifier for the request that ensures
-- idempotency.
createGatewayGroup_clientRequestToken :: Lens.Lens' CreateGatewayGroup Core.Text
createGatewayGroup_clientRequestToken = Lens.lens (\CreateGatewayGroup' {clientRequestToken} -> clientRequestToken) (\s@CreateGatewayGroup' {} a -> s {clientRequestToken = a} :: CreateGatewayGroup)

instance Core.AWSRequest CreateGatewayGroup where
  type
    AWSResponse CreateGatewayGroup =
      CreateGatewayGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGatewayGroupResponse'
            Core.<$> (x Core..?> "GatewayGroupArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateGatewayGroup

instance Core.NFData CreateGatewayGroup

instance Core.ToHeaders CreateGatewayGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.CreateGatewayGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateGatewayGroup where
  toJSON CreateGatewayGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("Name" Core..= name),
            Core.Just
              ("ClientRequestToken" Core..= clientRequestToken)
          ]
      )

instance Core.ToPath CreateGatewayGroup where
  toPath = Core.const "/"

instance Core.ToQuery CreateGatewayGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateGatewayGroupResponse' smart constructor.
data CreateGatewayGroupResponse = CreateGatewayGroupResponse'
  { -- | The ARN of the created gateway group.
    gatewayGroupArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateGatewayGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayGroupArn', 'createGatewayGroupResponse_gatewayGroupArn' - The ARN of the created gateway group.
--
-- 'httpStatus', 'createGatewayGroupResponse_httpStatus' - The response's http status code.
newCreateGatewayGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateGatewayGroupResponse
newCreateGatewayGroupResponse pHttpStatus_ =
  CreateGatewayGroupResponse'
    { gatewayGroupArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the created gateway group.
createGatewayGroupResponse_gatewayGroupArn :: Lens.Lens' CreateGatewayGroupResponse (Core.Maybe Core.Text)
createGatewayGroupResponse_gatewayGroupArn = Lens.lens (\CreateGatewayGroupResponse' {gatewayGroupArn} -> gatewayGroupArn) (\s@CreateGatewayGroupResponse' {} a -> s {gatewayGroupArn = a} :: CreateGatewayGroupResponse)

-- | The response's http status code.
createGatewayGroupResponse_httpStatus :: Lens.Lens' CreateGatewayGroupResponse Core.Int
createGatewayGroupResponse_httpStatus = Lens.lens (\CreateGatewayGroupResponse' {httpStatus} -> httpStatus) (\s@CreateGatewayGroupResponse' {} a -> s {httpStatus = a} :: CreateGatewayGroupResponse)

instance Core.NFData CreateGatewayGroupResponse
