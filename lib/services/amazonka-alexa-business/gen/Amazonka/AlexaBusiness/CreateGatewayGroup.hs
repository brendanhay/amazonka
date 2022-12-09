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
-- Module      : Amazonka.AlexaBusiness.CreateGatewayGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a gateway group with the specified details.
module Amazonka.AlexaBusiness.CreateGatewayGroup
  ( -- * Creating a Request
    CreateGatewayGroup (..),
    newCreateGatewayGroup,

    -- * Request Lenses
    createGatewayGroup_description,
    createGatewayGroup_tags,
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

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateGatewayGroup' smart constructor.
data CreateGatewayGroup = CreateGatewayGroup'
  { -- | The description of the gateway group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags to be added to the specified resource. Do not provide system
    -- tags.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the gateway group.
    name :: Prelude.Text,
    -- | A unique, user-specified identifier for the request that ensures
    -- idempotency.
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGatewayGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createGatewayGroup_description' - The description of the gateway group.
--
-- 'tags', 'createGatewayGroup_tags' - The tags to be added to the specified resource. Do not provide system
-- tags.
--
-- 'name', 'createGatewayGroup_name' - The name of the gateway group.
--
-- 'clientRequestToken', 'createGatewayGroup_clientRequestToken' - A unique, user-specified identifier for the request that ensures
-- idempotency.
newCreateGatewayGroup ::
  -- | 'name'
  Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  CreateGatewayGroup
newCreateGatewayGroup pName_ pClientRequestToken_ =
  CreateGatewayGroup'
    { description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      clientRequestToken = pClientRequestToken_
    }

-- | The description of the gateway group.
createGatewayGroup_description :: Lens.Lens' CreateGatewayGroup (Prelude.Maybe Prelude.Text)
createGatewayGroup_description = Lens.lens (\CreateGatewayGroup' {description} -> description) (\s@CreateGatewayGroup' {} a -> s {description = a} :: CreateGatewayGroup)

-- | The tags to be added to the specified resource. Do not provide system
-- tags.
createGatewayGroup_tags :: Lens.Lens' CreateGatewayGroup (Prelude.Maybe [Tag])
createGatewayGroup_tags = Lens.lens (\CreateGatewayGroup' {tags} -> tags) (\s@CreateGatewayGroup' {} a -> s {tags = a} :: CreateGatewayGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the gateway group.
createGatewayGroup_name :: Lens.Lens' CreateGatewayGroup Prelude.Text
createGatewayGroup_name = Lens.lens (\CreateGatewayGroup' {name} -> name) (\s@CreateGatewayGroup' {} a -> s {name = a} :: CreateGatewayGroup)

-- | A unique, user-specified identifier for the request that ensures
-- idempotency.
createGatewayGroup_clientRequestToken :: Lens.Lens' CreateGatewayGroup Prelude.Text
createGatewayGroup_clientRequestToken = Lens.lens (\CreateGatewayGroup' {clientRequestToken} -> clientRequestToken) (\s@CreateGatewayGroup' {} a -> s {clientRequestToken = a} :: CreateGatewayGroup)

instance Core.AWSRequest CreateGatewayGroup where
  type
    AWSResponse CreateGatewayGroup =
      CreateGatewayGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGatewayGroupResponse'
            Prelude.<$> (x Data..?> "GatewayGroupArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGatewayGroup where
  hashWithSalt _salt CreateGatewayGroup' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData CreateGatewayGroup where
  rnf CreateGatewayGroup' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Data.ToHeaders CreateGatewayGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.CreateGatewayGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateGatewayGroup where
  toJSON CreateGatewayGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken)
          ]
      )

instance Data.ToPath CreateGatewayGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateGatewayGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGatewayGroupResponse' smart constructor.
data CreateGatewayGroupResponse = CreateGatewayGroupResponse'
  { -- | The ARN of the created gateway group.
    gatewayGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateGatewayGroupResponse
newCreateGatewayGroupResponse pHttpStatus_ =
  CreateGatewayGroupResponse'
    { gatewayGroupArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the created gateway group.
createGatewayGroupResponse_gatewayGroupArn :: Lens.Lens' CreateGatewayGroupResponse (Prelude.Maybe Prelude.Text)
createGatewayGroupResponse_gatewayGroupArn = Lens.lens (\CreateGatewayGroupResponse' {gatewayGroupArn} -> gatewayGroupArn) (\s@CreateGatewayGroupResponse' {} a -> s {gatewayGroupArn = a} :: CreateGatewayGroupResponse)

-- | The response's http status code.
createGatewayGroupResponse_httpStatus :: Lens.Lens' CreateGatewayGroupResponse Prelude.Int
createGatewayGroupResponse_httpStatus = Lens.lens (\CreateGatewayGroupResponse' {httpStatus} -> httpStatus) (\s@CreateGatewayGroupResponse' {} a -> s {httpStatus = a} :: CreateGatewayGroupResponse)

instance Prelude.NFData CreateGatewayGroupResponse where
  rnf CreateGatewayGroupResponse' {..} =
    Prelude.rnf gatewayGroupArn
      `Prelude.seq` Prelude.rnf httpStatus
