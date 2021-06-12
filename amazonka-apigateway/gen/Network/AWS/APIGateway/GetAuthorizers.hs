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
-- Module      : Network.AWS.APIGateway.GetAuthorizers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an existing Authorizers resource.
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-authorizers.html AWS CLI>
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetAuthorizers
  ( -- * Creating a Request
    GetAuthorizers (..),
    newGetAuthorizers,

    -- * Request Lenses
    getAuthorizers_position,
    getAuthorizers_limit,
    getAuthorizers_restApiId,

    -- * Destructuring the Response
    GetAuthorizersResponse (..),
    newGetAuthorizersResponse,

    -- * Response Lenses
    getAuthorizersResponse_items,
    getAuthorizersResponse_position,
    getAuthorizersResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to describe an existing Authorizers resource.
--
-- /See:/ 'newGetAuthorizers' smart constructor.
data GetAuthorizers = GetAuthorizers'
  { -- | The current pagination position in the paged result set.
    position :: Core.Maybe Core.Text,
    -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Core.Maybe Core.Int,
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAuthorizers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'position', 'getAuthorizers_position' - The current pagination position in the paged result set.
--
-- 'limit', 'getAuthorizers_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'restApiId', 'getAuthorizers_restApiId' - [Required] The string identifier of the associated RestApi.
newGetAuthorizers ::
  -- | 'restApiId'
  Core.Text ->
  GetAuthorizers
newGetAuthorizers pRestApiId_ =
  GetAuthorizers'
    { position = Core.Nothing,
      limit = Core.Nothing,
      restApiId = pRestApiId_
    }

-- | The current pagination position in the paged result set.
getAuthorizers_position :: Lens.Lens' GetAuthorizers (Core.Maybe Core.Text)
getAuthorizers_position = Lens.lens (\GetAuthorizers' {position} -> position) (\s@GetAuthorizers' {} a -> s {position = a} :: GetAuthorizers)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getAuthorizers_limit :: Lens.Lens' GetAuthorizers (Core.Maybe Core.Int)
getAuthorizers_limit = Lens.lens (\GetAuthorizers' {limit} -> limit) (\s@GetAuthorizers' {} a -> s {limit = a} :: GetAuthorizers)

-- | [Required] The string identifier of the associated RestApi.
getAuthorizers_restApiId :: Lens.Lens' GetAuthorizers Core.Text
getAuthorizers_restApiId = Lens.lens (\GetAuthorizers' {restApiId} -> restApiId) (\s@GetAuthorizers' {} a -> s {restApiId = a} :: GetAuthorizers)

instance Core.AWSPager GetAuthorizers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getAuthorizersResponse_position Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getAuthorizersResponse_items Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getAuthorizers_position
          Lens..~ rs
          Lens.^? getAuthorizersResponse_position Core.. Lens._Just

instance Core.AWSRequest GetAuthorizers where
  type
    AWSResponse GetAuthorizers =
      GetAuthorizersResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAuthorizersResponse'
            Core.<$> (x Core..?> "item" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "position")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetAuthorizers

instance Core.NFData GetAuthorizers

instance Core.ToHeaders GetAuthorizers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetAuthorizers where
  toPath GetAuthorizers' {..} =
    Core.mconcat
      ["/restapis/", Core.toBS restApiId, "/authorizers"]

instance Core.ToQuery GetAuthorizers where
  toQuery GetAuthorizers' {..} =
    Core.mconcat
      ["position" Core.=: position, "limit" Core.=: limit]

-- | Represents a collection of Authorizer resources.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-use-lambda-authorizer.html Use Lambda Function as Authorizer>
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-integrate-with-cognito.html Use Cognito User Pool as Authorizer>
--
-- /See:/ 'newGetAuthorizersResponse' smart constructor.
data GetAuthorizersResponse = GetAuthorizersResponse'
  { -- | The current page of elements from this collection.
    items :: Core.Maybe [Authorizer],
    position :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAuthorizersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getAuthorizersResponse_items' - The current page of elements from this collection.
--
-- 'position', 'getAuthorizersResponse_position' - Undocumented member.
--
-- 'httpStatus', 'getAuthorizersResponse_httpStatus' - The response's http status code.
newGetAuthorizersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAuthorizersResponse
newGetAuthorizersResponse pHttpStatus_ =
  GetAuthorizersResponse'
    { items = Core.Nothing,
      position = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getAuthorizersResponse_items :: Lens.Lens' GetAuthorizersResponse (Core.Maybe [Authorizer])
getAuthorizersResponse_items = Lens.lens (\GetAuthorizersResponse' {items} -> items) (\s@GetAuthorizersResponse' {} a -> s {items = a} :: GetAuthorizersResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
getAuthorizersResponse_position :: Lens.Lens' GetAuthorizersResponse (Core.Maybe Core.Text)
getAuthorizersResponse_position = Lens.lens (\GetAuthorizersResponse' {position} -> position) (\s@GetAuthorizersResponse' {} a -> s {position = a} :: GetAuthorizersResponse)

-- | The response's http status code.
getAuthorizersResponse_httpStatus :: Lens.Lens' GetAuthorizersResponse Core.Int
getAuthorizersResponse_httpStatus = Lens.lens (\GetAuthorizersResponse' {httpStatus} -> httpStatus) (\s@GetAuthorizersResponse' {} a -> s {httpStatus = a} :: GetAuthorizersResponse)

instance Core.NFData GetAuthorizersResponse
