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
-- Module      : Network.AWS.APIGateway.GetRequestValidators
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the RequestValidators collection of a given RestApi.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetRequestValidators
  ( -- * Creating a Request
    GetRequestValidators (..),
    newGetRequestValidators,

    -- * Request Lenses
    getRequestValidators_position,
    getRequestValidators_limit,
    getRequestValidators_restApiId,

    -- * Destructuring the Response
    GetRequestValidatorsResponse (..),
    newGetRequestValidatorsResponse,

    -- * Response Lenses
    getRequestValidatorsResponse_items,
    getRequestValidatorsResponse_position,
    getRequestValidatorsResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Gets the RequestValidators collection of a given RestApi.
--
-- /See:/ 'newGetRequestValidators' smart constructor.
data GetRequestValidators = GetRequestValidators'
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
-- Create a value of 'GetRequestValidators' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'position', 'getRequestValidators_position' - The current pagination position in the paged result set.
--
-- 'limit', 'getRequestValidators_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'restApiId', 'getRequestValidators_restApiId' - [Required] The string identifier of the associated RestApi.
newGetRequestValidators ::
  -- | 'restApiId'
  Core.Text ->
  GetRequestValidators
newGetRequestValidators pRestApiId_ =
  GetRequestValidators'
    { position = Core.Nothing,
      limit = Core.Nothing,
      restApiId = pRestApiId_
    }

-- | The current pagination position in the paged result set.
getRequestValidators_position :: Lens.Lens' GetRequestValidators (Core.Maybe Core.Text)
getRequestValidators_position = Lens.lens (\GetRequestValidators' {position} -> position) (\s@GetRequestValidators' {} a -> s {position = a} :: GetRequestValidators)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getRequestValidators_limit :: Lens.Lens' GetRequestValidators (Core.Maybe Core.Int)
getRequestValidators_limit = Lens.lens (\GetRequestValidators' {limit} -> limit) (\s@GetRequestValidators' {} a -> s {limit = a} :: GetRequestValidators)

-- | [Required] The string identifier of the associated RestApi.
getRequestValidators_restApiId :: Lens.Lens' GetRequestValidators Core.Text
getRequestValidators_restApiId = Lens.lens (\GetRequestValidators' {restApiId} -> restApiId) (\s@GetRequestValidators' {} a -> s {restApiId = a} :: GetRequestValidators)

instance Core.AWSPager GetRequestValidators where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getRequestValidatorsResponse_position
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getRequestValidatorsResponse_items Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getRequestValidators_position
          Lens..~ rs
          Lens.^? getRequestValidatorsResponse_position
            Core.. Lens._Just

instance Core.AWSRequest GetRequestValidators where
  type
    AWSResponse GetRequestValidators =
      GetRequestValidatorsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRequestValidatorsResponse'
            Core.<$> (x Core..?> "item" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "position")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetRequestValidators

instance Core.NFData GetRequestValidators

instance Core.ToHeaders GetRequestValidators where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetRequestValidators where
  toPath GetRequestValidators' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/requestvalidators"
      ]

instance Core.ToQuery GetRequestValidators where
  toQuery GetRequestValidators' {..} =
    Core.mconcat
      ["position" Core.=: position, "limit" Core.=: limit]

-- | A collection of RequestValidator resources of a given RestApi.
--
-- In OpenAPI, the RequestValidators of an API is defined by the
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-swagger-extensions.html#api-gateway-swagger-extensions-request-validators.html x-amazon-apigateway-request-validators>
-- extension.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-method-request-validation.html Enable Basic Request Validation in API Gateway>
--
-- /See:/ 'newGetRequestValidatorsResponse' smart constructor.
data GetRequestValidatorsResponse = GetRequestValidatorsResponse'
  { -- | The current page of elements from this collection.
    items :: Core.Maybe [RequestValidator],
    position :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRequestValidatorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getRequestValidatorsResponse_items' - The current page of elements from this collection.
--
-- 'position', 'getRequestValidatorsResponse_position' - Undocumented member.
--
-- 'httpStatus', 'getRequestValidatorsResponse_httpStatus' - The response's http status code.
newGetRequestValidatorsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetRequestValidatorsResponse
newGetRequestValidatorsResponse pHttpStatus_ =
  GetRequestValidatorsResponse'
    { items = Core.Nothing,
      position = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getRequestValidatorsResponse_items :: Lens.Lens' GetRequestValidatorsResponse (Core.Maybe [RequestValidator])
getRequestValidatorsResponse_items = Lens.lens (\GetRequestValidatorsResponse' {items} -> items) (\s@GetRequestValidatorsResponse' {} a -> s {items = a} :: GetRequestValidatorsResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
getRequestValidatorsResponse_position :: Lens.Lens' GetRequestValidatorsResponse (Core.Maybe Core.Text)
getRequestValidatorsResponse_position = Lens.lens (\GetRequestValidatorsResponse' {position} -> position) (\s@GetRequestValidatorsResponse' {} a -> s {position = a} :: GetRequestValidatorsResponse)

-- | The response's http status code.
getRequestValidatorsResponse_httpStatus :: Lens.Lens' GetRequestValidatorsResponse Core.Int
getRequestValidatorsResponse_httpStatus = Lens.lens (\GetRequestValidatorsResponse' {httpStatus} -> httpStatus) (\s@GetRequestValidatorsResponse' {} a -> s {httpStatus = a} :: GetRequestValidatorsResponse)

instance Core.NFData GetRequestValidatorsResponse
