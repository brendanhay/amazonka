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
-- Module      : Network.AWS.APIGateway.GetDeployments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Deployments collection.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetDeployments
  ( -- * Creating a Request
    GetDeployments (..),
    newGetDeployments,

    -- * Request Lenses
    getDeployments_position,
    getDeployments_limit,
    getDeployments_restApiId,

    -- * Destructuring the Response
    GetDeploymentsResponse (..),
    newGetDeploymentsResponse,

    -- * Response Lenses
    getDeploymentsResponse_items,
    getDeploymentsResponse_position,
    getDeploymentsResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to get information about a Deployments collection.
--
-- /See:/ 'newGetDeployments' smart constructor.
data GetDeployments = GetDeployments'
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
-- Create a value of 'GetDeployments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'position', 'getDeployments_position' - The current pagination position in the paged result set.
--
-- 'limit', 'getDeployments_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'restApiId', 'getDeployments_restApiId' - [Required] The string identifier of the associated RestApi.
newGetDeployments ::
  -- | 'restApiId'
  Core.Text ->
  GetDeployments
newGetDeployments pRestApiId_ =
  GetDeployments'
    { position = Core.Nothing,
      limit = Core.Nothing,
      restApiId = pRestApiId_
    }

-- | The current pagination position in the paged result set.
getDeployments_position :: Lens.Lens' GetDeployments (Core.Maybe Core.Text)
getDeployments_position = Lens.lens (\GetDeployments' {position} -> position) (\s@GetDeployments' {} a -> s {position = a} :: GetDeployments)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getDeployments_limit :: Lens.Lens' GetDeployments (Core.Maybe Core.Int)
getDeployments_limit = Lens.lens (\GetDeployments' {limit} -> limit) (\s@GetDeployments' {} a -> s {limit = a} :: GetDeployments)

-- | [Required] The string identifier of the associated RestApi.
getDeployments_restApiId :: Lens.Lens' GetDeployments Core.Text
getDeployments_restApiId = Lens.lens (\GetDeployments' {restApiId} -> restApiId) (\s@GetDeployments' {} a -> s {restApiId = a} :: GetDeployments)

instance Core.AWSPager GetDeployments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDeploymentsResponse_position Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getDeploymentsResponse_items Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getDeployments_position
          Lens..~ rs
          Lens.^? getDeploymentsResponse_position Core.. Lens._Just

instance Core.AWSRequest GetDeployments where
  type
    AWSResponse GetDeployments =
      GetDeploymentsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeploymentsResponse'
            Core.<$> (x Core..?> "item" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "position")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDeployments

instance Core.NFData GetDeployments

instance Core.ToHeaders GetDeployments where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetDeployments where
  toPath GetDeployments' {..} =
    Core.mconcat
      ["/restapis/", Core.toBS restApiId, "/deployments"]

instance Core.ToQuery GetDeployments where
  toQuery GetDeployments' {..} =
    Core.mconcat
      ["position" Core.=: position, "limit" Core.=: limit]

-- | Represents a collection resource that contains zero or more references
-- to your existing deployments, and links that guide you on how to
-- interact with your collection. The collection offers a paginated view of
-- the contained deployments.
--
-- To create a new deployment of a RestApi, make a @POST@ request against
-- this resource. To view, update, or delete an existing deployment, make a
-- @GET@, @PATCH@, or @DELETE@ request, respectively, on a specified
-- Deployment resource.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-deploy-api.html Deploying an API>,
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-deployment.html AWS CLI>,
-- <https://aws.amazon.com/tools/ AWS SDKs>
--
-- /See:/ 'newGetDeploymentsResponse' smart constructor.
data GetDeploymentsResponse = GetDeploymentsResponse'
  { -- | The current page of elements from this collection.
    items :: Core.Maybe [Deployment],
    position :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDeploymentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getDeploymentsResponse_items' - The current page of elements from this collection.
--
-- 'position', 'getDeploymentsResponse_position' - Undocumented member.
--
-- 'httpStatus', 'getDeploymentsResponse_httpStatus' - The response's http status code.
newGetDeploymentsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDeploymentsResponse
newGetDeploymentsResponse pHttpStatus_ =
  GetDeploymentsResponse'
    { items = Core.Nothing,
      position = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getDeploymentsResponse_items :: Lens.Lens' GetDeploymentsResponse (Core.Maybe [Deployment])
getDeploymentsResponse_items = Lens.lens (\GetDeploymentsResponse' {items} -> items) (\s@GetDeploymentsResponse' {} a -> s {items = a} :: GetDeploymentsResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
getDeploymentsResponse_position :: Lens.Lens' GetDeploymentsResponse (Core.Maybe Core.Text)
getDeploymentsResponse_position = Lens.lens (\GetDeploymentsResponse' {position} -> position) (\s@GetDeploymentsResponse' {} a -> s {position = a} :: GetDeploymentsResponse)

-- | The response's http status code.
getDeploymentsResponse_httpStatus :: Lens.Lens' GetDeploymentsResponse Core.Int
getDeploymentsResponse_httpStatus = Lens.lens (\GetDeploymentsResponse' {httpStatus} -> httpStatus) (\s@GetDeploymentsResponse' {} a -> s {httpStatus = a} :: GetDeploymentsResponse)

instance Core.NFData GetDeploymentsResponse
