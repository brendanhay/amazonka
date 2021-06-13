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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to get information about a Deployments collection.
--
-- /See:/ 'newGetDeployments' smart constructor.
data GetDeployments = GetDeployments'
  { -- | The current pagination position in the paged result set.
    position :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Prelude.Maybe Prelude.Int,
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetDeployments
newGetDeployments pRestApiId_ =
  GetDeployments'
    { position = Prelude.Nothing,
      limit = Prelude.Nothing,
      restApiId = pRestApiId_
    }

-- | The current pagination position in the paged result set.
getDeployments_position :: Lens.Lens' GetDeployments (Prelude.Maybe Prelude.Text)
getDeployments_position = Lens.lens (\GetDeployments' {position} -> position) (\s@GetDeployments' {} a -> s {position = a} :: GetDeployments)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getDeployments_limit :: Lens.Lens' GetDeployments (Prelude.Maybe Prelude.Int)
getDeployments_limit = Lens.lens (\GetDeployments' {limit} -> limit) (\s@GetDeployments' {} a -> s {limit = a} :: GetDeployments)

-- | [Required] The string identifier of the associated RestApi.
getDeployments_restApiId :: Lens.Lens' GetDeployments Prelude.Text
getDeployments_restApiId = Lens.lens (\GetDeployments' {restApiId} -> restApiId) (\s@GetDeployments' {} a -> s {restApiId = a} :: GetDeployments)

instance Core.AWSPager GetDeployments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDeploymentsResponse_position Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getDeploymentsResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getDeployments_position
          Lens..~ rs
          Lens.^? getDeploymentsResponse_position Prelude.. Lens._Just

instance Core.AWSRequest GetDeployments where
  type
    AWSResponse GetDeployments =
      GetDeploymentsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeploymentsResponse'
            Prelude.<$> (x Core..?> "item" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDeployments

instance Prelude.NFData GetDeployments

instance Core.ToHeaders GetDeployments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetDeployments where
  toPath GetDeployments' {..} =
    Prelude.mconcat
      ["/restapis/", Core.toBS restApiId, "/deployments"]

instance Core.ToQuery GetDeployments where
  toQuery GetDeployments' {..} =
    Prelude.mconcat
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
    items :: Prelude.Maybe [Deployment],
    position :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetDeploymentsResponse
newGetDeploymentsResponse pHttpStatus_ =
  GetDeploymentsResponse'
    { items = Prelude.Nothing,
      position = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getDeploymentsResponse_items :: Lens.Lens' GetDeploymentsResponse (Prelude.Maybe [Deployment])
getDeploymentsResponse_items = Lens.lens (\GetDeploymentsResponse' {items} -> items) (\s@GetDeploymentsResponse' {} a -> s {items = a} :: GetDeploymentsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Undocumented member.
getDeploymentsResponse_position :: Lens.Lens' GetDeploymentsResponse (Prelude.Maybe Prelude.Text)
getDeploymentsResponse_position = Lens.lens (\GetDeploymentsResponse' {position} -> position) (\s@GetDeploymentsResponse' {} a -> s {position = a} :: GetDeploymentsResponse)

-- | The response's http status code.
getDeploymentsResponse_httpStatus :: Lens.Lens' GetDeploymentsResponse Prelude.Int
getDeploymentsResponse_httpStatus = Lens.lens (\GetDeploymentsResponse' {httpStatus} -> httpStatus) (\s@GetDeploymentsResponse' {} a -> s {httpStatus = a} :: GetDeploymentsResponse)

instance Prelude.NFData GetDeploymentsResponse
