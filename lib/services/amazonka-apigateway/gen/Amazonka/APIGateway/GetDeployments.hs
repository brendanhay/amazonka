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
-- Module      : Amazonka.APIGateway.GetDeployments
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Deployments collection.
--
-- This operation returns paginated results.
module Amazonka.APIGateway.GetDeployments
  ( -- * Creating a Request
    GetDeployments (..),
    newGetDeployments,

    -- * Request Lenses
    getDeployments_limit,
    getDeployments_position,
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

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Requests API Gateway to get information about a Deployments collection.
--
-- /See:/ 'newGetDeployments' smart constructor.
data GetDeployments = GetDeployments'
  { -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Prelude.Maybe Prelude.Int,
    -- | The current pagination position in the paged result set.
    position :: Prelude.Maybe Prelude.Text,
    -- | The string identifier of the associated RestApi.
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
-- 'limit', 'getDeployments_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'position', 'getDeployments_position' - The current pagination position in the paged result set.
--
-- 'restApiId', 'getDeployments_restApiId' - The string identifier of the associated RestApi.
newGetDeployments ::
  -- | 'restApiId'
  Prelude.Text ->
  GetDeployments
newGetDeployments pRestApiId_ =
  GetDeployments'
    { limit = Prelude.Nothing,
      position = Prelude.Nothing,
      restApiId = pRestApiId_
    }

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getDeployments_limit :: Lens.Lens' GetDeployments (Prelude.Maybe Prelude.Int)
getDeployments_limit = Lens.lens (\GetDeployments' {limit} -> limit) (\s@GetDeployments' {} a -> s {limit = a} :: GetDeployments)

-- | The current pagination position in the paged result set.
getDeployments_position :: Lens.Lens' GetDeployments (Prelude.Maybe Prelude.Text)
getDeployments_position = Lens.lens (\GetDeployments' {position} -> position) (\s@GetDeployments' {} a -> s {position = a} :: GetDeployments)

-- | The string identifier of the associated RestApi.
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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeploymentsResponse'
            Prelude.<$> (x Data..?> "item" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDeployments where
  hashWithSalt _salt GetDeployments' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` restApiId

instance Prelude.NFData GetDeployments where
  rnf GetDeployments' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf restApiId

instance Data.ToHeaders GetDeployments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetDeployments where
  toPath GetDeployments' {..} =
    Prelude.mconcat
      ["/restapis/", Data.toBS restApiId, "/deployments"]

instance Data.ToQuery GetDeployments where
  toQuery GetDeployments' {..} =
    Prelude.mconcat
      ["limit" Data.=: limit, "position" Data.=: position]

-- | Represents a collection resource that contains zero or more references
-- to your existing deployments, and links that guide you on how to
-- interact with your collection. The collection offers a paginated view of
-- the contained deployments.
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
getDeploymentsResponse_items = Lens.lens (\GetDeploymentsResponse' {items} -> items) (\s@GetDeploymentsResponse' {} a -> s {items = a} :: GetDeploymentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getDeploymentsResponse_position :: Lens.Lens' GetDeploymentsResponse (Prelude.Maybe Prelude.Text)
getDeploymentsResponse_position = Lens.lens (\GetDeploymentsResponse' {position} -> position) (\s@GetDeploymentsResponse' {} a -> s {position = a} :: GetDeploymentsResponse)

-- | The response's http status code.
getDeploymentsResponse_httpStatus :: Lens.Lens' GetDeploymentsResponse Prelude.Int
getDeploymentsResponse_httpStatus = Lens.lens (\GetDeploymentsResponse' {httpStatus} -> httpStatus) (\s@GetDeploymentsResponse' {} a -> s {httpStatus = a} :: GetDeploymentsResponse)

instance Prelude.NFData GetDeploymentsResponse where
  rnf GetDeploymentsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf httpStatus
