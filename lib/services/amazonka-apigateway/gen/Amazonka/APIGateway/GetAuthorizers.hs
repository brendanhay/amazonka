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
-- Module      : Amazonka.APIGateway.GetAuthorizers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an existing Authorizers resource.
--
-- This operation returns paginated results.
module Amazonka.APIGateway.GetAuthorizers
  ( -- * Creating a Request
    GetAuthorizers (..),
    newGetAuthorizers,

    -- * Request Lenses
    getAuthorizers_limit,
    getAuthorizers_position,
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

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to describe an existing Authorizers resource.
--
-- /See:/ 'newGetAuthorizers' smart constructor.
data GetAuthorizers = GetAuthorizers'
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
-- Create a value of 'GetAuthorizers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'getAuthorizers_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'position', 'getAuthorizers_position' - The current pagination position in the paged result set.
--
-- 'restApiId', 'getAuthorizers_restApiId' - The string identifier of the associated RestApi.
newGetAuthorizers ::
  -- | 'restApiId'
  Prelude.Text ->
  GetAuthorizers
newGetAuthorizers pRestApiId_ =
  GetAuthorizers'
    { limit = Prelude.Nothing,
      position = Prelude.Nothing,
      restApiId = pRestApiId_
    }

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getAuthorizers_limit :: Lens.Lens' GetAuthorizers (Prelude.Maybe Prelude.Int)
getAuthorizers_limit = Lens.lens (\GetAuthorizers' {limit} -> limit) (\s@GetAuthorizers' {} a -> s {limit = a} :: GetAuthorizers)

-- | The current pagination position in the paged result set.
getAuthorizers_position :: Lens.Lens' GetAuthorizers (Prelude.Maybe Prelude.Text)
getAuthorizers_position = Lens.lens (\GetAuthorizers' {position} -> position) (\s@GetAuthorizers' {} a -> s {position = a} :: GetAuthorizers)

-- | The string identifier of the associated RestApi.
getAuthorizers_restApiId :: Lens.Lens' GetAuthorizers Prelude.Text
getAuthorizers_restApiId = Lens.lens (\GetAuthorizers' {restApiId} -> restApiId) (\s@GetAuthorizers' {} a -> s {restApiId = a} :: GetAuthorizers)

instance Core.AWSPager GetAuthorizers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getAuthorizersResponse_position Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getAuthorizersResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getAuthorizers_position
          Lens..~ rs
          Lens.^? getAuthorizersResponse_position Prelude.. Lens._Just

instance Core.AWSRequest GetAuthorizers where
  type
    AWSResponse GetAuthorizers =
      GetAuthorizersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAuthorizersResponse'
            Prelude.<$> (x Core..?> "item" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAuthorizers where
  hashWithSalt _salt GetAuthorizers' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` restApiId

instance Prelude.NFData GetAuthorizers where
  rnf GetAuthorizers' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf restApiId

instance Core.ToHeaders GetAuthorizers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetAuthorizers where
  toPath GetAuthorizers' {..} =
    Prelude.mconcat
      ["/restapis/", Core.toBS restApiId, "/authorizers"]

instance Core.ToQuery GetAuthorizers where
  toQuery GetAuthorizers' {..} =
    Prelude.mconcat
      ["limit" Core.=: limit, "position" Core.=: position]

-- | Represents a collection of Authorizer resources.
--
-- /See:/ 'newGetAuthorizersResponse' smart constructor.
data GetAuthorizersResponse = GetAuthorizersResponse'
  { -- | The current page of elements from this collection.
    items :: Prelude.Maybe [Authorizer],
    position :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetAuthorizersResponse
newGetAuthorizersResponse pHttpStatus_ =
  GetAuthorizersResponse'
    { items = Prelude.Nothing,
      position = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getAuthorizersResponse_items :: Lens.Lens' GetAuthorizersResponse (Prelude.Maybe [Authorizer])
getAuthorizersResponse_items = Lens.lens (\GetAuthorizersResponse' {items} -> items) (\s@GetAuthorizersResponse' {} a -> s {items = a} :: GetAuthorizersResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getAuthorizersResponse_position :: Lens.Lens' GetAuthorizersResponse (Prelude.Maybe Prelude.Text)
getAuthorizersResponse_position = Lens.lens (\GetAuthorizersResponse' {position} -> position) (\s@GetAuthorizersResponse' {} a -> s {position = a} :: GetAuthorizersResponse)

-- | The response's http status code.
getAuthorizersResponse_httpStatus :: Lens.Lens' GetAuthorizersResponse Prelude.Int
getAuthorizersResponse_httpStatus = Lens.lens (\GetAuthorizersResponse' {httpStatus} -> httpStatus) (\s@GetAuthorizersResponse' {} a -> s {httpStatus = a} :: GetAuthorizersResponse)

instance Prelude.NFData GetAuthorizersResponse where
  rnf GetAuthorizersResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf httpStatus
