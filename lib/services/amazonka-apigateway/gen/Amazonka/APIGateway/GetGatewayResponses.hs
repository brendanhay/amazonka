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
-- Module      : Amazonka.APIGateway.GetGatewayResponses
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the GatewayResponses collection on the given RestApi. If an API
-- developer has not added any definitions for gateway responses, the
-- result will be the API Gateway-generated default GatewayResponses
-- collection for the supported response types.
--
-- This operation returns paginated results.
module Amazonka.APIGateway.GetGatewayResponses
  ( -- * Creating a Request
    GetGatewayResponses (..),
    newGetGatewayResponses,

    -- * Request Lenses
    getGatewayResponses_limit,
    getGatewayResponses_position,
    getGatewayResponses_restApiId,

    -- * Destructuring the Response
    GetGatewayResponsesResponse (..),
    newGetGatewayResponsesResponse,

    -- * Response Lenses
    getGatewayResponsesResponse_items,
    getGatewayResponsesResponse_position,
    getGatewayResponsesResponse_httpStatus,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Gets the GatewayResponses collection on the given RestApi. If an API
-- developer has not added any definitions for gateway responses, the
-- result will be the API Gateway-generated default GatewayResponses
-- collection for the supported response types.
--
-- /See:/ 'newGetGatewayResponses' smart constructor.
data GetGatewayResponses = GetGatewayResponses'
  { -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500. The GatewayResponses collection does not
    -- support pagination and the limit does not apply here.
    limit :: Prelude.Maybe Prelude.Int,
    -- | The current pagination position in the paged result set. The
    -- GatewayResponse collection does not support pagination and the position
    -- does not apply here.
    position :: Prelude.Maybe Prelude.Text,
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGatewayResponses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'getGatewayResponses_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500. The GatewayResponses collection does not
-- support pagination and the limit does not apply here.
--
-- 'position', 'getGatewayResponses_position' - The current pagination position in the paged result set. The
-- GatewayResponse collection does not support pagination and the position
-- does not apply here.
--
-- 'restApiId', 'getGatewayResponses_restApiId' - The string identifier of the associated RestApi.
newGetGatewayResponses ::
  -- | 'restApiId'
  Prelude.Text ->
  GetGatewayResponses
newGetGatewayResponses pRestApiId_ =
  GetGatewayResponses'
    { limit = Prelude.Nothing,
      position = Prelude.Nothing,
      restApiId = pRestApiId_
    }

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500. The GatewayResponses collection does not
-- support pagination and the limit does not apply here.
getGatewayResponses_limit :: Lens.Lens' GetGatewayResponses (Prelude.Maybe Prelude.Int)
getGatewayResponses_limit = Lens.lens (\GetGatewayResponses' {limit} -> limit) (\s@GetGatewayResponses' {} a -> s {limit = a} :: GetGatewayResponses)

-- | The current pagination position in the paged result set. The
-- GatewayResponse collection does not support pagination and the position
-- does not apply here.
getGatewayResponses_position :: Lens.Lens' GetGatewayResponses (Prelude.Maybe Prelude.Text)
getGatewayResponses_position = Lens.lens (\GetGatewayResponses' {position} -> position) (\s@GetGatewayResponses' {} a -> s {position = a} :: GetGatewayResponses)

-- | The string identifier of the associated RestApi.
getGatewayResponses_restApiId :: Lens.Lens' GetGatewayResponses Prelude.Text
getGatewayResponses_restApiId = Lens.lens (\GetGatewayResponses' {restApiId} -> restApiId) (\s@GetGatewayResponses' {} a -> s {restApiId = a} :: GetGatewayResponses)

instance Core.AWSPager GetGatewayResponses where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getGatewayResponsesResponse_position
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getGatewayResponsesResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getGatewayResponses_position
          Lens..~ rs
          Lens.^? getGatewayResponsesResponse_position
          Prelude.. Lens._Just

instance Core.AWSRequest GetGatewayResponses where
  type
    AWSResponse GetGatewayResponses =
      GetGatewayResponsesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGatewayResponsesResponse'
            Prelude.<$> (x Data..?> "item" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetGatewayResponses where
  hashWithSalt _salt GetGatewayResponses' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` restApiId

instance Prelude.NFData GetGatewayResponses where
  rnf GetGatewayResponses' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf restApiId

instance Data.ToHeaders GetGatewayResponses where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetGatewayResponses where
  toPath GetGatewayResponses' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/gatewayresponses"
      ]

instance Data.ToQuery GetGatewayResponses where
  toQuery GetGatewayResponses' {..} =
    Prelude.mconcat
      ["limit" Data.=: limit, "position" Data.=: position]

-- | The collection of the GatewayResponse instances of a RestApi as a
-- @responseType@-to-GatewayResponse object map of key-value pairs. As
-- such, pagination is not supported for querying this collection.
--
-- /See:/ 'newGetGatewayResponsesResponse' smart constructor.
data GetGatewayResponsesResponse = GetGatewayResponsesResponse'
  { -- | Returns the entire collection, because of no pagination support.
    items :: Prelude.Maybe [GatewayResponse],
    position :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGatewayResponsesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getGatewayResponsesResponse_items' - Returns the entire collection, because of no pagination support.
--
-- 'position', 'getGatewayResponsesResponse_position' - Undocumented member.
--
-- 'httpStatus', 'getGatewayResponsesResponse_httpStatus' - The response's http status code.
newGetGatewayResponsesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetGatewayResponsesResponse
newGetGatewayResponsesResponse pHttpStatus_ =
  GetGatewayResponsesResponse'
    { items =
        Prelude.Nothing,
      position = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns the entire collection, because of no pagination support.
getGatewayResponsesResponse_items :: Lens.Lens' GetGatewayResponsesResponse (Prelude.Maybe [GatewayResponse])
getGatewayResponsesResponse_items = Lens.lens (\GetGatewayResponsesResponse' {items} -> items) (\s@GetGatewayResponsesResponse' {} a -> s {items = a} :: GetGatewayResponsesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getGatewayResponsesResponse_position :: Lens.Lens' GetGatewayResponsesResponse (Prelude.Maybe Prelude.Text)
getGatewayResponsesResponse_position = Lens.lens (\GetGatewayResponsesResponse' {position} -> position) (\s@GetGatewayResponsesResponse' {} a -> s {position = a} :: GetGatewayResponsesResponse)

-- | The response's http status code.
getGatewayResponsesResponse_httpStatus :: Lens.Lens' GetGatewayResponsesResponse Prelude.Int
getGatewayResponsesResponse_httpStatus = Lens.lens (\GetGatewayResponsesResponse' {httpStatus} -> httpStatus) (\s@GetGatewayResponsesResponse' {} a -> s {httpStatus = a} :: GetGatewayResponsesResponse)

instance Prelude.NFData GetGatewayResponsesResponse where
  rnf GetGatewayResponsesResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf httpStatus
