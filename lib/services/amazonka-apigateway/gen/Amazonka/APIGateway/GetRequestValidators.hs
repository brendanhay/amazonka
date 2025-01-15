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
-- Module      : Amazonka.APIGateway.GetRequestValidators
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the RequestValidators collection of a given RestApi.
--
-- This operation returns paginated results.
module Amazonka.APIGateway.GetRequestValidators
  ( -- * Creating a Request
    GetRequestValidators (..),
    newGetRequestValidators,

    -- * Request Lenses
    getRequestValidators_limit,
    getRequestValidators_position,
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

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Gets the RequestValidators collection of a given RestApi.
--
-- /See:/ 'newGetRequestValidators' smart constructor.
data GetRequestValidators = GetRequestValidators'
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
-- Create a value of 'GetRequestValidators' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'getRequestValidators_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'position', 'getRequestValidators_position' - The current pagination position in the paged result set.
--
-- 'restApiId', 'getRequestValidators_restApiId' - The string identifier of the associated RestApi.
newGetRequestValidators ::
  -- | 'restApiId'
  Prelude.Text ->
  GetRequestValidators
newGetRequestValidators pRestApiId_ =
  GetRequestValidators'
    { limit = Prelude.Nothing,
      position = Prelude.Nothing,
      restApiId = pRestApiId_
    }

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getRequestValidators_limit :: Lens.Lens' GetRequestValidators (Prelude.Maybe Prelude.Int)
getRequestValidators_limit = Lens.lens (\GetRequestValidators' {limit} -> limit) (\s@GetRequestValidators' {} a -> s {limit = a} :: GetRequestValidators)

-- | The current pagination position in the paged result set.
getRequestValidators_position :: Lens.Lens' GetRequestValidators (Prelude.Maybe Prelude.Text)
getRequestValidators_position = Lens.lens (\GetRequestValidators' {position} -> position) (\s@GetRequestValidators' {} a -> s {position = a} :: GetRequestValidators)

-- | The string identifier of the associated RestApi.
getRequestValidators_restApiId :: Lens.Lens' GetRequestValidators Prelude.Text
getRequestValidators_restApiId = Lens.lens (\GetRequestValidators' {restApiId} -> restApiId) (\s@GetRequestValidators' {} a -> s {restApiId = a} :: GetRequestValidators)

instance Core.AWSPager GetRequestValidators where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getRequestValidatorsResponse_position
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getRequestValidatorsResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getRequestValidators_position
              Lens..~ rs
              Lens.^? getRequestValidatorsResponse_position
              Prelude.. Lens._Just

instance Core.AWSRequest GetRequestValidators where
  type
    AWSResponse GetRequestValidators =
      GetRequestValidatorsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRequestValidatorsResponse'
            Prelude.<$> (x Data..?> "item" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRequestValidators where
  hashWithSalt _salt GetRequestValidators' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` restApiId

instance Prelude.NFData GetRequestValidators where
  rnf GetRequestValidators' {..} =
    Prelude.rnf limit `Prelude.seq`
      Prelude.rnf position `Prelude.seq`
        Prelude.rnf restApiId

instance Data.ToHeaders GetRequestValidators where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetRequestValidators where
  toPath GetRequestValidators' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/requestvalidators"
      ]

instance Data.ToQuery GetRequestValidators where
  toQuery GetRequestValidators' {..} =
    Prelude.mconcat
      ["limit" Data.=: limit, "position" Data.=: position]

-- | A collection of RequestValidator resources of a given RestApi.
--
-- /See:/ 'newGetRequestValidatorsResponse' smart constructor.
data GetRequestValidatorsResponse = GetRequestValidatorsResponse'
  { -- | The current page of elements from this collection.
    items :: Prelude.Maybe [RequestValidator],
    position :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetRequestValidatorsResponse
newGetRequestValidatorsResponse pHttpStatus_ =
  GetRequestValidatorsResponse'
    { items =
        Prelude.Nothing,
      position = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getRequestValidatorsResponse_items :: Lens.Lens' GetRequestValidatorsResponse (Prelude.Maybe [RequestValidator])
getRequestValidatorsResponse_items = Lens.lens (\GetRequestValidatorsResponse' {items} -> items) (\s@GetRequestValidatorsResponse' {} a -> s {items = a} :: GetRequestValidatorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getRequestValidatorsResponse_position :: Lens.Lens' GetRequestValidatorsResponse (Prelude.Maybe Prelude.Text)
getRequestValidatorsResponse_position = Lens.lens (\GetRequestValidatorsResponse' {position} -> position) (\s@GetRequestValidatorsResponse' {} a -> s {position = a} :: GetRequestValidatorsResponse)

-- | The response's http status code.
getRequestValidatorsResponse_httpStatus :: Lens.Lens' GetRequestValidatorsResponse Prelude.Int
getRequestValidatorsResponse_httpStatus = Lens.lens (\GetRequestValidatorsResponse' {httpStatus} -> httpStatus) (\s@GetRequestValidatorsResponse' {} a -> s {httpStatus = a} :: GetRequestValidatorsResponse)

instance Prelude.NFData GetRequestValidatorsResponse where
  rnf GetRequestValidatorsResponse' {..} =
    Prelude.rnf items `Prelude.seq`
      Prelude.rnf position `Prelude.seq`
        Prelude.rnf httpStatus
