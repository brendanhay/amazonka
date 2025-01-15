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
-- Module      : Amazonka.APIGateway.GetDomainNames
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a collection of DomainName resources.
--
-- This operation returns paginated results.
module Amazonka.APIGateway.GetDomainNames
  ( -- * Creating a Request
    GetDomainNames (..),
    newGetDomainNames,

    -- * Request Lenses
    getDomainNames_limit,
    getDomainNames_position,

    -- * Destructuring the Response
    GetDomainNamesResponse (..),
    newGetDomainNamesResponse,

    -- * Response Lenses
    getDomainNamesResponse_items,
    getDomainNamesResponse_position,
    getDomainNamesResponse_httpStatus,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to describe a collection of DomainName resources.
--
-- /See:/ 'newGetDomainNames' smart constructor.
data GetDomainNames = GetDomainNames'
  { -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Prelude.Maybe Prelude.Int,
    -- | The current pagination position in the paged result set.
    position :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainNames' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'getDomainNames_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'position', 'getDomainNames_position' - The current pagination position in the paged result set.
newGetDomainNames ::
  GetDomainNames
newGetDomainNames =
  GetDomainNames'
    { limit = Prelude.Nothing,
      position = Prelude.Nothing
    }

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getDomainNames_limit :: Lens.Lens' GetDomainNames (Prelude.Maybe Prelude.Int)
getDomainNames_limit = Lens.lens (\GetDomainNames' {limit} -> limit) (\s@GetDomainNames' {} a -> s {limit = a} :: GetDomainNames)

-- | The current pagination position in the paged result set.
getDomainNames_position :: Lens.Lens' GetDomainNames (Prelude.Maybe Prelude.Text)
getDomainNames_position = Lens.lens (\GetDomainNames' {position} -> position) (\s@GetDomainNames' {} a -> s {position = a} :: GetDomainNames)

instance Core.AWSPager GetDomainNames where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDomainNamesResponse_position
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getDomainNamesResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getDomainNames_position
              Lens..~ rs
              Lens.^? getDomainNamesResponse_position
              Prelude.. Lens._Just

instance Core.AWSRequest GetDomainNames where
  type
    AWSResponse GetDomainNames =
      GetDomainNamesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainNamesResponse'
            Prelude.<$> (x Data..?> "item" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDomainNames where
  hashWithSalt _salt GetDomainNames' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` position

instance Prelude.NFData GetDomainNames where
  rnf GetDomainNames' {..} =
    Prelude.rnf limit `Prelude.seq`
      Prelude.rnf position

instance Data.ToHeaders GetDomainNames where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetDomainNames where
  toPath = Prelude.const "/domainnames"

instance Data.ToQuery GetDomainNames where
  toQuery GetDomainNames' {..} =
    Prelude.mconcat
      ["limit" Data.=: limit, "position" Data.=: position]

-- | Represents a collection of DomainName resources.
--
-- /See:/ 'newGetDomainNamesResponse' smart constructor.
data GetDomainNamesResponse = GetDomainNamesResponse'
  { -- | The current page of elements from this collection.
    items :: Prelude.Maybe [DomainName],
    position :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainNamesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getDomainNamesResponse_items' - The current page of elements from this collection.
--
-- 'position', 'getDomainNamesResponse_position' - Undocumented member.
--
-- 'httpStatus', 'getDomainNamesResponse_httpStatus' - The response's http status code.
newGetDomainNamesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDomainNamesResponse
newGetDomainNamesResponse pHttpStatus_ =
  GetDomainNamesResponse'
    { items = Prelude.Nothing,
      position = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getDomainNamesResponse_items :: Lens.Lens' GetDomainNamesResponse (Prelude.Maybe [DomainName])
getDomainNamesResponse_items = Lens.lens (\GetDomainNamesResponse' {items} -> items) (\s@GetDomainNamesResponse' {} a -> s {items = a} :: GetDomainNamesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getDomainNamesResponse_position :: Lens.Lens' GetDomainNamesResponse (Prelude.Maybe Prelude.Text)
getDomainNamesResponse_position = Lens.lens (\GetDomainNamesResponse' {position} -> position) (\s@GetDomainNamesResponse' {} a -> s {position = a} :: GetDomainNamesResponse)

-- | The response's http status code.
getDomainNamesResponse_httpStatus :: Lens.Lens' GetDomainNamesResponse Prelude.Int
getDomainNamesResponse_httpStatus = Lens.lens (\GetDomainNamesResponse' {httpStatus} -> httpStatus) (\s@GetDomainNamesResponse' {} a -> s {httpStatus = a} :: GetDomainNamesResponse)

instance Prelude.NFData GetDomainNamesResponse where
  rnf GetDomainNamesResponse' {..} =
    Prelude.rnf items `Prelude.seq`
      Prelude.rnf position `Prelude.seq`
        Prelude.rnf httpStatus
