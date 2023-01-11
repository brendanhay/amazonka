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
-- Module      : Amazonka.APIGateway.GetBasePathMappings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a collection of BasePathMapping resources.
--
-- This operation returns paginated results.
module Amazonka.APIGateway.GetBasePathMappings
  ( -- * Creating a Request
    GetBasePathMappings (..),
    newGetBasePathMappings,

    -- * Request Lenses
    getBasePathMappings_limit,
    getBasePathMappings_position,
    getBasePathMappings_domainName,

    -- * Destructuring the Response
    GetBasePathMappingsResponse (..),
    newGetBasePathMappingsResponse,

    -- * Response Lenses
    getBasePathMappingsResponse_items,
    getBasePathMappingsResponse_position,
    getBasePathMappingsResponse_httpStatus,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to get information about a collection of BasePathMapping
-- resources.
--
-- /See:/ 'newGetBasePathMappings' smart constructor.
data GetBasePathMappings = GetBasePathMappings'
  { -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Prelude.Maybe Prelude.Int,
    -- | The current pagination position in the paged result set.
    position :: Prelude.Maybe Prelude.Text,
    -- | The domain name of a BasePathMapping resource.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBasePathMappings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'getBasePathMappings_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'position', 'getBasePathMappings_position' - The current pagination position in the paged result set.
--
-- 'domainName', 'getBasePathMappings_domainName' - The domain name of a BasePathMapping resource.
newGetBasePathMappings ::
  -- | 'domainName'
  Prelude.Text ->
  GetBasePathMappings
newGetBasePathMappings pDomainName_ =
  GetBasePathMappings'
    { limit = Prelude.Nothing,
      position = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getBasePathMappings_limit :: Lens.Lens' GetBasePathMappings (Prelude.Maybe Prelude.Int)
getBasePathMappings_limit = Lens.lens (\GetBasePathMappings' {limit} -> limit) (\s@GetBasePathMappings' {} a -> s {limit = a} :: GetBasePathMappings)

-- | The current pagination position in the paged result set.
getBasePathMappings_position :: Lens.Lens' GetBasePathMappings (Prelude.Maybe Prelude.Text)
getBasePathMappings_position = Lens.lens (\GetBasePathMappings' {position} -> position) (\s@GetBasePathMappings' {} a -> s {position = a} :: GetBasePathMappings)

-- | The domain name of a BasePathMapping resource.
getBasePathMappings_domainName :: Lens.Lens' GetBasePathMappings Prelude.Text
getBasePathMappings_domainName = Lens.lens (\GetBasePathMappings' {domainName} -> domainName) (\s@GetBasePathMappings' {} a -> s {domainName = a} :: GetBasePathMappings)

instance Core.AWSPager GetBasePathMappings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getBasePathMappingsResponse_position
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getBasePathMappingsResponse_items
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getBasePathMappings_position
          Lens..~ rs
          Lens.^? getBasePathMappingsResponse_position
            Prelude.. Lens._Just

instance Core.AWSRequest GetBasePathMappings where
  type
    AWSResponse GetBasePathMappings =
      GetBasePathMappingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBasePathMappingsResponse'
            Prelude.<$> (x Data..?> "item" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBasePathMappings where
  hashWithSalt _salt GetBasePathMappings' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData GetBasePathMappings where
  rnf GetBasePathMappings' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders GetBasePathMappings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetBasePathMappings where
  toPath GetBasePathMappings' {..} =
    Prelude.mconcat
      [ "/domainnames/",
        Data.toBS domainName,
        "/basepathmappings"
      ]

instance Data.ToQuery GetBasePathMappings where
  toQuery GetBasePathMappings' {..} =
    Prelude.mconcat
      ["limit" Data.=: limit, "position" Data.=: position]

-- | Represents a collection of BasePathMapping resources.
--
-- /See:/ 'newGetBasePathMappingsResponse' smart constructor.
data GetBasePathMappingsResponse = GetBasePathMappingsResponse'
  { -- | The current page of elements from this collection.
    items :: Prelude.Maybe [BasePathMapping],
    position :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBasePathMappingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getBasePathMappingsResponse_items' - The current page of elements from this collection.
--
-- 'position', 'getBasePathMappingsResponse_position' - Undocumented member.
--
-- 'httpStatus', 'getBasePathMappingsResponse_httpStatus' - The response's http status code.
newGetBasePathMappingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBasePathMappingsResponse
newGetBasePathMappingsResponse pHttpStatus_ =
  GetBasePathMappingsResponse'
    { items =
        Prelude.Nothing,
      position = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getBasePathMappingsResponse_items :: Lens.Lens' GetBasePathMappingsResponse (Prelude.Maybe [BasePathMapping])
getBasePathMappingsResponse_items = Lens.lens (\GetBasePathMappingsResponse' {items} -> items) (\s@GetBasePathMappingsResponse' {} a -> s {items = a} :: GetBasePathMappingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getBasePathMappingsResponse_position :: Lens.Lens' GetBasePathMappingsResponse (Prelude.Maybe Prelude.Text)
getBasePathMappingsResponse_position = Lens.lens (\GetBasePathMappingsResponse' {position} -> position) (\s@GetBasePathMappingsResponse' {} a -> s {position = a} :: GetBasePathMappingsResponse)

-- | The response's http status code.
getBasePathMappingsResponse_httpStatus :: Lens.Lens' GetBasePathMappingsResponse Prelude.Int
getBasePathMappingsResponse_httpStatus = Lens.lens (\GetBasePathMappingsResponse' {httpStatus} -> httpStatus) (\s@GetBasePathMappingsResponse' {} a -> s {httpStatus = a} :: GetBasePathMappingsResponse)

instance Prelude.NFData GetBasePathMappingsResponse where
  rnf GetBasePathMappingsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf httpStatus
