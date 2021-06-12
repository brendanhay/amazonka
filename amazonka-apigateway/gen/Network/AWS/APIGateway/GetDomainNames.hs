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
-- Module      : Network.AWS.APIGateway.GetDomainNames
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a collection of DomainName resources.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetDomainNames
  ( -- * Creating a Request
    GetDomainNames (..),
    newGetDomainNames,

    -- * Request Lenses
    getDomainNames_position,
    getDomainNames_limit,

    -- * Destructuring the Response
    GetDomainNamesResponse (..),
    newGetDomainNamesResponse,

    -- * Response Lenses
    getDomainNamesResponse_items,
    getDomainNamesResponse_position,
    getDomainNamesResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to describe a collection of DomainName resources.
--
-- /See:/ 'newGetDomainNames' smart constructor.
data GetDomainNames = GetDomainNames'
  { -- | The current pagination position in the paged result set.
    position :: Core.Maybe Core.Text,
    -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDomainNames' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'position', 'getDomainNames_position' - The current pagination position in the paged result set.
--
-- 'limit', 'getDomainNames_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
newGetDomainNames ::
  GetDomainNames
newGetDomainNames =
  GetDomainNames'
    { position = Core.Nothing,
      limit = Core.Nothing
    }

-- | The current pagination position in the paged result set.
getDomainNames_position :: Lens.Lens' GetDomainNames (Core.Maybe Core.Text)
getDomainNames_position = Lens.lens (\GetDomainNames' {position} -> position) (\s@GetDomainNames' {} a -> s {position = a} :: GetDomainNames)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getDomainNames_limit :: Lens.Lens' GetDomainNames (Core.Maybe Core.Int)
getDomainNames_limit = Lens.lens (\GetDomainNames' {limit} -> limit) (\s@GetDomainNames' {} a -> s {limit = a} :: GetDomainNames)

instance Core.AWSPager GetDomainNames where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDomainNamesResponse_position Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getDomainNamesResponse_items Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getDomainNames_position
          Lens..~ rs
          Lens.^? getDomainNamesResponse_position Core.. Lens._Just

instance Core.AWSRequest GetDomainNames where
  type
    AWSResponse GetDomainNames =
      GetDomainNamesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainNamesResponse'
            Core.<$> (x Core..?> "item" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "position")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDomainNames

instance Core.NFData GetDomainNames

instance Core.ToHeaders GetDomainNames where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetDomainNames where
  toPath = Core.const "/domainnames"

instance Core.ToQuery GetDomainNames where
  toQuery GetDomainNames' {..} =
    Core.mconcat
      ["position" Core.=: position, "limit" Core.=: limit]

-- | Represents a collection of DomainName resources.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html Use Client-Side Certificate>
--
-- /See:/ 'newGetDomainNamesResponse' smart constructor.
data GetDomainNamesResponse = GetDomainNamesResponse'
  { -- | The current page of elements from this collection.
    items :: Core.Maybe [DomainName],
    position :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetDomainNamesResponse
newGetDomainNamesResponse pHttpStatus_ =
  GetDomainNamesResponse'
    { items = Core.Nothing,
      position = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getDomainNamesResponse_items :: Lens.Lens' GetDomainNamesResponse (Core.Maybe [DomainName])
getDomainNamesResponse_items = Lens.lens (\GetDomainNamesResponse' {items} -> items) (\s@GetDomainNamesResponse' {} a -> s {items = a} :: GetDomainNamesResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
getDomainNamesResponse_position :: Lens.Lens' GetDomainNamesResponse (Core.Maybe Core.Text)
getDomainNamesResponse_position = Lens.lens (\GetDomainNamesResponse' {position} -> position) (\s@GetDomainNamesResponse' {} a -> s {position = a} :: GetDomainNamesResponse)

-- | The response's http status code.
getDomainNamesResponse_httpStatus :: Lens.Lens' GetDomainNamesResponse Core.Int
getDomainNamesResponse_httpStatus = Lens.lens (\GetDomainNamesResponse' {httpStatus} -> httpStatus) (\s@GetDomainNamesResponse' {} a -> s {httpStatus = a} :: GetDomainNamesResponse)

instance Core.NFData GetDomainNamesResponse
