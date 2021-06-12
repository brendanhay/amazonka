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
-- Module      : Network.AWS.APIGateway.GetVpcLinks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the VpcLinks collection under the caller\'s account in a selected
-- region.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetVpcLinks
  ( -- * Creating a Request
    GetVpcLinks (..),
    newGetVpcLinks,

    -- * Request Lenses
    getVpcLinks_position,
    getVpcLinks_limit,

    -- * Destructuring the Response
    GetVpcLinksResponse (..),
    newGetVpcLinksResponse,

    -- * Response Lenses
    getVpcLinksResponse_items,
    getVpcLinksResponse_position,
    getVpcLinksResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Gets the VpcLinks collection under the caller\'s account in a selected
-- region.
--
-- /See:/ 'newGetVpcLinks' smart constructor.
data GetVpcLinks = GetVpcLinks'
  { -- | The current pagination position in the paged result set.
    position :: Core.Maybe Core.Text,
    -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetVpcLinks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'position', 'getVpcLinks_position' - The current pagination position in the paged result set.
--
-- 'limit', 'getVpcLinks_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
newGetVpcLinks ::
  GetVpcLinks
newGetVpcLinks =
  GetVpcLinks'
    { position = Core.Nothing,
      limit = Core.Nothing
    }

-- | The current pagination position in the paged result set.
getVpcLinks_position :: Lens.Lens' GetVpcLinks (Core.Maybe Core.Text)
getVpcLinks_position = Lens.lens (\GetVpcLinks' {position} -> position) (\s@GetVpcLinks' {} a -> s {position = a} :: GetVpcLinks)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getVpcLinks_limit :: Lens.Lens' GetVpcLinks (Core.Maybe Core.Int)
getVpcLinks_limit = Lens.lens (\GetVpcLinks' {limit} -> limit) (\s@GetVpcLinks' {} a -> s {limit = a} :: GetVpcLinks)

instance Core.AWSPager GetVpcLinks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getVpcLinksResponse_position Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getVpcLinksResponse_items Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getVpcLinks_position
          Lens..~ rs
          Lens.^? getVpcLinksResponse_position Core.. Lens._Just

instance Core.AWSRequest GetVpcLinks where
  type AWSResponse GetVpcLinks = GetVpcLinksResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVpcLinksResponse'
            Core.<$> (x Core..?> "item" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "position")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetVpcLinks

instance Core.NFData GetVpcLinks

instance Core.ToHeaders GetVpcLinks where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetVpcLinks where
  toPath = Core.const "/vpclinks"

instance Core.ToQuery GetVpcLinks where
  toQuery GetVpcLinks' {..} =
    Core.mconcat
      ["position" Core.=: position, "limit" Core.=: limit]

-- | The collection of VPC links under the caller\'s account in a region.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/getting-started-with-private-integration.html Getting Started with Private Integrations>,
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-private-integration.html Set up Private Integrations>
--
-- /See:/ 'newGetVpcLinksResponse' smart constructor.
data GetVpcLinksResponse = GetVpcLinksResponse'
  { -- | The current page of elements from this collection.
    items :: Core.Maybe [VpcLink],
    position :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetVpcLinksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getVpcLinksResponse_items' - The current page of elements from this collection.
--
-- 'position', 'getVpcLinksResponse_position' - Undocumented member.
--
-- 'httpStatus', 'getVpcLinksResponse_httpStatus' - The response's http status code.
newGetVpcLinksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetVpcLinksResponse
newGetVpcLinksResponse pHttpStatus_ =
  GetVpcLinksResponse'
    { items = Core.Nothing,
      position = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getVpcLinksResponse_items :: Lens.Lens' GetVpcLinksResponse (Core.Maybe [VpcLink])
getVpcLinksResponse_items = Lens.lens (\GetVpcLinksResponse' {items} -> items) (\s@GetVpcLinksResponse' {} a -> s {items = a} :: GetVpcLinksResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
getVpcLinksResponse_position :: Lens.Lens' GetVpcLinksResponse (Core.Maybe Core.Text)
getVpcLinksResponse_position = Lens.lens (\GetVpcLinksResponse' {position} -> position) (\s@GetVpcLinksResponse' {} a -> s {position = a} :: GetVpcLinksResponse)

-- | The response's http status code.
getVpcLinksResponse_httpStatus :: Lens.Lens' GetVpcLinksResponse Core.Int
getVpcLinksResponse_httpStatus = Lens.lens (\GetVpcLinksResponse' {httpStatus} -> httpStatus) (\s@GetVpcLinksResponse' {} a -> s {httpStatus = a} :: GetVpcLinksResponse)

instance Core.NFData GetVpcLinksResponse
