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
-- Module      : Network.AWS.StorageGateway.ListGateways
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists gateways owned by an AWS account in an AWS Region specified in the
-- request. The returned list is ordered by gateway Amazon Resource Name
-- (ARN).
--
-- By default, the operation returns a maximum of 100 gateways. This
-- operation supports pagination that allows you to optionally reduce the
-- number of gateways returned in a response.
--
-- If you have more gateways than are returned in a response (that is, the
-- response returns only a truncated list of your gateways), the response
-- contains a marker that you can specify in your next request to fetch the
-- next page of gateways.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.ListGateways
  ( -- * Creating a Request
    ListGateways (..),
    newListGateways,

    -- * Request Lenses
    listGateways_limit,
    listGateways_marker,

    -- * Destructuring the Response
    ListGatewaysResponse (..),
    newListGatewaysResponse,

    -- * Response Lenses
    listGatewaysResponse_gateways,
    listGatewaysResponse_marker,
    listGatewaysResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing zero or more of the following fields:
--
-- -   ListGatewaysInput$Limit
--
-- -   ListGatewaysInput$Marker
--
-- /See:/ 'newListGateways' smart constructor.
data ListGateways = ListGateways'
  { -- | Specifies that the list of gateways returned be limited to the specified
    -- number of items.
    limit :: Core.Maybe Core.Natural,
    -- | An opaque string that indicates the position at which to begin the
    -- returned list of gateways.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listGateways_limit' - Specifies that the list of gateways returned be limited to the specified
-- number of items.
--
-- 'marker', 'listGateways_marker' - An opaque string that indicates the position at which to begin the
-- returned list of gateways.
newListGateways ::
  ListGateways
newListGateways =
  ListGateways'
    { limit = Core.Nothing,
      marker = Core.Nothing
    }

-- | Specifies that the list of gateways returned be limited to the specified
-- number of items.
listGateways_limit :: Lens.Lens' ListGateways (Core.Maybe Core.Natural)
listGateways_limit = Lens.lens (\ListGateways' {limit} -> limit) (\s@ListGateways' {} a -> s {limit = a} :: ListGateways)

-- | An opaque string that indicates the position at which to begin the
-- returned list of gateways.
listGateways_marker :: Lens.Lens' ListGateways (Core.Maybe Core.Text)
listGateways_marker = Lens.lens (\ListGateways' {marker} -> marker) (\s@ListGateways' {} a -> s {marker = a} :: ListGateways)

instance Core.AWSPager ListGateways where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGatewaysResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listGatewaysResponse_gateways Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listGateways_marker
          Lens..~ rs
          Lens.^? listGatewaysResponse_marker Core.. Lens._Just

instance Core.AWSRequest ListGateways where
  type AWSResponse ListGateways = ListGatewaysResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGatewaysResponse'
            Core.<$> (x Core..?> "Gateways" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListGateways

instance Core.NFData ListGateways

instance Core.ToHeaders ListGateways where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.ListGateways" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListGateways where
  toJSON ListGateways' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("Marker" Core..=) Core.<$> marker
          ]
      )

instance Core.ToPath ListGateways where
  toPath = Core.const "/"

instance Core.ToQuery ListGateways where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListGatewaysResponse' smart constructor.
data ListGatewaysResponse = ListGatewaysResponse'
  { -- | An array of GatewayInfo objects.
    gateways :: Core.Maybe [GatewayInfo],
    -- | Use the marker in your next request to fetch the next set of gateways in
    -- the list. If there are no more gateways to list, this field does not
    -- appear in the response.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListGatewaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gateways', 'listGatewaysResponse_gateways' - An array of GatewayInfo objects.
--
-- 'marker', 'listGatewaysResponse_marker' - Use the marker in your next request to fetch the next set of gateways in
-- the list. If there are no more gateways to list, this field does not
-- appear in the response.
--
-- 'httpStatus', 'listGatewaysResponse_httpStatus' - The response's http status code.
newListGatewaysResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListGatewaysResponse
newListGatewaysResponse pHttpStatus_ =
  ListGatewaysResponse'
    { gateways = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of GatewayInfo objects.
listGatewaysResponse_gateways :: Lens.Lens' ListGatewaysResponse (Core.Maybe [GatewayInfo])
listGatewaysResponse_gateways = Lens.lens (\ListGatewaysResponse' {gateways} -> gateways) (\s@ListGatewaysResponse' {} a -> s {gateways = a} :: ListGatewaysResponse) Core.. Lens.mapping Lens._Coerce

-- | Use the marker in your next request to fetch the next set of gateways in
-- the list. If there are no more gateways to list, this field does not
-- appear in the response.
listGatewaysResponse_marker :: Lens.Lens' ListGatewaysResponse (Core.Maybe Core.Text)
listGatewaysResponse_marker = Lens.lens (\ListGatewaysResponse' {marker} -> marker) (\s@ListGatewaysResponse' {} a -> s {marker = a} :: ListGatewaysResponse)

-- | The response's http status code.
listGatewaysResponse_httpStatus :: Lens.Lens' ListGatewaysResponse Core.Int
listGatewaysResponse_httpStatus = Lens.lens (\ListGatewaysResponse' {httpStatus} -> httpStatus) (\s@ListGatewaysResponse' {} a -> s {httpStatus = a} :: ListGatewaysResponse)

instance Core.NFData ListGatewaysResponse
