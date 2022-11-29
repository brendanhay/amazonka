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
-- Module      : Amazonka.StorageGateway.ListGateways
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists gateways owned by an Amazon Web Services account in an Amazon Web
-- Services Region specified in the request. The returned list is ordered
-- by gateway Amazon Resource Name (ARN).
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
module Amazonka.StorageGateway.ListGateways
  ( -- * Creating a Request
    ListGateways (..),
    newListGateways,

    -- * Request Lenses
    listGateways_marker,
    listGateways_limit,

    -- * Destructuring the Response
    ListGatewaysResponse (..),
    newListGatewaysResponse,

    -- * Response Lenses
    listGatewaysResponse_marker,
    listGatewaysResponse_gateways,
    listGatewaysResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | A JSON object containing zero or more of the following fields:
--
-- -   ListGatewaysInput$Limit
--
-- -   ListGatewaysInput$Marker
--
-- /See:/ 'newListGateways' smart constructor.
data ListGateways = ListGateways'
  { -- | An opaque string that indicates the position at which to begin the
    -- returned list of gateways.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Specifies that the list of gateways returned be limited to the specified
    -- number of items.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listGateways_marker' - An opaque string that indicates the position at which to begin the
-- returned list of gateways.
--
-- 'limit', 'listGateways_limit' - Specifies that the list of gateways returned be limited to the specified
-- number of items.
newListGateways ::
  ListGateways
newListGateways =
  ListGateways'
    { marker = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | An opaque string that indicates the position at which to begin the
-- returned list of gateways.
listGateways_marker :: Lens.Lens' ListGateways (Prelude.Maybe Prelude.Text)
listGateways_marker = Lens.lens (\ListGateways' {marker} -> marker) (\s@ListGateways' {} a -> s {marker = a} :: ListGateways)

-- | Specifies that the list of gateways returned be limited to the specified
-- number of items.
listGateways_limit :: Lens.Lens' ListGateways (Prelude.Maybe Prelude.Natural)
listGateways_limit = Lens.lens (\ListGateways' {limit} -> limit) (\s@ListGateways' {} a -> s {limit = a} :: ListGateways)

instance Core.AWSPager ListGateways where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGatewaysResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listGatewaysResponse_gateways Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listGateways_marker
          Lens..~ rs
          Lens.^? listGatewaysResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest ListGateways where
  type AWSResponse ListGateways = ListGatewaysResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGatewaysResponse'
            Prelude.<$> (x Core..?> "Marker")
            Prelude.<*> (x Core..?> "Gateways" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGateways where
  hashWithSalt _salt ListGateways' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` limit

instance Prelude.NFData ListGateways where
  rnf ListGateways' {..} =
    Prelude.rnf marker `Prelude.seq` Prelude.rnf limit

instance Core.ToHeaders ListGateways where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.ListGateways" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListGateways where
  toJSON ListGateways' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Marker" Core..=) Prelude.<$> marker,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath ListGateways where
  toPath = Prelude.const "/"

instance Core.ToQuery ListGateways where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListGatewaysResponse' smart constructor.
data ListGatewaysResponse = ListGatewaysResponse'
  { -- | Use the marker in your next request to fetch the next set of gateways in
    -- the list. If there are no more gateways to list, this field does not
    -- appear in the response.
    marker :: Prelude.Maybe Prelude.Text,
    -- | An array of GatewayInfo objects.
    gateways :: Prelude.Maybe [GatewayInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGatewaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listGatewaysResponse_marker' - Use the marker in your next request to fetch the next set of gateways in
-- the list. If there are no more gateways to list, this field does not
-- appear in the response.
--
-- 'gateways', 'listGatewaysResponse_gateways' - An array of GatewayInfo objects.
--
-- 'httpStatus', 'listGatewaysResponse_httpStatus' - The response's http status code.
newListGatewaysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGatewaysResponse
newListGatewaysResponse pHttpStatus_ =
  ListGatewaysResponse'
    { marker = Prelude.Nothing,
      gateways = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Use the marker in your next request to fetch the next set of gateways in
-- the list. If there are no more gateways to list, this field does not
-- appear in the response.
listGatewaysResponse_marker :: Lens.Lens' ListGatewaysResponse (Prelude.Maybe Prelude.Text)
listGatewaysResponse_marker = Lens.lens (\ListGatewaysResponse' {marker} -> marker) (\s@ListGatewaysResponse' {} a -> s {marker = a} :: ListGatewaysResponse)

-- | An array of GatewayInfo objects.
listGatewaysResponse_gateways :: Lens.Lens' ListGatewaysResponse (Prelude.Maybe [GatewayInfo])
listGatewaysResponse_gateways = Lens.lens (\ListGatewaysResponse' {gateways} -> gateways) (\s@ListGatewaysResponse' {} a -> s {gateways = a} :: ListGatewaysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listGatewaysResponse_httpStatus :: Lens.Lens' ListGatewaysResponse Prelude.Int
listGatewaysResponse_httpStatus = Lens.lens (\ListGatewaysResponse' {httpStatus} -> httpStatus) (\s@ListGatewaysResponse' {} a -> s {httpStatus = a} :: ListGatewaysResponse)

instance Prelude.NFData ListGatewaysResponse where
  rnf ListGatewaysResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf gateways
      `Prelude.seq` Prelude.rnf httpStatus
