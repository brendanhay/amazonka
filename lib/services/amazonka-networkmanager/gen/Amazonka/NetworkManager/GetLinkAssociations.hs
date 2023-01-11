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
-- Module      : Amazonka.NetworkManager.GetLinkAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the link associations for a device or a link. Either the device ID
-- or the link ID must be specified.
--
-- This operation returns paginated results.
module Amazonka.NetworkManager.GetLinkAssociations
  ( -- * Creating a Request
    GetLinkAssociations (..),
    newGetLinkAssociations,

    -- * Request Lenses
    getLinkAssociations_deviceId,
    getLinkAssociations_linkId,
    getLinkAssociations_maxResults,
    getLinkAssociations_nextToken,
    getLinkAssociations_globalNetworkId,

    -- * Destructuring the Response
    GetLinkAssociationsResponse (..),
    newGetLinkAssociationsResponse,

    -- * Response Lenses
    getLinkAssociationsResponse_linkAssociations,
    getLinkAssociationsResponse_nextToken,
    getLinkAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLinkAssociations' smart constructor.
data GetLinkAssociations = GetLinkAssociations'
  { -- | The ID of the device.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the link.
    linkId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLinkAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceId', 'getLinkAssociations_deviceId' - The ID of the device.
--
-- 'linkId', 'getLinkAssociations_linkId' - The ID of the link.
--
-- 'maxResults', 'getLinkAssociations_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'getLinkAssociations_nextToken' - The token for the next page of results.
--
-- 'globalNetworkId', 'getLinkAssociations_globalNetworkId' - The ID of the global network.
newGetLinkAssociations ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  GetLinkAssociations
newGetLinkAssociations pGlobalNetworkId_ =
  GetLinkAssociations'
    { deviceId = Prelude.Nothing,
      linkId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      globalNetworkId = pGlobalNetworkId_
    }

-- | The ID of the device.
getLinkAssociations_deviceId :: Lens.Lens' GetLinkAssociations (Prelude.Maybe Prelude.Text)
getLinkAssociations_deviceId = Lens.lens (\GetLinkAssociations' {deviceId} -> deviceId) (\s@GetLinkAssociations' {} a -> s {deviceId = a} :: GetLinkAssociations)

-- | The ID of the link.
getLinkAssociations_linkId :: Lens.Lens' GetLinkAssociations (Prelude.Maybe Prelude.Text)
getLinkAssociations_linkId = Lens.lens (\GetLinkAssociations' {linkId} -> linkId) (\s@GetLinkAssociations' {} a -> s {linkId = a} :: GetLinkAssociations)

-- | The maximum number of results to return.
getLinkAssociations_maxResults :: Lens.Lens' GetLinkAssociations (Prelude.Maybe Prelude.Natural)
getLinkAssociations_maxResults = Lens.lens (\GetLinkAssociations' {maxResults} -> maxResults) (\s@GetLinkAssociations' {} a -> s {maxResults = a} :: GetLinkAssociations)

-- | The token for the next page of results.
getLinkAssociations_nextToken :: Lens.Lens' GetLinkAssociations (Prelude.Maybe Prelude.Text)
getLinkAssociations_nextToken = Lens.lens (\GetLinkAssociations' {nextToken} -> nextToken) (\s@GetLinkAssociations' {} a -> s {nextToken = a} :: GetLinkAssociations)

-- | The ID of the global network.
getLinkAssociations_globalNetworkId :: Lens.Lens' GetLinkAssociations Prelude.Text
getLinkAssociations_globalNetworkId = Lens.lens (\GetLinkAssociations' {globalNetworkId} -> globalNetworkId) (\s@GetLinkAssociations' {} a -> s {globalNetworkId = a} :: GetLinkAssociations)

instance Core.AWSPager GetLinkAssociations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getLinkAssociationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getLinkAssociationsResponse_linkAssociations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getLinkAssociations_nextToken
          Lens..~ rs
          Lens.^? getLinkAssociationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetLinkAssociations where
  type
    AWSResponse GetLinkAssociations =
      GetLinkAssociationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLinkAssociationsResponse'
            Prelude.<$> ( x Data..?> "LinkAssociations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLinkAssociations where
  hashWithSalt _salt GetLinkAssociations' {..} =
    _salt `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` linkId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` globalNetworkId

instance Prelude.NFData GetLinkAssociations where
  rnf GetLinkAssociations' {..} =
    Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf linkId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf globalNetworkId

instance Data.ToHeaders GetLinkAssociations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetLinkAssociations where
  toPath GetLinkAssociations' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/link-associations"
      ]

instance Data.ToQuery GetLinkAssociations where
  toQuery GetLinkAssociations' {..} =
    Prelude.mconcat
      [ "deviceId" Data.=: deviceId,
        "linkId" Data.=: linkId,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetLinkAssociationsResponse' smart constructor.
data GetLinkAssociationsResponse = GetLinkAssociationsResponse'
  { -- | The link associations.
    linkAssociations :: Prelude.Maybe [LinkAssociation],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLinkAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'linkAssociations', 'getLinkAssociationsResponse_linkAssociations' - The link associations.
--
-- 'nextToken', 'getLinkAssociationsResponse_nextToken' - The token for the next page of results.
--
-- 'httpStatus', 'getLinkAssociationsResponse_httpStatus' - The response's http status code.
newGetLinkAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLinkAssociationsResponse
newGetLinkAssociationsResponse pHttpStatus_ =
  GetLinkAssociationsResponse'
    { linkAssociations =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The link associations.
getLinkAssociationsResponse_linkAssociations :: Lens.Lens' GetLinkAssociationsResponse (Prelude.Maybe [LinkAssociation])
getLinkAssociationsResponse_linkAssociations = Lens.lens (\GetLinkAssociationsResponse' {linkAssociations} -> linkAssociations) (\s@GetLinkAssociationsResponse' {} a -> s {linkAssociations = a} :: GetLinkAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
getLinkAssociationsResponse_nextToken :: Lens.Lens' GetLinkAssociationsResponse (Prelude.Maybe Prelude.Text)
getLinkAssociationsResponse_nextToken = Lens.lens (\GetLinkAssociationsResponse' {nextToken} -> nextToken) (\s@GetLinkAssociationsResponse' {} a -> s {nextToken = a} :: GetLinkAssociationsResponse)

-- | The response's http status code.
getLinkAssociationsResponse_httpStatus :: Lens.Lens' GetLinkAssociationsResponse Prelude.Int
getLinkAssociationsResponse_httpStatus = Lens.lens (\GetLinkAssociationsResponse' {httpStatus} -> httpStatus) (\s@GetLinkAssociationsResponse' {} a -> s {httpStatus = a} :: GetLinkAssociationsResponse)

instance Prelude.NFData GetLinkAssociationsResponse where
  rnf GetLinkAssociationsResponse' {..} =
    Prelude.rnf linkAssociations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
