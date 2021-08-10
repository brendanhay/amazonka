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
-- Module      : Network.AWS.GuardDuty.ListIPSets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the IPSets of the GuardDuty service specified by the detector ID.
-- If you use this operation from a member account, the IPSets returned are
-- the IPSets from the associated administrator account.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListIPSets
  ( -- * Creating a Request
    ListIPSets (..),
    newListIPSets,

    -- * Request Lenses
    listIPSets_nextToken,
    listIPSets_maxResults,
    listIPSets_detectorId,

    -- * Destructuring the Response
    ListIPSetsResponse (..),
    newListIPSetsResponse,

    -- * Response Lenses
    listIPSetsResponse_nextToken,
    listIPSetsResponse_httpStatus,
    listIPSetsResponse_ipSetIds,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListIPSets' smart constructor.
data ListIPSets = ListIPSets'
  { -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the list action. For
    -- subsequent calls to the action, fill nextToken in the request with the
    -- value of NextToken from the previous response to continue listing data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | You can use this parameter to indicate the maximum number of items you
    -- want in the response. The default value is 50. The maximum value is 50.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The unique ID of the detector that the IPSet is associated with.
    detectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIPSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIPSets_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the list action. For
-- subsequent calls to the action, fill nextToken in the request with the
-- value of NextToken from the previous response to continue listing data.
--
-- 'maxResults', 'listIPSets_maxResults' - You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 50. The maximum value is 50.
--
-- 'detectorId', 'listIPSets_detectorId' - The unique ID of the detector that the IPSet is associated with.
newListIPSets ::
  -- | 'detectorId'
  Prelude.Text ->
  ListIPSets
newListIPSets pDetectorId_ =
  ListIPSets'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      detectorId = pDetectorId_
    }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the list action. For
-- subsequent calls to the action, fill nextToken in the request with the
-- value of NextToken from the previous response to continue listing data.
listIPSets_nextToken :: Lens.Lens' ListIPSets (Prelude.Maybe Prelude.Text)
listIPSets_nextToken = Lens.lens (\ListIPSets' {nextToken} -> nextToken) (\s@ListIPSets' {} a -> s {nextToken = a} :: ListIPSets)

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 50. The maximum value is 50.
listIPSets_maxResults :: Lens.Lens' ListIPSets (Prelude.Maybe Prelude.Natural)
listIPSets_maxResults = Lens.lens (\ListIPSets' {maxResults} -> maxResults) (\s@ListIPSets' {} a -> s {maxResults = a} :: ListIPSets)

-- | The unique ID of the detector that the IPSet is associated with.
listIPSets_detectorId :: Lens.Lens' ListIPSets Prelude.Text
listIPSets_detectorId = Lens.lens (\ListIPSets' {detectorId} -> detectorId) (\s@ListIPSets' {} a -> s {detectorId = a} :: ListIPSets)

instance Core.AWSPager ListIPSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listIPSetsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop (rs Lens.^. listIPSetsResponse_ipSetIds) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listIPSets_nextToken
          Lens..~ rs
          Lens.^? listIPSetsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListIPSets where
  type AWSResponse ListIPSets = ListIPSetsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIPSetsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "ipSetIds" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListIPSets

instance Prelude.NFData ListIPSets

instance Core.ToHeaders ListIPSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListIPSets where
  toPath ListIPSets' {..} =
    Prelude.mconcat
      ["/detector/", Core.toBS detectorId, "/ipset"]

instance Core.ToQuery ListIPSets where
  toQuery ListIPSets' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListIPSetsResponse' smart constructor.
data ListIPSetsResponse = ListIPSetsResponse'
  { -- | The pagination parameter to be used on the next list operation to
    -- retrieve more items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The IDs of the IPSet resources.
    ipSetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIPSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIPSetsResponse_nextToken' - The pagination parameter to be used on the next list operation to
-- retrieve more items.
--
-- 'httpStatus', 'listIPSetsResponse_httpStatus' - The response's http status code.
--
-- 'ipSetIds', 'listIPSetsResponse_ipSetIds' - The IDs of the IPSet resources.
newListIPSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListIPSetsResponse
newListIPSetsResponse pHttpStatus_ =
  ListIPSetsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      ipSetIds = Prelude.mempty
    }

-- | The pagination parameter to be used on the next list operation to
-- retrieve more items.
listIPSetsResponse_nextToken :: Lens.Lens' ListIPSetsResponse (Prelude.Maybe Prelude.Text)
listIPSetsResponse_nextToken = Lens.lens (\ListIPSetsResponse' {nextToken} -> nextToken) (\s@ListIPSetsResponse' {} a -> s {nextToken = a} :: ListIPSetsResponse)

-- | The response's http status code.
listIPSetsResponse_httpStatus :: Lens.Lens' ListIPSetsResponse Prelude.Int
listIPSetsResponse_httpStatus = Lens.lens (\ListIPSetsResponse' {httpStatus} -> httpStatus) (\s@ListIPSetsResponse' {} a -> s {httpStatus = a} :: ListIPSetsResponse)

-- | The IDs of the IPSet resources.
listIPSetsResponse_ipSetIds :: Lens.Lens' ListIPSetsResponse [Prelude.Text]
listIPSetsResponse_ipSetIds = Lens.lens (\ListIPSetsResponse' {ipSetIds} -> ipSetIds) (\s@ListIPSetsResponse' {} a -> s {ipSetIds = a} :: ListIPSetsResponse) Prelude.. Lens._Coerce

instance Prelude.NFData ListIPSetsResponse
