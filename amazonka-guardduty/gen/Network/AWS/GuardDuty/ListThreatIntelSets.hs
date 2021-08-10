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
-- Module      : Network.AWS.GuardDuty.ListThreatIntelSets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the ThreatIntelSets of the GuardDuty service specified by the
-- detector ID. If you use this operation from a member account, the
-- ThreatIntelSets associated with the administrator account are returned.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListThreatIntelSets
  ( -- * Creating a Request
    ListThreatIntelSets (..),
    newListThreatIntelSets,

    -- * Request Lenses
    listThreatIntelSets_nextToken,
    listThreatIntelSets_maxResults,
    listThreatIntelSets_detectorId,

    -- * Destructuring the Response
    ListThreatIntelSetsResponse (..),
    newListThreatIntelSetsResponse,

    -- * Response Lenses
    listThreatIntelSetsResponse_nextToken,
    listThreatIntelSetsResponse_httpStatus,
    listThreatIntelSetsResponse_threatIntelSetIds,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListThreatIntelSets' smart constructor.
data ListThreatIntelSets = ListThreatIntelSets'
  { -- | You can use this parameter to paginate results in the response. Set the
    -- value of this parameter to null on your first call to the list action.
    -- For subsequent calls to the action, fill nextToken in the request with
    -- the value of NextToken from the previous response to continue listing
    -- data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | You can use this parameter to indicate the maximum number of items that
    -- you want in the response. The default value is 50. The maximum value is
    -- 50.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The unique ID of the detector that the threatIntelSet is associated
    -- with.
    detectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThreatIntelSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThreatIntelSets_nextToken' - You can use this parameter to paginate results in the response. Set the
-- value of this parameter to null on your first call to the list action.
-- For subsequent calls to the action, fill nextToken in the request with
-- the value of NextToken from the previous response to continue listing
-- data.
--
-- 'maxResults', 'listThreatIntelSets_maxResults' - You can use this parameter to indicate the maximum number of items that
-- you want in the response. The default value is 50. The maximum value is
-- 50.
--
-- 'detectorId', 'listThreatIntelSets_detectorId' - The unique ID of the detector that the threatIntelSet is associated
-- with.
newListThreatIntelSets ::
  -- | 'detectorId'
  Prelude.Text ->
  ListThreatIntelSets
newListThreatIntelSets pDetectorId_ =
  ListThreatIntelSets'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      detectorId = pDetectorId_
    }

-- | You can use this parameter to paginate results in the response. Set the
-- value of this parameter to null on your first call to the list action.
-- For subsequent calls to the action, fill nextToken in the request with
-- the value of NextToken from the previous response to continue listing
-- data.
listThreatIntelSets_nextToken :: Lens.Lens' ListThreatIntelSets (Prelude.Maybe Prelude.Text)
listThreatIntelSets_nextToken = Lens.lens (\ListThreatIntelSets' {nextToken} -> nextToken) (\s@ListThreatIntelSets' {} a -> s {nextToken = a} :: ListThreatIntelSets)

-- | You can use this parameter to indicate the maximum number of items that
-- you want in the response. The default value is 50. The maximum value is
-- 50.
listThreatIntelSets_maxResults :: Lens.Lens' ListThreatIntelSets (Prelude.Maybe Prelude.Natural)
listThreatIntelSets_maxResults = Lens.lens (\ListThreatIntelSets' {maxResults} -> maxResults) (\s@ListThreatIntelSets' {} a -> s {maxResults = a} :: ListThreatIntelSets)

-- | The unique ID of the detector that the threatIntelSet is associated
-- with.
listThreatIntelSets_detectorId :: Lens.Lens' ListThreatIntelSets Prelude.Text
listThreatIntelSets_detectorId = Lens.lens (\ListThreatIntelSets' {detectorId} -> detectorId) (\s@ListThreatIntelSets' {} a -> s {detectorId = a} :: ListThreatIntelSets)

instance Core.AWSPager ListThreatIntelSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listThreatIntelSetsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listThreatIntelSetsResponse_threatIntelSetIds
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listThreatIntelSets_nextToken
          Lens..~ rs
          Lens.^? listThreatIntelSetsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListThreatIntelSets where
  type
    AWSResponse ListThreatIntelSets =
      ListThreatIntelSetsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThreatIntelSetsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "threatIntelSetIds"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListThreatIntelSets

instance Prelude.NFData ListThreatIntelSets

instance Core.ToHeaders ListThreatIntelSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListThreatIntelSets where
  toPath ListThreatIntelSets' {..} =
    Prelude.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/threatintelset"
      ]

instance Core.ToQuery ListThreatIntelSets where
  toQuery ListThreatIntelSets' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListThreatIntelSetsResponse' smart constructor.
data ListThreatIntelSetsResponse = ListThreatIntelSetsResponse'
  { -- | The pagination parameter to be used on the next list operation to
    -- retrieve more items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The IDs of the ThreatIntelSet resources.
    threatIntelSetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThreatIntelSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThreatIntelSetsResponse_nextToken' - The pagination parameter to be used on the next list operation to
-- retrieve more items.
--
-- 'httpStatus', 'listThreatIntelSetsResponse_httpStatus' - The response's http status code.
--
-- 'threatIntelSetIds', 'listThreatIntelSetsResponse_threatIntelSetIds' - The IDs of the ThreatIntelSet resources.
newListThreatIntelSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListThreatIntelSetsResponse
newListThreatIntelSetsResponse pHttpStatus_ =
  ListThreatIntelSetsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      threatIntelSetIds = Prelude.mempty
    }

-- | The pagination parameter to be used on the next list operation to
-- retrieve more items.
listThreatIntelSetsResponse_nextToken :: Lens.Lens' ListThreatIntelSetsResponse (Prelude.Maybe Prelude.Text)
listThreatIntelSetsResponse_nextToken = Lens.lens (\ListThreatIntelSetsResponse' {nextToken} -> nextToken) (\s@ListThreatIntelSetsResponse' {} a -> s {nextToken = a} :: ListThreatIntelSetsResponse)

-- | The response's http status code.
listThreatIntelSetsResponse_httpStatus :: Lens.Lens' ListThreatIntelSetsResponse Prelude.Int
listThreatIntelSetsResponse_httpStatus = Lens.lens (\ListThreatIntelSetsResponse' {httpStatus} -> httpStatus) (\s@ListThreatIntelSetsResponse' {} a -> s {httpStatus = a} :: ListThreatIntelSetsResponse)

-- | The IDs of the ThreatIntelSet resources.
listThreatIntelSetsResponse_threatIntelSetIds :: Lens.Lens' ListThreatIntelSetsResponse [Prelude.Text]
listThreatIntelSetsResponse_threatIntelSetIds = Lens.lens (\ListThreatIntelSetsResponse' {threatIntelSetIds} -> threatIntelSetIds) (\s@ListThreatIntelSetsResponse' {} a -> s {threatIntelSetIds = a} :: ListThreatIntelSetsResponse) Prelude.. Lens._Coerce

instance Prelude.NFData ListThreatIntelSetsResponse
