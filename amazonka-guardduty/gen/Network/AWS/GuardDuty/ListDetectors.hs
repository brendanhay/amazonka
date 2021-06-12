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
-- Module      : Network.AWS.GuardDuty.ListDetectors
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists detectorIds of all the existing Amazon GuardDuty detector
-- resources.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListDetectors
  ( -- * Creating a Request
    ListDetectors (..),
    newListDetectors,

    -- * Request Lenses
    listDetectors_nextToken,
    listDetectors_maxResults,

    -- * Destructuring the Response
    ListDetectorsResponse (..),
    newListDetectorsResponse,

    -- * Response Lenses
    listDetectorsResponse_nextToken,
    listDetectorsResponse_httpStatus,
    listDetectorsResponse_detectorIds,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDetectors' smart constructor.
data ListDetectors = ListDetectors'
  { -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the list action. For
    -- subsequent calls to the action, fill nextToken in the request with the
    -- value of NextToken from the previous response to continue listing data.
    nextToken :: Core.Maybe Core.Text,
    -- | You can use this parameter to indicate the maximum number of items that
    -- you want in the response. The default value is 50. The maximum value is
    -- 50.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDetectors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDetectors_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the list action. For
-- subsequent calls to the action, fill nextToken in the request with the
-- value of NextToken from the previous response to continue listing data.
--
-- 'maxResults', 'listDetectors_maxResults' - You can use this parameter to indicate the maximum number of items that
-- you want in the response. The default value is 50. The maximum value is
-- 50.
newListDetectors ::
  ListDetectors
newListDetectors =
  ListDetectors'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the list action. For
-- subsequent calls to the action, fill nextToken in the request with the
-- value of NextToken from the previous response to continue listing data.
listDetectors_nextToken :: Lens.Lens' ListDetectors (Core.Maybe Core.Text)
listDetectors_nextToken = Lens.lens (\ListDetectors' {nextToken} -> nextToken) (\s@ListDetectors' {} a -> s {nextToken = a} :: ListDetectors)

-- | You can use this parameter to indicate the maximum number of items that
-- you want in the response. The default value is 50. The maximum value is
-- 50.
listDetectors_maxResults :: Lens.Lens' ListDetectors (Core.Maybe Core.Natural)
listDetectors_maxResults = Lens.lens (\ListDetectors' {maxResults} -> maxResults) (\s@ListDetectors' {} a -> s {maxResults = a} :: ListDetectors)

instance Core.AWSPager ListDetectors where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDetectorsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^. listDetectorsResponse_detectorIds) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDetectors_nextToken
          Lens..~ rs
          Lens.^? listDetectorsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListDetectors where
  type
    AWSResponse ListDetectors =
      ListDetectorsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDetectorsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "detectorIds" Core..!@ Core.mempty)
      )

instance Core.Hashable ListDetectors

instance Core.NFData ListDetectors

instance Core.ToHeaders ListDetectors where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListDetectors where
  toPath = Core.const "/detector"

instance Core.ToQuery ListDetectors where
  toQuery ListDetectors' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListDetectorsResponse' smart constructor.
data ListDetectorsResponse = ListDetectorsResponse'
  { -- | The pagination parameter to be used on the next list operation to
    -- retrieve more items.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of detector IDs.
    detectorIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDetectorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDetectorsResponse_nextToken' - The pagination parameter to be used on the next list operation to
-- retrieve more items.
--
-- 'httpStatus', 'listDetectorsResponse_httpStatus' - The response's http status code.
--
-- 'detectorIds', 'listDetectorsResponse_detectorIds' - A list of detector IDs.
newListDetectorsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDetectorsResponse
newListDetectorsResponse pHttpStatus_ =
  ListDetectorsResponse'
    { nextToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      detectorIds = Core.mempty
    }

-- | The pagination parameter to be used on the next list operation to
-- retrieve more items.
listDetectorsResponse_nextToken :: Lens.Lens' ListDetectorsResponse (Core.Maybe Core.Text)
listDetectorsResponse_nextToken = Lens.lens (\ListDetectorsResponse' {nextToken} -> nextToken) (\s@ListDetectorsResponse' {} a -> s {nextToken = a} :: ListDetectorsResponse)

-- | The response's http status code.
listDetectorsResponse_httpStatus :: Lens.Lens' ListDetectorsResponse Core.Int
listDetectorsResponse_httpStatus = Lens.lens (\ListDetectorsResponse' {httpStatus} -> httpStatus) (\s@ListDetectorsResponse' {} a -> s {httpStatus = a} :: ListDetectorsResponse)

-- | A list of detector IDs.
listDetectorsResponse_detectorIds :: Lens.Lens' ListDetectorsResponse [Core.Text]
listDetectorsResponse_detectorIds = Lens.lens (\ListDetectorsResponse' {detectorIds} -> detectorIds) (\s@ListDetectorsResponse' {} a -> s {detectorIds = a} :: ListDetectorsResponse) Core.. Lens._Coerce

instance Core.NFData ListDetectorsResponse
