{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDetectors' smart constructor.
data ListDetectors = ListDetectors'
  { -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the list action. For
    -- subsequent calls to the action, fill nextToken in the request with the
    -- value of NextToken from the previous response to continue listing data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | You can use this parameter to indicate the maximum number of items that
    -- you want in the response. The default value is 50. The maximum value is
    -- 50.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the list action. For
-- subsequent calls to the action, fill nextToken in the request with the
-- value of NextToken from the previous response to continue listing data.
listDetectors_nextToken :: Lens.Lens' ListDetectors (Prelude.Maybe Prelude.Text)
listDetectors_nextToken = Lens.lens (\ListDetectors' {nextToken} -> nextToken) (\s@ListDetectors' {} a -> s {nextToken = a} :: ListDetectors)

-- | You can use this parameter to indicate the maximum number of items that
-- you want in the response. The default value is 50. The maximum value is
-- 50.
listDetectors_maxResults :: Lens.Lens' ListDetectors (Prelude.Maybe Prelude.Natural)
listDetectors_maxResults = Lens.lens (\ListDetectors' {maxResults} -> maxResults) (\s@ListDetectors' {} a -> s {maxResults = a} :: ListDetectors)

instance Pager.AWSPager ListDetectors where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listDetectorsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        (rs Lens.^. listDetectorsResponse_detectorIds) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listDetectors_nextToken
          Lens..~ rs
          Lens.^? listDetectorsResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListDetectors where
  type Rs ListDetectors = ListDetectorsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDetectorsResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..?> "detectorIds"
                            Prelude..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListDetectors

instance Prelude.NFData ListDetectors

instance Prelude.ToHeaders ListDetectors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath ListDetectors where
  toPath = Prelude.const "/detector"

instance Prelude.ToQuery ListDetectors where
  toQuery ListDetectors' {..} =
    Prelude.mconcat
      [ "nextToken" Prelude.=: nextToken,
        "maxResults" Prelude.=: maxResults
      ]

-- | /See:/ 'newListDetectorsResponse' smart constructor.
data ListDetectorsResponse = ListDetectorsResponse'
  { -- | The pagination parameter to be used on the next list operation to
    -- retrieve more items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of detector IDs.
    detectorIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListDetectorsResponse
newListDetectorsResponse pHttpStatus_ =
  ListDetectorsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      detectorIds = Prelude.mempty
    }

-- | The pagination parameter to be used on the next list operation to
-- retrieve more items.
listDetectorsResponse_nextToken :: Lens.Lens' ListDetectorsResponse (Prelude.Maybe Prelude.Text)
listDetectorsResponse_nextToken = Lens.lens (\ListDetectorsResponse' {nextToken} -> nextToken) (\s@ListDetectorsResponse' {} a -> s {nextToken = a} :: ListDetectorsResponse)

-- | The response's http status code.
listDetectorsResponse_httpStatus :: Lens.Lens' ListDetectorsResponse Prelude.Int
listDetectorsResponse_httpStatus = Lens.lens (\ListDetectorsResponse' {httpStatus} -> httpStatus) (\s@ListDetectorsResponse' {} a -> s {httpStatus = a} :: ListDetectorsResponse)

-- | A list of detector IDs.
listDetectorsResponse_detectorIds :: Lens.Lens' ListDetectorsResponse [Prelude.Text]
listDetectorsResponse_detectorIds = Lens.lens (\ListDetectorsResponse' {detectorIds} -> detectorIds) (\s@ListDetectorsResponse' {} a -> s {detectorIds = a} :: ListDetectorsResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData ListDetectorsResponse
