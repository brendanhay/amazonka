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
-- Module      : Network.AWS.MechanicalTurk.ListHITs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListHITs@ operation returns all of a Requester\'s HITs. The
-- operation returns HITs of any status, except for HITs that have been
-- deleted of with the DeleteHIT operation or that have been auto-deleted.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListHITs
  ( -- * Creating a Request
    ListHITs (..),
    newListHITs,

    -- * Request Lenses
    listHITs_nextToken,
    listHITs_maxResults,

    -- * Destructuring the Response
    ListHITsResponse (..),
    newListHITsResponse,

    -- * Response Lenses
    listHITsResponse_nextToken,
    listHITsResponse_hITs,
    listHITsResponse_numResults,
    listHITsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListHITs' smart constructor.
data ListHITs = ListHITs'
  { -- | Pagination token
    nextToken :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListHITs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHITs_nextToken' - Pagination token
--
-- 'maxResults', 'listHITs_maxResults' - Undocumented member.
newListHITs ::
  ListHITs
newListHITs =
  ListHITs'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Pagination token
listHITs_nextToken :: Lens.Lens' ListHITs (Prelude.Maybe Prelude.Text)
listHITs_nextToken = Lens.lens (\ListHITs' {nextToken} -> nextToken) (\s@ListHITs' {} a -> s {nextToken = a} :: ListHITs)

-- | Undocumented member.
listHITs_maxResults :: Lens.Lens' ListHITs (Prelude.Maybe Prelude.Natural)
listHITs_maxResults = Lens.lens (\ListHITs' {maxResults} -> maxResults) (\s@ListHITs' {} a -> s {maxResults = a} :: ListHITs)

instance Pager.AWSPager ListHITs where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listHITsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listHITsResponse_hITs Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listHITs_nextToken
          Lens..~ rs
          Lens.^? listHITsResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListHITs where
  type Rs ListHITs = ListHITsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHITsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (x Prelude..?> "HITs" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "NumResults")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListHITs

instance Prelude.NFData ListHITs

instance Prelude.ToHeaders ListHITs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "MTurkRequesterServiceV20170117.ListHITs" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListHITs where
  toJSON ListHITs' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults
          ]
      )

instance Prelude.ToPath ListHITs where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListHITs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListHITsResponse' smart constructor.
data ListHITsResponse = ListHITsResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of HIT elements returned by the query.
    hITs :: Prelude.Maybe [HIT],
    -- | The number of HITs on this page in the filtered results list, equivalent
    -- to the number of HITs being returned by this call.
    numResults :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListHITsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHITsResponse_nextToken' - Undocumented member.
--
-- 'hITs', 'listHITsResponse_hITs' - The list of HIT elements returned by the query.
--
-- 'numResults', 'listHITsResponse_numResults' - The number of HITs on this page in the filtered results list, equivalent
-- to the number of HITs being returned by this call.
--
-- 'httpStatus', 'listHITsResponse_httpStatus' - The response's http status code.
newListHITsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListHITsResponse
newListHITsResponse pHttpStatus_ =
  ListHITsResponse'
    { nextToken = Prelude.Nothing,
      hITs = Prelude.Nothing,
      numResults = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listHITsResponse_nextToken :: Lens.Lens' ListHITsResponse (Prelude.Maybe Prelude.Text)
listHITsResponse_nextToken = Lens.lens (\ListHITsResponse' {nextToken} -> nextToken) (\s@ListHITsResponse' {} a -> s {nextToken = a} :: ListHITsResponse)

-- | The list of HIT elements returned by the query.
listHITsResponse_hITs :: Lens.Lens' ListHITsResponse (Prelude.Maybe [HIT])
listHITsResponse_hITs = Lens.lens (\ListHITsResponse' {hITs} -> hITs) (\s@ListHITsResponse' {} a -> s {hITs = a} :: ListHITsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of HITs on this page in the filtered results list, equivalent
-- to the number of HITs being returned by this call.
listHITsResponse_numResults :: Lens.Lens' ListHITsResponse (Prelude.Maybe Prelude.Int)
listHITsResponse_numResults = Lens.lens (\ListHITsResponse' {numResults} -> numResults) (\s@ListHITsResponse' {} a -> s {numResults = a} :: ListHITsResponse)

-- | The response's http status code.
listHITsResponse_httpStatus :: Lens.Lens' ListHITsResponse Prelude.Int
listHITsResponse_httpStatus = Lens.lens (\ListHITsResponse' {httpStatus} -> httpStatus) (\s@ListHITsResponse' {} a -> s {httpStatus = a} :: ListHITsResponse)

instance Prelude.NFData ListHITsResponse
