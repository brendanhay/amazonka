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
-- Module      : Amazonka.MechanicalTurk.ListHITs
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.MechanicalTurk.ListHITs
  ( -- * Creating a Request
    ListHITs (..),
    newListHITs,

    -- * Request Lenses
    listHITs_maxResults,
    listHITs_nextToken,

    -- * Destructuring the Response
    ListHITsResponse (..),
    newListHITsResponse,

    -- * Response Lenses
    listHITsResponse_hITs,
    listHITsResponse_nextToken,
    listHITsResponse_numResults,
    listHITsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListHITs' smart constructor.
data ListHITs = ListHITs'
  { maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Pagination token
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHITs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listHITs_maxResults' - Undocumented member.
--
-- 'nextToken', 'listHITs_nextToken' - Pagination token
newListHITs ::
  ListHITs
newListHITs =
  ListHITs'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Undocumented member.
listHITs_maxResults :: Lens.Lens' ListHITs (Prelude.Maybe Prelude.Natural)
listHITs_maxResults = Lens.lens (\ListHITs' {maxResults} -> maxResults) (\s@ListHITs' {} a -> s {maxResults = a} :: ListHITs)

-- | Pagination token
listHITs_nextToken :: Lens.Lens' ListHITs (Prelude.Maybe Prelude.Text)
listHITs_nextToken = Lens.lens (\ListHITs' {nextToken} -> nextToken) (\s@ListHITs' {} a -> s {nextToken = a} :: ListHITs)

instance Core.AWSPager ListHITs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listHITsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listHITsResponse_hITs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listHITs_nextToken
          Lens..~ rs
          Lens.^? listHITsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListHITs where
  type AWSResponse ListHITs = ListHITsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHITsResponse'
            Prelude.<$> (x Data..?> "HITs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "NumResults")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListHITs where
  hashWithSalt _salt ListHITs' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListHITs where
  rnf ListHITs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListHITs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.ListHITs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListHITs where
  toJSON ListHITs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListHITs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListHITs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListHITsResponse' smart constructor.
data ListHITsResponse = ListHITsResponse'
  { -- | The list of HIT elements returned by the query.
    hITs :: Prelude.Maybe [HIT],
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of HITs on this page in the filtered results list, equivalent
    -- to the number of HITs being returned by this call.
    numResults :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHITsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hITs', 'listHITsResponse_hITs' - The list of HIT elements returned by the query.
--
-- 'nextToken', 'listHITsResponse_nextToken' - Undocumented member.
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
    { hITs = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      numResults = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of HIT elements returned by the query.
listHITsResponse_hITs :: Lens.Lens' ListHITsResponse (Prelude.Maybe [HIT])
listHITsResponse_hITs = Lens.lens (\ListHITsResponse' {hITs} -> hITs) (\s@ListHITsResponse' {} a -> s {hITs = a} :: ListHITsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
listHITsResponse_nextToken :: Lens.Lens' ListHITsResponse (Prelude.Maybe Prelude.Text)
listHITsResponse_nextToken = Lens.lens (\ListHITsResponse' {nextToken} -> nextToken) (\s@ListHITsResponse' {} a -> s {nextToken = a} :: ListHITsResponse)

-- | The number of HITs on this page in the filtered results list, equivalent
-- to the number of HITs being returned by this call.
listHITsResponse_numResults :: Lens.Lens' ListHITsResponse (Prelude.Maybe Prelude.Int)
listHITsResponse_numResults = Lens.lens (\ListHITsResponse' {numResults} -> numResults) (\s@ListHITsResponse' {} a -> s {numResults = a} :: ListHITsResponse)

-- | The response's http status code.
listHITsResponse_httpStatus :: Lens.Lens' ListHITsResponse Prelude.Int
listHITsResponse_httpStatus = Lens.lens (\ListHITsResponse' {httpStatus} -> httpStatus) (\s@ListHITsResponse' {} a -> s {httpStatus = a} :: ListHITsResponse)

instance Prelude.NFData ListHITsResponse where
  rnf ListHITsResponse' {..} =
    Prelude.rnf hITs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf numResults
      `Prelude.seq` Prelude.rnf httpStatus
