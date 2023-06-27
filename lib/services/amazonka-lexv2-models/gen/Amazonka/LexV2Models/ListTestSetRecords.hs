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
-- Module      : Amazonka.LexV2Models.ListTestSetRecords
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The list of test set records.
module Amazonka.LexV2Models.ListTestSetRecords
  ( -- * Creating a Request
    ListTestSetRecords (..),
    newListTestSetRecords,

    -- * Request Lenses
    listTestSetRecords_maxResults,
    listTestSetRecords_nextToken,
    listTestSetRecords_testSetId,

    -- * Destructuring the Response
    ListTestSetRecordsResponse (..),
    newListTestSetRecordsResponse,

    -- * Response Lenses
    listTestSetRecordsResponse_nextToken,
    listTestSetRecordsResponse_testSetRecords,
    listTestSetRecordsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTestSetRecords' smart constructor.
data ListTestSetRecords = ListTestSetRecords'
  { -- | The maximum number of test set records to return in each page. If there
    -- are fewer records than the max page size, only the actual number of
    -- records are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the response from the ListTestSetRecords operation contains more
    -- results than specified in the maxResults parameter, a token is returned
    -- in the response. Use that token in the nextToken parameter to return the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the test set to list its test set records.
    testSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTestSetRecords' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTestSetRecords_maxResults' - The maximum number of test set records to return in each page. If there
-- are fewer records than the max page size, only the actual number of
-- records are returned.
--
-- 'nextToken', 'listTestSetRecords_nextToken' - If the response from the ListTestSetRecords operation contains more
-- results than specified in the maxResults parameter, a token is returned
-- in the response. Use that token in the nextToken parameter to return the
-- next page of results.
--
-- 'testSetId', 'listTestSetRecords_testSetId' - The identifier of the test set to list its test set records.
newListTestSetRecords ::
  -- | 'testSetId'
  Prelude.Text ->
  ListTestSetRecords
newListTestSetRecords pTestSetId_ =
  ListTestSetRecords'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      testSetId = pTestSetId_
    }

-- | The maximum number of test set records to return in each page. If there
-- are fewer records than the max page size, only the actual number of
-- records are returned.
listTestSetRecords_maxResults :: Lens.Lens' ListTestSetRecords (Prelude.Maybe Prelude.Natural)
listTestSetRecords_maxResults = Lens.lens (\ListTestSetRecords' {maxResults} -> maxResults) (\s@ListTestSetRecords' {} a -> s {maxResults = a} :: ListTestSetRecords)

-- | If the response from the ListTestSetRecords operation contains more
-- results than specified in the maxResults parameter, a token is returned
-- in the response. Use that token in the nextToken parameter to return the
-- next page of results.
listTestSetRecords_nextToken :: Lens.Lens' ListTestSetRecords (Prelude.Maybe Prelude.Text)
listTestSetRecords_nextToken = Lens.lens (\ListTestSetRecords' {nextToken} -> nextToken) (\s@ListTestSetRecords' {} a -> s {nextToken = a} :: ListTestSetRecords)

-- | The identifier of the test set to list its test set records.
listTestSetRecords_testSetId :: Lens.Lens' ListTestSetRecords Prelude.Text
listTestSetRecords_testSetId = Lens.lens (\ListTestSetRecords' {testSetId} -> testSetId) (\s@ListTestSetRecords' {} a -> s {testSetId = a} :: ListTestSetRecords)

instance Core.AWSRequest ListTestSetRecords where
  type
    AWSResponse ListTestSetRecords =
      ListTestSetRecordsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTestSetRecordsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "testSetRecords" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTestSetRecords where
  hashWithSalt _salt ListTestSetRecords' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` testSetId

instance Prelude.NFData ListTestSetRecords where
  rnf ListTestSetRecords' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf testSetId

instance Data.ToHeaders ListTestSetRecords where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTestSetRecords where
  toJSON ListTestSetRecords' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListTestSetRecords where
  toPath ListTestSetRecords' {..} =
    Prelude.mconcat
      ["/testsets/", Data.toBS testSetId, "/records"]

instance Data.ToQuery ListTestSetRecords where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTestSetRecordsResponse' smart constructor.
data ListTestSetRecordsResponse = ListTestSetRecordsResponse'
  { -- | A token that indicates whether there are more records to return in a
    -- response to the ListTestSetRecords operation. If the nextToken field is
    -- present, you send the contents as the nextToken parameter of a
    -- ListTestSetRecords operation request to get the next page of records.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of records from the test set.
    testSetRecords :: Prelude.Maybe [TestSetTurnRecord],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTestSetRecordsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTestSetRecordsResponse_nextToken' - A token that indicates whether there are more records to return in a
-- response to the ListTestSetRecords operation. If the nextToken field is
-- present, you send the contents as the nextToken parameter of a
-- ListTestSetRecords operation request to get the next page of records.
--
-- 'testSetRecords', 'listTestSetRecordsResponse_testSetRecords' - The list of records from the test set.
--
-- 'httpStatus', 'listTestSetRecordsResponse_httpStatus' - The response's http status code.
newListTestSetRecordsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTestSetRecordsResponse
newListTestSetRecordsResponse pHttpStatus_ =
  ListTestSetRecordsResponse'
    { nextToken =
        Prelude.Nothing,
      testSetRecords = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates whether there are more records to return in a
-- response to the ListTestSetRecords operation. If the nextToken field is
-- present, you send the contents as the nextToken parameter of a
-- ListTestSetRecords operation request to get the next page of records.
listTestSetRecordsResponse_nextToken :: Lens.Lens' ListTestSetRecordsResponse (Prelude.Maybe Prelude.Text)
listTestSetRecordsResponse_nextToken = Lens.lens (\ListTestSetRecordsResponse' {nextToken} -> nextToken) (\s@ListTestSetRecordsResponse' {} a -> s {nextToken = a} :: ListTestSetRecordsResponse)

-- | The list of records from the test set.
listTestSetRecordsResponse_testSetRecords :: Lens.Lens' ListTestSetRecordsResponse (Prelude.Maybe [TestSetTurnRecord])
listTestSetRecordsResponse_testSetRecords = Lens.lens (\ListTestSetRecordsResponse' {testSetRecords} -> testSetRecords) (\s@ListTestSetRecordsResponse' {} a -> s {testSetRecords = a} :: ListTestSetRecordsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTestSetRecordsResponse_httpStatus :: Lens.Lens' ListTestSetRecordsResponse Prelude.Int
listTestSetRecordsResponse_httpStatus = Lens.lens (\ListTestSetRecordsResponse' {httpStatus} -> httpStatus) (\s@ListTestSetRecordsResponse' {} a -> s {httpStatus = a} :: ListTestSetRecordsResponse)

instance Prelude.NFData ListTestSetRecordsResponse where
  rnf ListTestSetRecordsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf testSetRecords
      `Prelude.seq` Prelude.rnf httpStatus
