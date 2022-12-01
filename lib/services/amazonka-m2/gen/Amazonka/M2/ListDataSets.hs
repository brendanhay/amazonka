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
-- Module      : Amazonka.M2.ListDataSets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the data sets imported for a specific application. In Amazon Web
-- Services Mainframe Modernization, data sets are associated with
-- applications deployed on environments. This is known as importing data
-- sets. Currently, Amazon Web Services Mainframe Modernization can import
-- data sets into catalogs using
-- <https://docs.aws.amazon.com/m2/latest/APIReference/API_CreateDataSetImportTask.html CreateDataSetImportTask>.
--
-- This operation returns paginated results.
module Amazonka.M2.ListDataSets
  ( -- * Creating a Request
    ListDataSets (..),
    newListDataSets,

    -- * Request Lenses
    listDataSets_nextToken,
    listDataSets_maxResults,
    listDataSets_prefix,
    listDataSets_applicationId,

    -- * Destructuring the Response
    ListDataSetsResponse (..),
    newListDataSetsResponse,

    -- * Response Lenses
    listDataSetsResponse_nextToken,
    listDataSetsResponse_httpStatus,
    listDataSetsResponse_dataSets,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDataSets' smart constructor.
data ListDataSets = ListDataSets'
  { -- | A pagination token returned from a previous call to this operation. This
    -- specifies the next item to return. To return to the beginning of the
    -- list, exclude this parameter.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The prefix of the data set name, which you can use to filter the list of
    -- data sets.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the application for which you want to list the
    -- associated data sets.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDataSets_nextToken' - A pagination token returned from a previous call to this operation. This
-- specifies the next item to return. To return to the beginning of the
-- list, exclude this parameter.
--
-- 'maxResults', 'listDataSets_maxResults' - The maximum number of objects to return.
--
-- 'prefix', 'listDataSets_prefix' - The prefix of the data set name, which you can use to filter the list of
-- data sets.
--
-- 'applicationId', 'listDataSets_applicationId' - The unique identifier of the application for which you want to list the
-- associated data sets.
newListDataSets ::
  -- | 'applicationId'
  Prelude.Text ->
  ListDataSets
newListDataSets pApplicationId_ =
  ListDataSets'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      prefix = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | A pagination token returned from a previous call to this operation. This
-- specifies the next item to return. To return to the beginning of the
-- list, exclude this parameter.
listDataSets_nextToken :: Lens.Lens' ListDataSets (Prelude.Maybe Prelude.Text)
listDataSets_nextToken = Lens.lens (\ListDataSets' {nextToken} -> nextToken) (\s@ListDataSets' {} a -> s {nextToken = a} :: ListDataSets)

-- | The maximum number of objects to return.
listDataSets_maxResults :: Lens.Lens' ListDataSets (Prelude.Maybe Prelude.Natural)
listDataSets_maxResults = Lens.lens (\ListDataSets' {maxResults} -> maxResults) (\s@ListDataSets' {} a -> s {maxResults = a} :: ListDataSets)

-- | The prefix of the data set name, which you can use to filter the list of
-- data sets.
listDataSets_prefix :: Lens.Lens' ListDataSets (Prelude.Maybe Prelude.Text)
listDataSets_prefix = Lens.lens (\ListDataSets' {prefix} -> prefix) (\s@ListDataSets' {} a -> s {prefix = a} :: ListDataSets)

-- | The unique identifier of the application for which you want to list the
-- associated data sets.
listDataSets_applicationId :: Lens.Lens' ListDataSets Prelude.Text
listDataSets_applicationId = Lens.lens (\ListDataSets' {applicationId} -> applicationId) (\s@ListDataSets' {} a -> s {applicationId = a} :: ListDataSets)

instance Core.AWSPager ListDataSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDataSetsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listDataSetsResponse_dataSets) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDataSets_nextToken
          Lens..~ rs
          Lens.^? listDataSetsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListDataSets where
  type AWSResponse ListDataSets = ListDataSetsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataSetsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "dataSets" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListDataSets where
  hashWithSalt _salt ListDataSets' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData ListDataSets where
  rnf ListDataSets' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf applicationId

instance Core.ToHeaders ListDataSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListDataSets where
  toPath ListDataSets' {..} =
    Prelude.mconcat
      [ "/applications/",
        Core.toBS applicationId,
        "/datasets"
      ]

instance Core.ToQuery ListDataSets where
  toQuery ListDataSets' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "prefix" Core.=: prefix
      ]

-- | /See:/ 'newListDataSetsResponse' smart constructor.
data ListDataSetsResponse = ListDataSetsResponse'
  { -- | If there are more items to return, this contains a token that is passed
    -- to a subsequent call to this operation to retrieve the next set of
    -- items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of data sets, containing ionformation including the creating
    -- time, the data set name, the data set organization, the data set format,
    -- and the last time the data set was referenced or updated.
    dataSets :: [DataSetSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDataSetsResponse_nextToken' - If there are more items to return, this contains a token that is passed
-- to a subsequent call to this operation to retrieve the next set of
-- items.
--
-- 'httpStatus', 'listDataSetsResponse_httpStatus' - The response's http status code.
--
-- 'dataSets', 'listDataSetsResponse_dataSets' - The list of data sets, containing ionformation including the creating
-- time, the data set name, the data set organization, the data set format,
-- and the last time the data set was referenced or updated.
newListDataSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDataSetsResponse
newListDataSetsResponse pHttpStatus_ =
  ListDataSetsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      dataSets = Prelude.mempty
    }

-- | If there are more items to return, this contains a token that is passed
-- to a subsequent call to this operation to retrieve the next set of
-- items.
listDataSetsResponse_nextToken :: Lens.Lens' ListDataSetsResponse (Prelude.Maybe Prelude.Text)
listDataSetsResponse_nextToken = Lens.lens (\ListDataSetsResponse' {nextToken} -> nextToken) (\s@ListDataSetsResponse' {} a -> s {nextToken = a} :: ListDataSetsResponse)

-- | The response's http status code.
listDataSetsResponse_httpStatus :: Lens.Lens' ListDataSetsResponse Prelude.Int
listDataSetsResponse_httpStatus = Lens.lens (\ListDataSetsResponse' {httpStatus} -> httpStatus) (\s@ListDataSetsResponse' {} a -> s {httpStatus = a} :: ListDataSetsResponse)

-- | The list of data sets, containing ionformation including the creating
-- time, the data set name, the data set organization, the data set format,
-- and the last time the data set was referenced or updated.
listDataSetsResponse_dataSets :: Lens.Lens' ListDataSetsResponse [DataSetSummary]
listDataSetsResponse_dataSets = Lens.lens (\ListDataSetsResponse' {dataSets} -> dataSets) (\s@ListDataSetsResponse' {} a -> s {dataSets = a} :: ListDataSetsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListDataSetsResponse where
  rnf ListDataSetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf dataSets
