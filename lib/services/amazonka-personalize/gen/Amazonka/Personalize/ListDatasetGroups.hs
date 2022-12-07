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
-- Module      : Amazonka.Personalize.ListDatasetGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of dataset groups. The response provides the properties
-- for each dataset group, including the Amazon Resource Name (ARN). For
-- more information on dataset groups, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateDatasetGroup.html CreateDatasetGroup>.
--
-- This operation returns paginated results.
module Amazonka.Personalize.ListDatasetGroups
  ( -- * Creating a Request
    ListDatasetGroups (..),
    newListDatasetGroups,

    -- * Request Lenses
    listDatasetGroups_nextToken,
    listDatasetGroups_maxResults,

    -- * Destructuring the Response
    ListDatasetGroupsResponse (..),
    newListDatasetGroupsResponse,

    -- * Response Lenses
    listDatasetGroupsResponse_nextToken,
    listDatasetGroupsResponse_datasetGroups,
    listDatasetGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDatasetGroups' smart constructor.
data ListDatasetGroups = ListDatasetGroups'
  { -- | A token returned from the previous call to @ListDatasetGroups@ for
    -- getting the next set of dataset groups (if they exist).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of dataset groups to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatasetGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatasetGroups_nextToken' - A token returned from the previous call to @ListDatasetGroups@ for
-- getting the next set of dataset groups (if they exist).
--
-- 'maxResults', 'listDatasetGroups_maxResults' - The maximum number of dataset groups to return.
newListDatasetGroups ::
  ListDatasetGroups
newListDatasetGroups =
  ListDatasetGroups'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token returned from the previous call to @ListDatasetGroups@ for
-- getting the next set of dataset groups (if they exist).
listDatasetGroups_nextToken :: Lens.Lens' ListDatasetGroups (Prelude.Maybe Prelude.Text)
listDatasetGroups_nextToken = Lens.lens (\ListDatasetGroups' {nextToken} -> nextToken) (\s@ListDatasetGroups' {} a -> s {nextToken = a} :: ListDatasetGroups)

-- | The maximum number of dataset groups to return.
listDatasetGroups_maxResults :: Lens.Lens' ListDatasetGroups (Prelude.Maybe Prelude.Natural)
listDatasetGroups_maxResults = Lens.lens (\ListDatasetGroups' {maxResults} -> maxResults) (\s@ListDatasetGroups' {} a -> s {maxResults = a} :: ListDatasetGroups)

instance Core.AWSPager ListDatasetGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDatasetGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDatasetGroupsResponse_datasetGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDatasetGroups_nextToken
          Lens..~ rs
          Lens.^? listDatasetGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDatasetGroups where
  type
    AWSResponse ListDatasetGroups =
      ListDatasetGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatasetGroupsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "datasetGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDatasetGroups where
  hashWithSalt _salt ListDatasetGroups' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListDatasetGroups where
  rnf ListDatasetGroups' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListDatasetGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.ListDatasetGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDatasetGroups where
  toJSON ListDatasetGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("maxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListDatasetGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDatasetGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDatasetGroupsResponse' smart constructor.
data ListDatasetGroupsResponse = ListDatasetGroupsResponse'
  { -- | A token for getting the next set of dataset groups (if they exist).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of your dataset groups.
    datasetGroups :: Prelude.Maybe [DatasetGroupSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatasetGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatasetGroupsResponse_nextToken' - A token for getting the next set of dataset groups (if they exist).
--
-- 'datasetGroups', 'listDatasetGroupsResponse_datasetGroups' - The list of your dataset groups.
--
-- 'httpStatus', 'listDatasetGroupsResponse_httpStatus' - The response's http status code.
newListDatasetGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDatasetGroupsResponse
newListDatasetGroupsResponse pHttpStatus_ =
  ListDatasetGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      datasetGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of dataset groups (if they exist).
listDatasetGroupsResponse_nextToken :: Lens.Lens' ListDatasetGroupsResponse (Prelude.Maybe Prelude.Text)
listDatasetGroupsResponse_nextToken = Lens.lens (\ListDatasetGroupsResponse' {nextToken} -> nextToken) (\s@ListDatasetGroupsResponse' {} a -> s {nextToken = a} :: ListDatasetGroupsResponse)

-- | The list of your dataset groups.
listDatasetGroupsResponse_datasetGroups :: Lens.Lens' ListDatasetGroupsResponse (Prelude.Maybe [DatasetGroupSummary])
listDatasetGroupsResponse_datasetGroups = Lens.lens (\ListDatasetGroupsResponse' {datasetGroups} -> datasetGroups) (\s@ListDatasetGroupsResponse' {} a -> s {datasetGroups = a} :: ListDatasetGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDatasetGroupsResponse_httpStatus :: Lens.Lens' ListDatasetGroupsResponse Prelude.Int
listDatasetGroupsResponse_httpStatus = Lens.lens (\ListDatasetGroupsResponse' {httpStatus} -> httpStatus) (\s@ListDatasetGroupsResponse' {} a -> s {httpStatus = a} :: ListDatasetGroupsResponse)

instance Prelude.NFData ListDatasetGroupsResponse where
  rnf ListDatasetGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf datasetGroups
      `Prelude.seq` Prelude.rnf httpStatus
