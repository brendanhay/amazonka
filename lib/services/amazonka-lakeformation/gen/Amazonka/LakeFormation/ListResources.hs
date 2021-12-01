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
-- Module      : Amazonka.LakeFormation.ListResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resources registered to be managed by the Data Catalog.
module Amazonka.LakeFormation.ListResources
  ( -- * Creating a Request
    ListResources (..),
    newListResources,

    -- * Request Lenses
    listResources_filterConditionList,
    listResources_nextToken,
    listResources_maxResults,

    -- * Destructuring the Response
    ListResourcesResponse (..),
    newListResourcesResponse,

    -- * Response Lenses
    listResourcesResponse_resourceInfoList,
    listResourcesResponse_nextToken,
    listResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.LakeFormation.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListResources' smart constructor.
data ListResources = ListResources'
  { -- | Any applicable row-level and\/or column-level filtering conditions for
    -- the resources.
    filterConditionList :: Prelude.Maybe (Prelude.NonEmpty FilterCondition),
    -- | A continuation token, if this is not the first call to retrieve these
    -- resources.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of resource results.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterConditionList', 'listResources_filterConditionList' - Any applicable row-level and\/or column-level filtering conditions for
-- the resources.
--
-- 'nextToken', 'listResources_nextToken' - A continuation token, if this is not the first call to retrieve these
-- resources.
--
-- 'maxResults', 'listResources_maxResults' - The maximum number of resource results.
newListResources ::
  ListResources
newListResources =
  ListResources'
    { filterConditionList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Any applicable row-level and\/or column-level filtering conditions for
-- the resources.
listResources_filterConditionList :: Lens.Lens' ListResources (Prelude.Maybe (Prelude.NonEmpty FilterCondition))
listResources_filterConditionList = Lens.lens (\ListResources' {filterConditionList} -> filterConditionList) (\s@ListResources' {} a -> s {filterConditionList = a} :: ListResources) Prelude.. Lens.mapping Lens.coerced

-- | A continuation token, if this is not the first call to retrieve these
-- resources.
listResources_nextToken :: Lens.Lens' ListResources (Prelude.Maybe Prelude.Text)
listResources_nextToken = Lens.lens (\ListResources' {nextToken} -> nextToken) (\s@ListResources' {} a -> s {nextToken = a} :: ListResources)

-- | The maximum number of resource results.
listResources_maxResults :: Lens.Lens' ListResources (Prelude.Maybe Prelude.Natural)
listResources_maxResults = Lens.lens (\ListResources' {maxResults} -> maxResults) (\s@ListResources' {} a -> s {maxResults = a} :: ListResources)

instance Core.AWSRequest ListResources where
  type
    AWSResponse ListResources =
      ListResourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourcesResponse'
            Prelude.<$> ( x Core..?> "ResourceInfoList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResources where
  hashWithSalt salt' ListResources' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filterConditionList

instance Prelude.NFData ListResources where
  rnf ListResources' {..} =
    Prelude.rnf filterConditionList
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Core.ToHeaders ListResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLakeFormation.ListResources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListResources where
  toJSON ListResources' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FilterConditionList" Core..=)
              Prelude.<$> filterConditionList,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListResources where
  toPath = Prelude.const "/"

instance Core.ToQuery ListResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourcesResponse' smart constructor.
data ListResourcesResponse = ListResourcesResponse'
  { -- | A summary of the data lake resources.
    resourceInfoList :: Prelude.Maybe [ResourceInfo],
    -- | A continuation token, if this is not the first call to retrieve these
    -- resources.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceInfoList', 'listResourcesResponse_resourceInfoList' - A summary of the data lake resources.
--
-- 'nextToken', 'listResourcesResponse_nextToken' - A continuation token, if this is not the first call to retrieve these
-- resources.
--
-- 'httpStatus', 'listResourcesResponse_httpStatus' - The response's http status code.
newListResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourcesResponse
newListResourcesResponse pHttpStatus_ =
  ListResourcesResponse'
    { resourceInfoList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A summary of the data lake resources.
listResourcesResponse_resourceInfoList :: Lens.Lens' ListResourcesResponse (Prelude.Maybe [ResourceInfo])
listResourcesResponse_resourceInfoList = Lens.lens (\ListResourcesResponse' {resourceInfoList} -> resourceInfoList) (\s@ListResourcesResponse' {} a -> s {resourceInfoList = a} :: ListResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A continuation token, if this is not the first call to retrieve these
-- resources.
listResourcesResponse_nextToken :: Lens.Lens' ListResourcesResponse (Prelude.Maybe Prelude.Text)
listResourcesResponse_nextToken = Lens.lens (\ListResourcesResponse' {nextToken} -> nextToken) (\s@ListResourcesResponse' {} a -> s {nextToken = a} :: ListResourcesResponse)

-- | The response's http status code.
listResourcesResponse_httpStatus :: Lens.Lens' ListResourcesResponse Prelude.Int
listResourcesResponse_httpStatus = Lens.lens (\ListResourcesResponse' {httpStatus} -> httpStatus) (\s@ListResourcesResponse' {} a -> s {httpStatus = a} :: ListResourcesResponse)

instance Prelude.NFData ListResourcesResponse where
  rnf ListResourcesResponse' {..} =
    Prelude.rnf resourceInfoList
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
