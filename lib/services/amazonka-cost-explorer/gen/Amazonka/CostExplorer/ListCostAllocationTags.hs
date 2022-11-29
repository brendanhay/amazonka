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
-- Module      : Amazonka.CostExplorer.ListCostAllocationTags
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a list of cost allocation tags. All inputs in the API are optional
-- and serve as filters. By default, all cost allocation tags are returned.
module Amazonka.CostExplorer.ListCostAllocationTags
  ( -- * Creating a Request
    ListCostAllocationTags (..),
    newListCostAllocationTags,

    -- * Request Lenses
    listCostAllocationTags_tagKeys,
    listCostAllocationTags_nextToken,
    listCostAllocationTags_type,
    listCostAllocationTags_status,
    listCostAllocationTags_maxResults,

    -- * Destructuring the Response
    ListCostAllocationTagsResponse (..),
    newListCostAllocationTagsResponse,

    -- * Response Lenses
    listCostAllocationTagsResponse_nextToken,
    listCostAllocationTagsResponse_costAllocationTags,
    listCostAllocationTagsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCostAllocationTags' smart constructor.
data ListCostAllocationTags = ListCostAllocationTags'
  { -- | The list of cost allocation tag keys that are returned for this request.
    tagKeys :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of @CostAllocationTag@ object that are returned for this
    -- request. The @AWSGenerated@ type tags are tags that Amazon Web Services
    -- defines and applies to support Amazon Web Services resources for cost
    -- allocation purposes. The @UserDefined@ type tags are tags that you
    -- define, create, and apply to resources.
    type' :: Prelude.Maybe CostAllocationTagType,
    -- | The status of cost allocation tag keys that are returned for this
    -- request.
    status :: Prelude.Maybe CostAllocationTagStatus,
    -- | The maximum number of objects that are returned for this request. By
    -- default, the request returns 100 results.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCostAllocationTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagKeys', 'listCostAllocationTags_tagKeys' - The list of cost allocation tag keys that are returned for this request.
--
-- 'nextToken', 'listCostAllocationTags_nextToken' - The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'type'', 'listCostAllocationTags_type' - The type of @CostAllocationTag@ object that are returned for this
-- request. The @AWSGenerated@ type tags are tags that Amazon Web Services
-- defines and applies to support Amazon Web Services resources for cost
-- allocation purposes. The @UserDefined@ type tags are tags that you
-- define, create, and apply to resources.
--
-- 'status', 'listCostAllocationTags_status' - The status of cost allocation tag keys that are returned for this
-- request.
--
-- 'maxResults', 'listCostAllocationTags_maxResults' - The maximum number of objects that are returned for this request. By
-- default, the request returns 100 results.
newListCostAllocationTags ::
  ListCostAllocationTags
newListCostAllocationTags =
  ListCostAllocationTags'
    { tagKeys = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      type' = Prelude.Nothing,
      status = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The list of cost allocation tag keys that are returned for this request.
listCostAllocationTags_tagKeys :: Lens.Lens' ListCostAllocationTags (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listCostAllocationTags_tagKeys = Lens.lens (\ListCostAllocationTags' {tagKeys} -> tagKeys) (\s@ListCostAllocationTags' {} a -> s {tagKeys = a} :: ListCostAllocationTags) Prelude.. Lens.mapping Lens.coerced

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
listCostAllocationTags_nextToken :: Lens.Lens' ListCostAllocationTags (Prelude.Maybe Prelude.Text)
listCostAllocationTags_nextToken = Lens.lens (\ListCostAllocationTags' {nextToken} -> nextToken) (\s@ListCostAllocationTags' {} a -> s {nextToken = a} :: ListCostAllocationTags)

-- | The type of @CostAllocationTag@ object that are returned for this
-- request. The @AWSGenerated@ type tags are tags that Amazon Web Services
-- defines and applies to support Amazon Web Services resources for cost
-- allocation purposes. The @UserDefined@ type tags are tags that you
-- define, create, and apply to resources.
listCostAllocationTags_type :: Lens.Lens' ListCostAllocationTags (Prelude.Maybe CostAllocationTagType)
listCostAllocationTags_type = Lens.lens (\ListCostAllocationTags' {type'} -> type') (\s@ListCostAllocationTags' {} a -> s {type' = a} :: ListCostAllocationTags)

-- | The status of cost allocation tag keys that are returned for this
-- request.
listCostAllocationTags_status :: Lens.Lens' ListCostAllocationTags (Prelude.Maybe CostAllocationTagStatus)
listCostAllocationTags_status = Lens.lens (\ListCostAllocationTags' {status} -> status) (\s@ListCostAllocationTags' {} a -> s {status = a} :: ListCostAllocationTags)

-- | The maximum number of objects that are returned for this request. By
-- default, the request returns 100 results.
listCostAllocationTags_maxResults :: Lens.Lens' ListCostAllocationTags (Prelude.Maybe Prelude.Natural)
listCostAllocationTags_maxResults = Lens.lens (\ListCostAllocationTags' {maxResults} -> maxResults) (\s@ListCostAllocationTags' {} a -> s {maxResults = a} :: ListCostAllocationTags)

instance Core.AWSRequest ListCostAllocationTags where
  type
    AWSResponse ListCostAllocationTags =
      ListCostAllocationTagsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCostAllocationTagsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "CostAllocationTags"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCostAllocationTags where
  hashWithSalt _salt ListCostAllocationTags' {..} =
    _salt `Prelude.hashWithSalt` tagKeys
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListCostAllocationTags where
  rnf ListCostAllocationTags' {..} =
    Prelude.rnf tagKeys
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListCostAllocationTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.ListCostAllocationTags" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListCostAllocationTags where
  toJSON ListCostAllocationTags' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TagKeys" Core..=) Prelude.<$> tagKeys,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Type" Core..=) Prelude.<$> type',
            ("Status" Core..=) Prelude.<$> status,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListCostAllocationTags where
  toPath = Prelude.const "/"

instance Core.ToQuery ListCostAllocationTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCostAllocationTagsResponse' smart constructor.
data ListCostAllocationTagsResponse = ListCostAllocationTagsResponse'
  { -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of cost allocation tags that includes the detailed metadata for
    -- each one.
    costAllocationTags :: Prelude.Maybe [CostAllocationTag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCostAllocationTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCostAllocationTagsResponse_nextToken' - The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'costAllocationTags', 'listCostAllocationTagsResponse_costAllocationTags' - A list of cost allocation tags that includes the detailed metadata for
-- each one.
--
-- 'httpStatus', 'listCostAllocationTagsResponse_httpStatus' - The response's http status code.
newListCostAllocationTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCostAllocationTagsResponse
newListCostAllocationTagsResponse pHttpStatus_ =
  ListCostAllocationTagsResponse'
    { nextToken =
        Prelude.Nothing,
      costAllocationTags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
listCostAllocationTagsResponse_nextToken :: Lens.Lens' ListCostAllocationTagsResponse (Prelude.Maybe Prelude.Text)
listCostAllocationTagsResponse_nextToken = Lens.lens (\ListCostAllocationTagsResponse' {nextToken} -> nextToken) (\s@ListCostAllocationTagsResponse' {} a -> s {nextToken = a} :: ListCostAllocationTagsResponse)

-- | A list of cost allocation tags that includes the detailed metadata for
-- each one.
listCostAllocationTagsResponse_costAllocationTags :: Lens.Lens' ListCostAllocationTagsResponse (Prelude.Maybe [CostAllocationTag])
listCostAllocationTagsResponse_costAllocationTags = Lens.lens (\ListCostAllocationTagsResponse' {costAllocationTags} -> costAllocationTags) (\s@ListCostAllocationTagsResponse' {} a -> s {costAllocationTags = a} :: ListCostAllocationTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCostAllocationTagsResponse_httpStatus :: Lens.Lens' ListCostAllocationTagsResponse Prelude.Int
listCostAllocationTagsResponse_httpStatus = Lens.lens (\ListCostAllocationTagsResponse' {httpStatus} -> httpStatus) (\s@ListCostAllocationTagsResponse' {} a -> s {httpStatus = a} :: ListCostAllocationTagsResponse)

instance
  Prelude.NFData
    ListCostAllocationTagsResponse
  where
  rnf ListCostAllocationTagsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf costAllocationTags
      `Prelude.seq` Prelude.rnf httpStatus
