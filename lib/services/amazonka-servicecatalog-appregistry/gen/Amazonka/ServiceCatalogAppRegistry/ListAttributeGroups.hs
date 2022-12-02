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
-- Module      : Amazonka.ServiceCatalogAppRegistry.ListAttributeGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all attribute groups which you have access to. Results are
-- paginated.
--
-- This operation returns paginated results.
module Amazonka.ServiceCatalogAppRegistry.ListAttributeGroups
  ( -- * Creating a Request
    ListAttributeGroups (..),
    newListAttributeGroups,

    -- * Request Lenses
    listAttributeGroups_nextToken,
    listAttributeGroups_maxResults,

    -- * Destructuring the Response
    ListAttributeGroupsResponse (..),
    newListAttributeGroupsResponse,

    -- * Response Lenses
    listAttributeGroupsResponse_nextToken,
    listAttributeGroupsResponse_attributeGroups,
    listAttributeGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalogAppRegistry.Types

-- | /See:/ 'newListAttributeGroups' smart constructor.
data ListAttributeGroups = ListAttributeGroups'
  { -- | The token to use to get the next page of results after a previous API
    -- call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The upper bound of the number of results to return (cannot exceed 25).
    -- If this parameter is omitted, it defaults to 25. This value is optional.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAttributeGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAttributeGroups_nextToken' - The token to use to get the next page of results after a previous API
-- call.
--
-- 'maxResults', 'listAttributeGroups_maxResults' - The upper bound of the number of results to return (cannot exceed 25).
-- If this parameter is omitted, it defaults to 25. This value is optional.
newListAttributeGroups ::
  ListAttributeGroups
newListAttributeGroups =
  ListAttributeGroups'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token to use to get the next page of results after a previous API
-- call.
listAttributeGroups_nextToken :: Lens.Lens' ListAttributeGroups (Prelude.Maybe Prelude.Text)
listAttributeGroups_nextToken = Lens.lens (\ListAttributeGroups' {nextToken} -> nextToken) (\s@ListAttributeGroups' {} a -> s {nextToken = a} :: ListAttributeGroups)

-- | The upper bound of the number of results to return (cannot exceed 25).
-- If this parameter is omitted, it defaults to 25. This value is optional.
listAttributeGroups_maxResults :: Lens.Lens' ListAttributeGroups (Prelude.Maybe Prelude.Natural)
listAttributeGroups_maxResults = Lens.lens (\ListAttributeGroups' {maxResults} -> maxResults) (\s@ListAttributeGroups' {} a -> s {maxResults = a} :: ListAttributeGroups)

instance Core.AWSPager ListAttributeGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAttributeGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAttributeGroupsResponse_attributeGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAttributeGroups_nextToken
          Lens..~ rs
          Lens.^? listAttributeGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAttributeGroups where
  type
    AWSResponse ListAttributeGroups =
      ListAttributeGroupsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAttributeGroupsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "attributeGroups"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAttributeGroups where
  hashWithSalt _salt ListAttributeGroups' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListAttributeGroups where
  rnf ListAttributeGroups' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListAttributeGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListAttributeGroups where
  toPath = Prelude.const "/attribute-groups"

instance Data.ToQuery ListAttributeGroups where
  toQuery ListAttributeGroups' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListAttributeGroupsResponse' smart constructor.
data ListAttributeGroupsResponse = ListAttributeGroupsResponse'
  { -- | The token to use to get the next page of results after a previous API
    -- call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | This list of attribute groups.
    attributeGroups :: Prelude.Maybe [AttributeGroupSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAttributeGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAttributeGroupsResponse_nextToken' - The token to use to get the next page of results after a previous API
-- call.
--
-- 'attributeGroups', 'listAttributeGroupsResponse_attributeGroups' - This list of attribute groups.
--
-- 'httpStatus', 'listAttributeGroupsResponse_httpStatus' - The response's http status code.
newListAttributeGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAttributeGroupsResponse
newListAttributeGroupsResponse pHttpStatus_ =
  ListAttributeGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      attributeGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next page of results after a previous API
-- call.
listAttributeGroupsResponse_nextToken :: Lens.Lens' ListAttributeGroupsResponse (Prelude.Maybe Prelude.Text)
listAttributeGroupsResponse_nextToken = Lens.lens (\ListAttributeGroupsResponse' {nextToken} -> nextToken) (\s@ListAttributeGroupsResponse' {} a -> s {nextToken = a} :: ListAttributeGroupsResponse)

-- | This list of attribute groups.
listAttributeGroupsResponse_attributeGroups :: Lens.Lens' ListAttributeGroupsResponse (Prelude.Maybe [AttributeGroupSummary])
listAttributeGroupsResponse_attributeGroups = Lens.lens (\ListAttributeGroupsResponse' {attributeGroups} -> attributeGroups) (\s@ListAttributeGroupsResponse' {} a -> s {attributeGroups = a} :: ListAttributeGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAttributeGroupsResponse_httpStatus :: Lens.Lens' ListAttributeGroupsResponse Prelude.Int
listAttributeGroupsResponse_httpStatus = Lens.lens (\ListAttributeGroupsResponse' {httpStatus} -> httpStatus) (\s@ListAttributeGroupsResponse' {} a -> s {httpStatus = a} :: ListAttributeGroupsResponse)

instance Prelude.NFData ListAttributeGroupsResponse where
  rnf ListAttributeGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf attributeGroups
      `Prelude.seq` Prelude.rnf httpStatus
