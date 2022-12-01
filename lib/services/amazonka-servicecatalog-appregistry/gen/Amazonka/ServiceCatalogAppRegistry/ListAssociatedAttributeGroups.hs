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
-- Module      : Amazonka.ServiceCatalogAppRegistry.ListAssociatedAttributeGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all attribute groups that are associated with specified
-- application. Results are paginated.
--
-- This operation returns paginated results.
module Amazonka.ServiceCatalogAppRegistry.ListAssociatedAttributeGroups
  ( -- * Creating a Request
    ListAssociatedAttributeGroups (..),
    newListAssociatedAttributeGroups,

    -- * Request Lenses
    listAssociatedAttributeGroups_nextToken,
    listAssociatedAttributeGroups_maxResults,
    listAssociatedAttributeGroups_application,

    -- * Destructuring the Response
    ListAssociatedAttributeGroupsResponse (..),
    newListAssociatedAttributeGroupsResponse,

    -- * Response Lenses
    listAssociatedAttributeGroupsResponse_nextToken,
    listAssociatedAttributeGroupsResponse_attributeGroups,
    listAssociatedAttributeGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalogAppRegistry.Types

-- | /See:/ 'newListAssociatedAttributeGroups' smart constructor.
data ListAssociatedAttributeGroups = ListAssociatedAttributeGroups'
  { -- | The token to use to get the next page of results after a previous API
    -- call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The upper bound of the number of results to return (cannot exceed 25).
    -- If this parameter is omitted, it defaults to 25. This value is optional.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name or ID of the application.
    application :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssociatedAttributeGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssociatedAttributeGroups_nextToken' - The token to use to get the next page of results after a previous API
-- call.
--
-- 'maxResults', 'listAssociatedAttributeGroups_maxResults' - The upper bound of the number of results to return (cannot exceed 25).
-- If this parameter is omitted, it defaults to 25. This value is optional.
--
-- 'application', 'listAssociatedAttributeGroups_application' - The name or ID of the application.
newListAssociatedAttributeGroups ::
  -- | 'application'
  Prelude.Text ->
  ListAssociatedAttributeGroups
newListAssociatedAttributeGroups pApplication_ =
  ListAssociatedAttributeGroups'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      application = pApplication_
    }

-- | The token to use to get the next page of results after a previous API
-- call.
listAssociatedAttributeGroups_nextToken :: Lens.Lens' ListAssociatedAttributeGroups (Prelude.Maybe Prelude.Text)
listAssociatedAttributeGroups_nextToken = Lens.lens (\ListAssociatedAttributeGroups' {nextToken} -> nextToken) (\s@ListAssociatedAttributeGroups' {} a -> s {nextToken = a} :: ListAssociatedAttributeGroups)

-- | The upper bound of the number of results to return (cannot exceed 25).
-- If this parameter is omitted, it defaults to 25. This value is optional.
listAssociatedAttributeGroups_maxResults :: Lens.Lens' ListAssociatedAttributeGroups (Prelude.Maybe Prelude.Natural)
listAssociatedAttributeGroups_maxResults = Lens.lens (\ListAssociatedAttributeGroups' {maxResults} -> maxResults) (\s@ListAssociatedAttributeGroups' {} a -> s {maxResults = a} :: ListAssociatedAttributeGroups)

-- | The name or ID of the application.
listAssociatedAttributeGroups_application :: Lens.Lens' ListAssociatedAttributeGroups Prelude.Text
listAssociatedAttributeGroups_application = Lens.lens (\ListAssociatedAttributeGroups' {application} -> application) (\s@ListAssociatedAttributeGroups' {} a -> s {application = a} :: ListAssociatedAttributeGroups)

instance Core.AWSPager ListAssociatedAttributeGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssociatedAttributeGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAssociatedAttributeGroupsResponse_attributeGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAssociatedAttributeGroups_nextToken
          Lens..~ rs
          Lens.^? listAssociatedAttributeGroupsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListAssociatedAttributeGroups
  where
  type
    AWSResponse ListAssociatedAttributeGroups =
      ListAssociatedAttributeGroupsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociatedAttributeGroupsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "attributeGroups"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAssociatedAttributeGroups
  where
  hashWithSalt _salt ListAssociatedAttributeGroups' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` application

instance Prelude.NFData ListAssociatedAttributeGroups where
  rnf ListAssociatedAttributeGroups' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf application

instance Core.ToHeaders ListAssociatedAttributeGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListAssociatedAttributeGroups where
  toPath ListAssociatedAttributeGroups' {..} =
    Prelude.mconcat
      [ "/applications/",
        Core.toBS application,
        "/attribute-groups"
      ]

instance Core.ToQuery ListAssociatedAttributeGroups where
  toQuery ListAssociatedAttributeGroups' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListAssociatedAttributeGroupsResponse' smart constructor.
data ListAssociatedAttributeGroupsResponse = ListAssociatedAttributeGroupsResponse'
  { -- | The token to use to get the next page of results after a previous API
    -- call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of attribute group IDs.
    attributeGroups :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssociatedAttributeGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssociatedAttributeGroupsResponse_nextToken' - The token to use to get the next page of results after a previous API
-- call.
--
-- 'attributeGroups', 'listAssociatedAttributeGroupsResponse_attributeGroups' - A list of attribute group IDs.
--
-- 'httpStatus', 'listAssociatedAttributeGroupsResponse_httpStatus' - The response's http status code.
newListAssociatedAttributeGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssociatedAttributeGroupsResponse
newListAssociatedAttributeGroupsResponse pHttpStatus_ =
  ListAssociatedAttributeGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      attributeGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next page of results after a previous API
-- call.
listAssociatedAttributeGroupsResponse_nextToken :: Lens.Lens' ListAssociatedAttributeGroupsResponse (Prelude.Maybe Prelude.Text)
listAssociatedAttributeGroupsResponse_nextToken = Lens.lens (\ListAssociatedAttributeGroupsResponse' {nextToken} -> nextToken) (\s@ListAssociatedAttributeGroupsResponse' {} a -> s {nextToken = a} :: ListAssociatedAttributeGroupsResponse)

-- | A list of attribute group IDs.
listAssociatedAttributeGroupsResponse_attributeGroups :: Lens.Lens' ListAssociatedAttributeGroupsResponse (Prelude.Maybe [Prelude.Text])
listAssociatedAttributeGroupsResponse_attributeGroups = Lens.lens (\ListAssociatedAttributeGroupsResponse' {attributeGroups} -> attributeGroups) (\s@ListAssociatedAttributeGroupsResponse' {} a -> s {attributeGroups = a} :: ListAssociatedAttributeGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAssociatedAttributeGroupsResponse_httpStatus :: Lens.Lens' ListAssociatedAttributeGroupsResponse Prelude.Int
listAssociatedAttributeGroupsResponse_httpStatus = Lens.lens (\ListAssociatedAttributeGroupsResponse' {httpStatus} -> httpStatus) (\s@ListAssociatedAttributeGroupsResponse' {} a -> s {httpStatus = a} :: ListAssociatedAttributeGroupsResponse)

instance
  Prelude.NFData
    ListAssociatedAttributeGroupsResponse
  where
  rnf ListAssociatedAttributeGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf attributeGroups
      `Prelude.seq` Prelude.rnf httpStatus
