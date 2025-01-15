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
-- Module      : Amazonka.FinSpaceData.ListPermissionGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all available permission groups in FinSpace.
--
-- This operation returns paginated results.
module Amazonka.FinSpaceData.ListPermissionGroups
  ( -- * Creating a Request
    ListPermissionGroups (..),
    newListPermissionGroups,

    -- * Request Lenses
    listPermissionGroups_nextToken,
    listPermissionGroups_maxResults,

    -- * Destructuring the Response
    ListPermissionGroupsResponse (..),
    newListPermissionGroupsResponse,

    -- * Response Lenses
    listPermissionGroupsResponse_nextToken,
    listPermissionGroupsResponse_permissionGroups,
    listPermissionGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPermissionGroups' smart constructor.
data ListPermissionGroups = ListPermissionGroups'
  { -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results per page.
    maxResults :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPermissionGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPermissionGroups_nextToken' - A token that indicates where a results page should begin.
--
-- 'maxResults', 'listPermissionGroups_maxResults' - The maximum number of results per page.
newListPermissionGroups ::
  -- | 'maxResults'
  Prelude.Natural ->
  ListPermissionGroups
newListPermissionGroups pMaxResults_ =
  ListPermissionGroups'
    { nextToken = Prelude.Nothing,
      maxResults = pMaxResults_
    }

-- | A token that indicates where a results page should begin.
listPermissionGroups_nextToken :: Lens.Lens' ListPermissionGroups (Prelude.Maybe Prelude.Text)
listPermissionGroups_nextToken = Lens.lens (\ListPermissionGroups' {nextToken} -> nextToken) (\s@ListPermissionGroups' {} a -> s {nextToken = a} :: ListPermissionGroups)

-- | The maximum number of results per page.
listPermissionGroups_maxResults :: Lens.Lens' ListPermissionGroups Prelude.Natural
listPermissionGroups_maxResults = Lens.lens (\ListPermissionGroups' {maxResults} -> maxResults) (\s@ListPermissionGroups' {} a -> s {maxResults = a} :: ListPermissionGroups)

instance Core.AWSPager ListPermissionGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPermissionGroupsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPermissionGroupsResponse_permissionGroups
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listPermissionGroups_nextToken
              Lens..~ rs
              Lens.^? listPermissionGroupsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListPermissionGroups where
  type
    AWSResponse ListPermissionGroups =
      ListPermissionGroupsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPermissionGroupsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "permissionGroups"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPermissionGroups where
  hashWithSalt _salt ListPermissionGroups' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListPermissionGroups where
  rnf ListPermissionGroups' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf maxResults

instance Data.ToHeaders ListPermissionGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListPermissionGroups where
  toPath = Prelude.const "/permission-group"

instance Data.ToQuery ListPermissionGroups where
  toQuery ListPermissionGroups' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListPermissionGroupsResponse' smart constructor.
data ListPermissionGroupsResponse = ListPermissionGroupsResponse'
  { -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of all the permission groups.
    permissionGroups :: Prelude.Maybe [PermissionGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPermissionGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPermissionGroupsResponse_nextToken' - A token that indicates where a results page should begin.
--
-- 'permissionGroups', 'listPermissionGroupsResponse_permissionGroups' - A list of all the permission groups.
--
-- 'httpStatus', 'listPermissionGroupsResponse_httpStatus' - The response's http status code.
newListPermissionGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPermissionGroupsResponse
newListPermissionGroupsResponse pHttpStatus_ =
  ListPermissionGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      permissionGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where a results page should begin.
listPermissionGroupsResponse_nextToken :: Lens.Lens' ListPermissionGroupsResponse (Prelude.Maybe Prelude.Text)
listPermissionGroupsResponse_nextToken = Lens.lens (\ListPermissionGroupsResponse' {nextToken} -> nextToken) (\s@ListPermissionGroupsResponse' {} a -> s {nextToken = a} :: ListPermissionGroupsResponse)

-- | A list of all the permission groups.
listPermissionGroupsResponse_permissionGroups :: Lens.Lens' ListPermissionGroupsResponse (Prelude.Maybe [PermissionGroup])
listPermissionGroupsResponse_permissionGroups = Lens.lens (\ListPermissionGroupsResponse' {permissionGroups} -> permissionGroups) (\s@ListPermissionGroupsResponse' {} a -> s {permissionGroups = a} :: ListPermissionGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPermissionGroupsResponse_httpStatus :: Lens.Lens' ListPermissionGroupsResponse Prelude.Int
listPermissionGroupsResponse_httpStatus = Lens.lens (\ListPermissionGroupsResponse' {httpStatus} -> httpStatus) (\s@ListPermissionGroupsResponse' {} a -> s {httpStatus = a} :: ListPermissionGroupsResponse)

instance Prelude.NFData ListPermissionGroupsResponse where
  rnf ListPermissionGroupsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf permissionGroups `Prelude.seq`
        Prelude.rnf httpStatus
