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
-- Module      : Amazonka.MediaPackageVOD.ListPackagingGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a collection of MediaPackage VOD PackagingGroup resources.
--
-- This operation returns paginated results.
module Amazonka.MediaPackageVOD.ListPackagingGroups
  ( -- * Creating a Request
    ListPackagingGroups (..),
    newListPackagingGroups,

    -- * Request Lenses
    listPackagingGroups_maxResults,
    listPackagingGroups_nextToken,

    -- * Destructuring the Response
    ListPackagingGroupsResponse (..),
    newListPackagingGroupsResponse,

    -- * Response Lenses
    listPackagingGroupsResponse_nextToken,
    listPackagingGroupsResponse_packagingGroups,
    listPackagingGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageVOD.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPackagingGroups' smart constructor.
data ListPackagingGroups = ListPackagingGroups'
  { -- | Upper bound on number of records to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token used to resume pagination from the end of a previous request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPackagingGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listPackagingGroups_maxResults' - Upper bound on number of records to return.
--
-- 'nextToken', 'listPackagingGroups_nextToken' - A token used to resume pagination from the end of a previous request.
newListPackagingGroups ::
  ListPackagingGroups
newListPackagingGroups =
  ListPackagingGroups'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Upper bound on number of records to return.
listPackagingGroups_maxResults :: Lens.Lens' ListPackagingGroups (Prelude.Maybe Prelude.Natural)
listPackagingGroups_maxResults = Lens.lens (\ListPackagingGroups' {maxResults} -> maxResults) (\s@ListPackagingGroups' {} a -> s {maxResults = a} :: ListPackagingGroups)

-- | A token used to resume pagination from the end of a previous request.
listPackagingGroups_nextToken :: Lens.Lens' ListPackagingGroups (Prelude.Maybe Prelude.Text)
listPackagingGroups_nextToken = Lens.lens (\ListPackagingGroups' {nextToken} -> nextToken) (\s@ListPackagingGroups' {} a -> s {nextToken = a} :: ListPackagingGroups)

instance Core.AWSPager ListPackagingGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPackagingGroupsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPackagingGroupsResponse_packagingGroups
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listPackagingGroups_nextToken
          Lens..~ rs
          Lens.^? listPackagingGroupsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListPackagingGroups where
  type
    AWSResponse ListPackagingGroups =
      ListPackagingGroupsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPackagingGroupsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "packagingGroups"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPackagingGroups where
  hashWithSalt _salt ListPackagingGroups' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListPackagingGroups where
  rnf ListPackagingGroups' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListPackagingGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListPackagingGroups where
  toPath = Prelude.const "/packaging_groups"

instance Data.ToQuery ListPackagingGroups where
  toQuery ListPackagingGroups' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListPackagingGroupsResponse' smart constructor.
data ListPackagingGroupsResponse = ListPackagingGroupsResponse'
  { -- | A token that can be used to resume pagination from the end of the
    -- collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of MediaPackage VOD PackagingGroup resources.
    packagingGroups :: Prelude.Maybe [PackagingGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPackagingGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPackagingGroupsResponse_nextToken' - A token that can be used to resume pagination from the end of the
-- collection.
--
-- 'packagingGroups', 'listPackagingGroupsResponse_packagingGroups' - A list of MediaPackage VOD PackagingGroup resources.
--
-- 'httpStatus', 'listPackagingGroupsResponse_httpStatus' - The response's http status code.
newListPackagingGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPackagingGroupsResponse
newListPackagingGroupsResponse pHttpStatus_ =
  ListPackagingGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      packagingGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used to resume pagination from the end of the
-- collection.
listPackagingGroupsResponse_nextToken :: Lens.Lens' ListPackagingGroupsResponse (Prelude.Maybe Prelude.Text)
listPackagingGroupsResponse_nextToken = Lens.lens (\ListPackagingGroupsResponse' {nextToken} -> nextToken) (\s@ListPackagingGroupsResponse' {} a -> s {nextToken = a} :: ListPackagingGroupsResponse)

-- | A list of MediaPackage VOD PackagingGroup resources.
listPackagingGroupsResponse_packagingGroups :: Lens.Lens' ListPackagingGroupsResponse (Prelude.Maybe [PackagingGroup])
listPackagingGroupsResponse_packagingGroups = Lens.lens (\ListPackagingGroupsResponse' {packagingGroups} -> packagingGroups) (\s@ListPackagingGroupsResponse' {} a -> s {packagingGroups = a} :: ListPackagingGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPackagingGroupsResponse_httpStatus :: Lens.Lens' ListPackagingGroupsResponse Prelude.Int
listPackagingGroupsResponse_httpStatus = Lens.lens (\ListPackagingGroupsResponse' {httpStatus} -> httpStatus) (\s@ListPackagingGroupsResponse' {} a -> s {httpStatus = a} :: ListPackagingGroupsResponse)

instance Prelude.NFData ListPackagingGroupsResponse where
  rnf ListPackagingGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf packagingGroups
      `Prelude.seq` Prelude.rnf httpStatus
