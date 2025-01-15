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
-- Module      : Amazonka.RolesAnywhere.ListProfiles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all profiles in the authenticated account and Amazon Web Services
-- Region.
--
-- __Required permissions:__ @rolesanywhere:ListProfiles@.
--
-- This operation returns paginated results.
module Amazonka.RolesAnywhere.ListProfiles
  ( -- * Creating a Request
    ListProfiles (..),
    newListProfiles,

    -- * Request Lenses
    listProfiles_nextToken,
    listProfiles_pageSize,

    -- * Destructuring the Response
    ListProfilesResponse (..),
    newListProfilesResponse,

    -- * Response Lenses
    listProfilesResponse_nextToken,
    listProfilesResponse_profiles,
    listProfilesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RolesAnywhere.Types

-- | /See:/ 'newListProfiles' smart constructor.
data ListProfiles = ListProfiles'
  { -- | A token that indicates where the output should continue from, if a
    -- previous operation did not show all results. To get the next results,
    -- call the operation again with this value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of resources in the paginated list.
    pageSize :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProfiles_nextToken' - A token that indicates where the output should continue from, if a
-- previous operation did not show all results. To get the next results,
-- call the operation again with this value.
--
-- 'pageSize', 'listProfiles_pageSize' - The number of resources in the paginated list.
newListProfiles ::
  ListProfiles
newListProfiles =
  ListProfiles'
    { nextToken = Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | A token that indicates where the output should continue from, if a
-- previous operation did not show all results. To get the next results,
-- call the operation again with this value.
listProfiles_nextToken :: Lens.Lens' ListProfiles (Prelude.Maybe Prelude.Text)
listProfiles_nextToken = Lens.lens (\ListProfiles' {nextToken} -> nextToken) (\s@ListProfiles' {} a -> s {nextToken = a} :: ListProfiles)

-- | The number of resources in the paginated list.
listProfiles_pageSize :: Lens.Lens' ListProfiles (Prelude.Maybe Prelude.Int)
listProfiles_pageSize = Lens.lens (\ListProfiles' {pageSize} -> pageSize) (\s@ListProfiles' {} a -> s {pageSize = a} :: ListProfiles)

instance Core.AWSPager ListProfiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProfilesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listProfilesResponse_profiles
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listProfiles_nextToken
              Lens..~ rs
              Lens.^? listProfilesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListProfiles where
  type AWSResponse ListProfiles = ListProfilesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProfilesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "profiles" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProfiles where
  hashWithSalt _salt ListProfiles' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pageSize

instance Prelude.NFData ListProfiles where
  rnf ListProfiles' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf pageSize

instance Data.ToHeaders ListProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListProfiles where
  toPath = Prelude.const "/profiles"

instance Data.ToQuery ListProfiles where
  toQuery ListProfiles' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "pageSize" Data.=: pageSize
      ]

-- | /See:/ 'newListProfilesResponse' smart constructor.
data ListProfilesResponse = ListProfilesResponse'
  { -- | A token that indicates where the output should continue from, if a
    -- previous operation did not show all results. To get the next results,
    -- call the operation again with this value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of profiles.
    profiles :: Prelude.Maybe [ProfileDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProfilesResponse_nextToken' - A token that indicates where the output should continue from, if a
-- previous operation did not show all results. To get the next results,
-- call the operation again with this value.
--
-- 'profiles', 'listProfilesResponse_profiles' - A list of profiles.
--
-- 'httpStatus', 'listProfilesResponse_httpStatus' - The response's http status code.
newListProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProfilesResponse
newListProfilesResponse pHttpStatus_ =
  ListProfilesResponse'
    { nextToken = Prelude.Nothing,
      profiles = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where the output should continue from, if a
-- previous operation did not show all results. To get the next results,
-- call the operation again with this value.
listProfilesResponse_nextToken :: Lens.Lens' ListProfilesResponse (Prelude.Maybe Prelude.Text)
listProfilesResponse_nextToken = Lens.lens (\ListProfilesResponse' {nextToken} -> nextToken) (\s@ListProfilesResponse' {} a -> s {nextToken = a} :: ListProfilesResponse)

-- | A list of profiles.
listProfilesResponse_profiles :: Lens.Lens' ListProfilesResponse (Prelude.Maybe [ProfileDetail])
listProfilesResponse_profiles = Lens.lens (\ListProfilesResponse' {profiles} -> profiles) (\s@ListProfilesResponse' {} a -> s {profiles = a} :: ListProfilesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listProfilesResponse_httpStatus :: Lens.Lens' ListProfilesResponse Prelude.Int
listProfilesResponse_httpStatus = Lens.lens (\ListProfilesResponse' {httpStatus} -> httpStatus) (\s@ListProfilesResponse' {} a -> s {httpStatus = a} :: ListProfilesResponse)

instance Prelude.NFData ListProfilesResponse where
  rnf ListProfilesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf profiles `Prelude.seq`
        Prelude.rnf httpStatus
