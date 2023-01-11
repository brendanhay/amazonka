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
-- Module      : Amazonka.Transfer.ListProfiles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the profiles for your system. If you want to limit the
-- results to a certain number, supply a value for the @MaxResults@
-- parameter. If you ran the command previously and received a value for
-- @NextToken@, you can supply that value to continue listing profiles from
-- where you left off.
--
-- This operation returns paginated results.
module Amazonka.Transfer.ListProfiles
  ( -- * Creating a Request
    ListProfiles (..),
    newListProfiles,

    -- * Request Lenses
    listProfiles_maxResults,
    listProfiles_nextToken,
    listProfiles_profileType,

    -- * Destructuring the Response
    ListProfilesResponse (..),
    newListProfilesResponse,

    -- * Response Lenses
    listProfilesResponse_nextToken,
    listProfilesResponse_httpStatus,
    listProfilesResponse_profiles,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newListProfiles' smart constructor.
data ListProfiles = ListProfiles'
  { -- | The maximum number of profiles to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | When there are additional results that were not returned, a @NextToken@
    -- parameter is returned. You can use that value for a subsequent call to
    -- @ListProfiles@ to continue listing results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to list only @LOCAL@ type profiles or only @PARTNER@
    -- type profiles. If not supplied in the request, the command lists all
    -- types of profiles.
    profileType :: Prelude.Maybe ProfileType
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
-- 'maxResults', 'listProfiles_maxResults' - The maximum number of profiles to return.
--
-- 'nextToken', 'listProfiles_nextToken' - When there are additional results that were not returned, a @NextToken@
-- parameter is returned. You can use that value for a subsequent call to
-- @ListProfiles@ to continue listing results.
--
-- 'profileType', 'listProfiles_profileType' - Indicates whether to list only @LOCAL@ type profiles or only @PARTNER@
-- type profiles. If not supplied in the request, the command lists all
-- types of profiles.
newListProfiles ::
  ListProfiles
newListProfiles =
  ListProfiles'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      profileType = Prelude.Nothing
    }

-- | The maximum number of profiles to return.
listProfiles_maxResults :: Lens.Lens' ListProfiles (Prelude.Maybe Prelude.Natural)
listProfiles_maxResults = Lens.lens (\ListProfiles' {maxResults} -> maxResults) (\s@ListProfiles' {} a -> s {maxResults = a} :: ListProfiles)

-- | When there are additional results that were not returned, a @NextToken@
-- parameter is returned. You can use that value for a subsequent call to
-- @ListProfiles@ to continue listing results.
listProfiles_nextToken :: Lens.Lens' ListProfiles (Prelude.Maybe Prelude.Text)
listProfiles_nextToken = Lens.lens (\ListProfiles' {nextToken} -> nextToken) (\s@ListProfiles' {} a -> s {nextToken = a} :: ListProfiles)

-- | Indicates whether to list only @LOCAL@ type profiles or only @PARTNER@
-- type profiles. If not supplied in the request, the command lists all
-- types of profiles.
listProfiles_profileType :: Lens.Lens' ListProfiles (Prelude.Maybe ProfileType)
listProfiles_profileType = Lens.lens (\ListProfiles' {profileType} -> profileType) (\s@ListProfiles' {} a -> s {profileType = a} :: ListProfiles)

instance Core.AWSPager ListProfiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProfilesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listProfilesResponse_profiles) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listProfiles_nextToken
          Lens..~ rs
          Lens.^? listProfilesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListProfiles where
  type AWSResponse ListProfiles = ListProfilesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProfilesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Profiles" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListProfiles where
  hashWithSalt _salt ListProfiles' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` profileType

instance Prelude.NFData ListProfiles where
  rnf ListProfiles' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf profileType

instance Data.ToHeaders ListProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.ListProfiles" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListProfiles where
  toJSON ListProfiles' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ProfileType" Data..=) Prelude.<$> profileType
          ]
      )

instance Data.ToPath ListProfiles where
  toPath = Prelude.const "/"

instance Data.ToQuery ListProfiles where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListProfilesResponse' smart constructor.
data ListProfilesResponse = ListProfilesResponse'
  { -- | Returns a token that you can use to call @ListProfiles@ again and
    -- receive additional results, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns an array, where each item contains the details of a profile.
    profiles :: [ListedProfile]
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
-- 'nextToken', 'listProfilesResponse_nextToken' - Returns a token that you can use to call @ListProfiles@ again and
-- receive additional results, if there are any.
--
-- 'httpStatus', 'listProfilesResponse_httpStatus' - The response's http status code.
--
-- 'profiles', 'listProfilesResponse_profiles' - Returns an array, where each item contains the details of a profile.
newListProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProfilesResponse
newListProfilesResponse pHttpStatus_ =
  ListProfilesResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      profiles = Prelude.mempty
    }

-- | Returns a token that you can use to call @ListProfiles@ again and
-- receive additional results, if there are any.
listProfilesResponse_nextToken :: Lens.Lens' ListProfilesResponse (Prelude.Maybe Prelude.Text)
listProfilesResponse_nextToken = Lens.lens (\ListProfilesResponse' {nextToken} -> nextToken) (\s@ListProfilesResponse' {} a -> s {nextToken = a} :: ListProfilesResponse)

-- | The response's http status code.
listProfilesResponse_httpStatus :: Lens.Lens' ListProfilesResponse Prelude.Int
listProfilesResponse_httpStatus = Lens.lens (\ListProfilesResponse' {httpStatus} -> httpStatus) (\s@ListProfilesResponse' {} a -> s {httpStatus = a} :: ListProfilesResponse)

-- | Returns an array, where each item contains the details of a profile.
listProfilesResponse_profiles :: Lens.Lens' ListProfilesResponse [ListedProfile]
listProfilesResponse_profiles = Lens.lens (\ListProfilesResponse' {profiles} -> profiles) (\s@ListProfilesResponse' {} a -> s {profiles = a} :: ListProfilesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListProfilesResponse where
  rnf ListProfilesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf profiles
