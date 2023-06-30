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
-- Module      : Amazonka.Signer.ListSigningProfiles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all available signing profiles in your AWS account. Returns only
-- profiles with an @ACTIVE@ status unless the @includeCanceled@ request
-- field is set to @true@. If additional jobs remain to be listed, code
-- signing returns a @nextToken@ value. Use this value in subsequent calls
-- to @ListSigningJobs@ to fetch the remaining values. You can continue
-- calling @ListSigningJobs@ with your @maxResults@ parameter and with new
-- values that code signing returns in the @nextToken@ parameter until all
-- of your signing jobs have been returned.
--
-- This operation returns paginated results.
module Amazonka.Signer.ListSigningProfiles
  ( -- * Creating a Request
    ListSigningProfiles (..),
    newListSigningProfiles,

    -- * Request Lenses
    listSigningProfiles_includeCanceled,
    listSigningProfiles_maxResults,
    listSigningProfiles_nextToken,
    listSigningProfiles_platformId,
    listSigningProfiles_statuses,

    -- * Destructuring the Response
    ListSigningProfilesResponse (..),
    newListSigningProfilesResponse,

    -- * Response Lenses
    listSigningProfilesResponse_nextToken,
    listSigningProfilesResponse_profiles,
    listSigningProfilesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Signer.Types

-- | /See:/ 'newListSigningProfiles' smart constructor.
data ListSigningProfiles = ListSigningProfiles'
  { -- | Designates whether to include profiles with the status of @CANCELED@.
    includeCanceled :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of profiles to be returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Value for specifying the next set of paginated results to return. After
    -- you receive a response with truncated results, use this parameter in a
    -- subsequent request. Set it to the value of @nextToken@ from the response
    -- that you just received.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters results to return only signing jobs initiated for a specified
    -- signing platform.
    platformId :: Prelude.Maybe Prelude.Text,
    -- | Filters results to return only signing jobs with statuses in the
    -- specified list.
    statuses :: Prelude.Maybe [SigningProfileStatus]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSigningProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeCanceled', 'listSigningProfiles_includeCanceled' - Designates whether to include profiles with the status of @CANCELED@.
--
-- 'maxResults', 'listSigningProfiles_maxResults' - The maximum number of profiles to be returned.
--
-- 'nextToken', 'listSigningProfiles_nextToken' - Value for specifying the next set of paginated results to return. After
-- you receive a response with truncated results, use this parameter in a
-- subsequent request. Set it to the value of @nextToken@ from the response
-- that you just received.
--
-- 'platformId', 'listSigningProfiles_platformId' - Filters results to return only signing jobs initiated for a specified
-- signing platform.
--
-- 'statuses', 'listSigningProfiles_statuses' - Filters results to return only signing jobs with statuses in the
-- specified list.
newListSigningProfiles ::
  ListSigningProfiles
newListSigningProfiles =
  ListSigningProfiles'
    { includeCanceled =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      platformId = Prelude.Nothing,
      statuses = Prelude.Nothing
    }

-- | Designates whether to include profiles with the status of @CANCELED@.
listSigningProfiles_includeCanceled :: Lens.Lens' ListSigningProfiles (Prelude.Maybe Prelude.Bool)
listSigningProfiles_includeCanceled = Lens.lens (\ListSigningProfiles' {includeCanceled} -> includeCanceled) (\s@ListSigningProfiles' {} a -> s {includeCanceled = a} :: ListSigningProfiles)

-- | The maximum number of profiles to be returned.
listSigningProfiles_maxResults :: Lens.Lens' ListSigningProfiles (Prelude.Maybe Prelude.Natural)
listSigningProfiles_maxResults = Lens.lens (\ListSigningProfiles' {maxResults} -> maxResults) (\s@ListSigningProfiles' {} a -> s {maxResults = a} :: ListSigningProfiles)

-- | Value for specifying the next set of paginated results to return. After
-- you receive a response with truncated results, use this parameter in a
-- subsequent request. Set it to the value of @nextToken@ from the response
-- that you just received.
listSigningProfiles_nextToken :: Lens.Lens' ListSigningProfiles (Prelude.Maybe Prelude.Text)
listSigningProfiles_nextToken = Lens.lens (\ListSigningProfiles' {nextToken} -> nextToken) (\s@ListSigningProfiles' {} a -> s {nextToken = a} :: ListSigningProfiles)

-- | Filters results to return only signing jobs initiated for a specified
-- signing platform.
listSigningProfiles_platformId :: Lens.Lens' ListSigningProfiles (Prelude.Maybe Prelude.Text)
listSigningProfiles_platformId = Lens.lens (\ListSigningProfiles' {platformId} -> platformId) (\s@ListSigningProfiles' {} a -> s {platformId = a} :: ListSigningProfiles)

-- | Filters results to return only signing jobs with statuses in the
-- specified list.
listSigningProfiles_statuses :: Lens.Lens' ListSigningProfiles (Prelude.Maybe [SigningProfileStatus])
listSigningProfiles_statuses = Lens.lens (\ListSigningProfiles' {statuses} -> statuses) (\s@ListSigningProfiles' {} a -> s {statuses = a} :: ListSigningProfiles) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager ListSigningProfiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSigningProfilesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSigningProfilesResponse_profiles
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSigningProfiles_nextToken
          Lens..~ rs
          Lens.^? listSigningProfilesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSigningProfiles where
  type
    AWSResponse ListSigningProfiles =
      ListSigningProfilesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSigningProfilesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "profiles" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSigningProfiles where
  hashWithSalt _salt ListSigningProfiles' {..} =
    _salt
      `Prelude.hashWithSalt` includeCanceled
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` platformId
      `Prelude.hashWithSalt` statuses

instance Prelude.NFData ListSigningProfiles where
  rnf ListSigningProfiles' {..} =
    Prelude.rnf includeCanceled
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf platformId
      `Prelude.seq` Prelude.rnf statuses

instance Data.ToHeaders ListSigningProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSigningProfiles where
  toPath = Prelude.const "/signing-profiles"

instance Data.ToQuery ListSigningProfiles where
  toQuery ListSigningProfiles' {..} =
    Prelude.mconcat
      [ "includeCanceled" Data.=: includeCanceled,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "platformId" Data.=: platformId,
        "statuses"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> statuses)
      ]

-- | /See:/ 'newListSigningProfilesResponse' smart constructor.
data ListSigningProfilesResponse = ListSigningProfilesResponse'
  { -- | Value for specifying the next set of paginated results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of profiles that are available in the AWS account. This includes
    -- profiles with the status of @CANCELED@ if the @includeCanceled@
    -- parameter is set to @true@.
    profiles :: Prelude.Maybe [SigningProfile],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSigningProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSigningProfilesResponse_nextToken' - Value for specifying the next set of paginated results to return.
--
-- 'profiles', 'listSigningProfilesResponse_profiles' - A list of profiles that are available in the AWS account. This includes
-- profiles with the status of @CANCELED@ if the @includeCanceled@
-- parameter is set to @true@.
--
-- 'httpStatus', 'listSigningProfilesResponse_httpStatus' - The response's http status code.
newListSigningProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSigningProfilesResponse
newListSigningProfilesResponse pHttpStatus_ =
  ListSigningProfilesResponse'
    { nextToken =
        Prelude.Nothing,
      profiles = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Value for specifying the next set of paginated results to return.
listSigningProfilesResponse_nextToken :: Lens.Lens' ListSigningProfilesResponse (Prelude.Maybe Prelude.Text)
listSigningProfilesResponse_nextToken = Lens.lens (\ListSigningProfilesResponse' {nextToken} -> nextToken) (\s@ListSigningProfilesResponse' {} a -> s {nextToken = a} :: ListSigningProfilesResponse)

-- | A list of profiles that are available in the AWS account. This includes
-- profiles with the status of @CANCELED@ if the @includeCanceled@
-- parameter is set to @true@.
listSigningProfilesResponse_profiles :: Lens.Lens' ListSigningProfilesResponse (Prelude.Maybe [SigningProfile])
listSigningProfilesResponse_profiles = Lens.lens (\ListSigningProfilesResponse' {profiles} -> profiles) (\s@ListSigningProfilesResponse' {} a -> s {profiles = a} :: ListSigningProfilesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSigningProfilesResponse_httpStatus :: Lens.Lens' ListSigningProfilesResponse Prelude.Int
listSigningProfilesResponse_httpStatus = Lens.lens (\ListSigningProfilesResponse' {httpStatus} -> httpStatus) (\s@ListSigningProfilesResponse' {} a -> s {httpStatus = a} :: ListSigningProfilesResponse)

instance Prelude.NFData ListSigningProfilesResponse where
  rnf ListSigningProfilesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf profiles
      `Prelude.seq` Prelude.rnf httpStatus
