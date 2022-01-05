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
-- Module      : Amazonka.Nimble.ListLaunchProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all the launch profiles a studio.
--
-- This operation returns paginated results.
module Amazonka.Nimble.ListLaunchProfiles
  ( -- * Creating a Request
    ListLaunchProfiles (..),
    newListLaunchProfiles,

    -- * Request Lenses
    listLaunchProfiles_states,
    listLaunchProfiles_principalId,
    listLaunchProfiles_nextToken,
    listLaunchProfiles_maxResults,
    listLaunchProfiles_studioId,

    -- * Destructuring the Response
    ListLaunchProfilesResponse (..),
    newListLaunchProfilesResponse,

    -- * Response Lenses
    listLaunchProfilesResponse_launchProfiles,
    listLaunchProfilesResponse_nextToken,
    listLaunchProfilesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLaunchProfiles' smart constructor.
data ListLaunchProfiles = ListLaunchProfiles'
  { -- | A list of states.
    states :: Prelude.Maybe [Prelude.Text],
    -- | The principal ID.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLaunchProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'states', 'listLaunchProfiles_states' - A list of states.
--
-- 'principalId', 'listLaunchProfiles_principalId' - The principal ID.
--
-- 'nextToken', 'listLaunchProfiles_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'maxResults', 'listLaunchProfiles_maxResults' - The maximum number of results to be returned per request.
--
-- 'studioId', 'listLaunchProfiles_studioId' - The studio ID.
newListLaunchProfiles ::
  -- | 'studioId'
  Prelude.Text ->
  ListLaunchProfiles
newListLaunchProfiles pStudioId_ =
  ListLaunchProfiles'
    { states = Prelude.Nothing,
      principalId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      studioId = pStudioId_
    }

-- | A list of states.
listLaunchProfiles_states :: Lens.Lens' ListLaunchProfiles (Prelude.Maybe [Prelude.Text])
listLaunchProfiles_states = Lens.lens (\ListLaunchProfiles' {states} -> states) (\s@ListLaunchProfiles' {} a -> s {states = a} :: ListLaunchProfiles) Prelude.. Lens.mapping Lens.coerced

-- | The principal ID.
listLaunchProfiles_principalId :: Lens.Lens' ListLaunchProfiles (Prelude.Maybe Prelude.Text)
listLaunchProfiles_principalId = Lens.lens (\ListLaunchProfiles' {principalId} -> principalId) (\s@ListLaunchProfiles' {} a -> s {principalId = a} :: ListLaunchProfiles)

-- | The token for the next set of results, or null if there are no more
-- results.
listLaunchProfiles_nextToken :: Lens.Lens' ListLaunchProfiles (Prelude.Maybe Prelude.Text)
listLaunchProfiles_nextToken = Lens.lens (\ListLaunchProfiles' {nextToken} -> nextToken) (\s@ListLaunchProfiles' {} a -> s {nextToken = a} :: ListLaunchProfiles)

-- | The maximum number of results to be returned per request.
listLaunchProfiles_maxResults :: Lens.Lens' ListLaunchProfiles (Prelude.Maybe Prelude.Natural)
listLaunchProfiles_maxResults = Lens.lens (\ListLaunchProfiles' {maxResults} -> maxResults) (\s@ListLaunchProfiles' {} a -> s {maxResults = a} :: ListLaunchProfiles)

-- | The studio ID.
listLaunchProfiles_studioId :: Lens.Lens' ListLaunchProfiles Prelude.Text
listLaunchProfiles_studioId = Lens.lens (\ListLaunchProfiles' {studioId} -> studioId) (\s@ListLaunchProfiles' {} a -> s {studioId = a} :: ListLaunchProfiles)

instance Core.AWSPager ListLaunchProfiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLaunchProfilesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLaunchProfilesResponse_launchProfiles
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listLaunchProfiles_nextToken
          Lens..~ rs
          Lens.^? listLaunchProfilesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListLaunchProfiles where
  type
    AWSResponse ListLaunchProfiles =
      ListLaunchProfilesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLaunchProfilesResponse'
            Prelude.<$> (x Core..?> "launchProfiles" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLaunchProfiles where
  hashWithSalt _salt ListLaunchProfiles' {..} =
    _salt `Prelude.hashWithSalt` states
      `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData ListLaunchProfiles where
  rnf ListLaunchProfiles' {..} =
    Prelude.rnf states
      `Prelude.seq` Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf studioId

instance Core.ToHeaders ListLaunchProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListLaunchProfiles where
  toPath ListLaunchProfiles' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/launch-profiles"
      ]

instance Core.ToQuery ListLaunchProfiles where
  toQuery ListLaunchProfiles' {..} =
    Prelude.mconcat
      [ "states"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> states),
        "principalId" Core.=: principalId,
        "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListLaunchProfilesResponse' smart constructor.
data ListLaunchProfilesResponse = ListLaunchProfilesResponse'
  { -- | A collection of launch profiles.
    launchProfiles :: Prelude.Maybe [LaunchProfile],
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLaunchProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchProfiles', 'listLaunchProfilesResponse_launchProfiles' - A collection of launch profiles.
--
-- 'nextToken', 'listLaunchProfilesResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listLaunchProfilesResponse_httpStatus' - The response's http status code.
newListLaunchProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLaunchProfilesResponse
newListLaunchProfilesResponse pHttpStatus_ =
  ListLaunchProfilesResponse'
    { launchProfiles =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of launch profiles.
listLaunchProfilesResponse_launchProfiles :: Lens.Lens' ListLaunchProfilesResponse (Prelude.Maybe [LaunchProfile])
listLaunchProfilesResponse_launchProfiles = Lens.lens (\ListLaunchProfilesResponse' {launchProfiles} -> launchProfiles) (\s@ListLaunchProfilesResponse' {} a -> s {launchProfiles = a} :: ListLaunchProfilesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
listLaunchProfilesResponse_nextToken :: Lens.Lens' ListLaunchProfilesResponse (Prelude.Maybe Prelude.Text)
listLaunchProfilesResponse_nextToken = Lens.lens (\ListLaunchProfilesResponse' {nextToken} -> nextToken) (\s@ListLaunchProfilesResponse' {} a -> s {nextToken = a} :: ListLaunchProfilesResponse)

-- | The response's http status code.
listLaunchProfilesResponse_httpStatus :: Lens.Lens' ListLaunchProfilesResponse Prelude.Int
listLaunchProfilesResponse_httpStatus = Lens.lens (\ListLaunchProfilesResponse' {httpStatus} -> httpStatus) (\s@ListLaunchProfilesResponse' {} a -> s {httpStatus = a} :: ListLaunchProfilesResponse)

instance Prelude.NFData ListLaunchProfilesResponse where
  rnf ListLaunchProfilesResponse' {..} =
    Prelude.rnf launchProfiles
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
