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
-- Module      : Amazonka.GroundStation.ListMissionProfiles
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of mission profiles.
--
-- This operation returns paginated results.
module Amazonka.GroundStation.ListMissionProfiles
  ( -- * Creating a Request
    ListMissionProfiles (..),
    newListMissionProfiles,

    -- * Request Lenses
    listMissionProfiles_nextToken,
    listMissionProfiles_maxResults,

    -- * Destructuring the Response
    ListMissionProfilesResponse (..),
    newListMissionProfilesResponse,

    -- * Response Lenses
    listMissionProfilesResponse_nextToken,
    listMissionProfilesResponse_missionProfileList,
    listMissionProfilesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newListMissionProfiles' smart constructor.
data ListMissionProfiles = ListMissionProfiles'
  { -- | Next token returned in the request of a previous @ListMissionProfiles@
    -- call. Used to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of mission profiles returned.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMissionProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMissionProfiles_nextToken' - Next token returned in the request of a previous @ListMissionProfiles@
-- call. Used to get the next page of results.
--
-- 'maxResults', 'listMissionProfiles_maxResults' - Maximum number of mission profiles returned.
newListMissionProfiles ::
  ListMissionProfiles
newListMissionProfiles =
  ListMissionProfiles'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Next token returned in the request of a previous @ListMissionProfiles@
-- call. Used to get the next page of results.
listMissionProfiles_nextToken :: Lens.Lens' ListMissionProfiles (Prelude.Maybe Prelude.Text)
listMissionProfiles_nextToken = Lens.lens (\ListMissionProfiles' {nextToken} -> nextToken) (\s@ListMissionProfiles' {} a -> s {nextToken = a} :: ListMissionProfiles)

-- | Maximum number of mission profiles returned.
listMissionProfiles_maxResults :: Lens.Lens' ListMissionProfiles (Prelude.Maybe Prelude.Natural)
listMissionProfiles_maxResults = Lens.lens (\ListMissionProfiles' {maxResults} -> maxResults) (\s@ListMissionProfiles' {} a -> s {maxResults = a} :: ListMissionProfiles)

instance Core.AWSPager ListMissionProfiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMissionProfilesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listMissionProfilesResponse_missionProfileList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listMissionProfiles_nextToken
          Lens..~ rs
          Lens.^? listMissionProfilesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListMissionProfiles where
  type
    AWSResponse ListMissionProfiles =
      ListMissionProfilesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMissionProfilesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "missionProfileList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMissionProfiles where
  hashWithSalt _salt ListMissionProfiles' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListMissionProfiles where
  rnf ListMissionProfiles' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListMissionProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListMissionProfiles where
  toPath = Prelude.const "/missionprofile"

instance Core.ToQuery ListMissionProfiles where
  toQuery ListMissionProfiles' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- |
--
-- /See:/ 'newListMissionProfilesResponse' smart constructor.
data ListMissionProfilesResponse = ListMissionProfilesResponse'
  { -- | Next token returned in the response of a previous @ListMissionProfiles@
    -- call. Used to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of mission profiles.
    missionProfileList :: Prelude.Maybe [MissionProfileListItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMissionProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMissionProfilesResponse_nextToken' - Next token returned in the response of a previous @ListMissionProfiles@
-- call. Used to get the next page of results.
--
-- 'missionProfileList', 'listMissionProfilesResponse_missionProfileList' - List of mission profiles.
--
-- 'httpStatus', 'listMissionProfilesResponse_httpStatus' - The response's http status code.
newListMissionProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMissionProfilesResponse
newListMissionProfilesResponse pHttpStatus_ =
  ListMissionProfilesResponse'
    { nextToken =
        Prelude.Nothing,
      missionProfileList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Next token returned in the response of a previous @ListMissionProfiles@
-- call. Used to get the next page of results.
listMissionProfilesResponse_nextToken :: Lens.Lens' ListMissionProfilesResponse (Prelude.Maybe Prelude.Text)
listMissionProfilesResponse_nextToken = Lens.lens (\ListMissionProfilesResponse' {nextToken} -> nextToken) (\s@ListMissionProfilesResponse' {} a -> s {nextToken = a} :: ListMissionProfilesResponse)

-- | List of mission profiles.
listMissionProfilesResponse_missionProfileList :: Lens.Lens' ListMissionProfilesResponse (Prelude.Maybe [MissionProfileListItem])
listMissionProfilesResponse_missionProfileList = Lens.lens (\ListMissionProfilesResponse' {missionProfileList} -> missionProfileList) (\s@ListMissionProfilesResponse' {} a -> s {missionProfileList = a} :: ListMissionProfilesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listMissionProfilesResponse_httpStatus :: Lens.Lens' ListMissionProfilesResponse Prelude.Int
listMissionProfilesResponse_httpStatus = Lens.lens (\ListMissionProfilesResponse' {httpStatus} -> httpStatus) (\s@ListMissionProfilesResponse' {} a -> s {httpStatus = a} :: ListMissionProfilesResponse)

instance Prelude.NFData ListMissionProfilesResponse where
  rnf ListMissionProfilesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf missionProfileList
      `Prelude.seq` Prelude.rnf httpStatus
