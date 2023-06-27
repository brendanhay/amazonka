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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    listMissionProfiles_maxResults,
    listMissionProfiles_nextToken,

    -- * Destructuring the Response
    ListMissionProfilesResponse (..),
    newListMissionProfilesResponse,

    -- * Response Lenses
    listMissionProfilesResponse_missionProfileList,
    listMissionProfilesResponse_nextToken,
    listMissionProfilesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newListMissionProfiles' smart constructor.
data ListMissionProfiles = ListMissionProfiles'
  { -- | Maximum number of mission profiles returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Next token returned in the request of a previous @ListMissionProfiles@
    -- call. Used to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'listMissionProfiles_maxResults' - Maximum number of mission profiles returned.
--
-- 'nextToken', 'listMissionProfiles_nextToken' - Next token returned in the request of a previous @ListMissionProfiles@
-- call. Used to get the next page of results.
newListMissionProfiles ::
  ListMissionProfiles
newListMissionProfiles =
  ListMissionProfiles'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Maximum number of mission profiles returned.
listMissionProfiles_maxResults :: Lens.Lens' ListMissionProfiles (Prelude.Maybe Prelude.Natural)
listMissionProfiles_maxResults = Lens.lens (\ListMissionProfiles' {maxResults} -> maxResults) (\s@ListMissionProfiles' {} a -> s {maxResults = a} :: ListMissionProfiles)

-- | Next token returned in the request of a previous @ListMissionProfiles@
-- call. Used to get the next page of results.
listMissionProfiles_nextToken :: Lens.Lens' ListMissionProfiles (Prelude.Maybe Prelude.Text)
listMissionProfiles_nextToken = Lens.lens (\ListMissionProfiles' {nextToken} -> nextToken) (\s@ListMissionProfiles' {} a -> s {nextToken = a} :: ListMissionProfiles)

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
        Prelude.Just
          Prelude.$ rq
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
            Prelude.<$> ( x
                            Data..?> "missionProfileList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMissionProfiles where
  hashWithSalt _salt ListMissionProfiles' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListMissionProfiles where
  rnf ListMissionProfiles' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListMissionProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListMissionProfiles where
  toPath = Prelude.const "/missionprofile"

instance Data.ToQuery ListMissionProfiles where
  toQuery ListMissionProfiles' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- |
--
-- /See:/ 'newListMissionProfilesResponse' smart constructor.
data ListMissionProfilesResponse = ListMissionProfilesResponse'
  { -- | List of mission profiles.
    missionProfileList :: Prelude.Maybe [MissionProfileListItem],
    -- | Next token returned in the response of a previous @ListMissionProfiles@
    -- call. Used to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'missionProfileList', 'listMissionProfilesResponse_missionProfileList' - List of mission profiles.
--
-- 'nextToken', 'listMissionProfilesResponse_nextToken' - Next token returned in the response of a previous @ListMissionProfiles@
-- call. Used to get the next page of results.
--
-- 'httpStatus', 'listMissionProfilesResponse_httpStatus' - The response's http status code.
newListMissionProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMissionProfilesResponse
newListMissionProfilesResponse pHttpStatus_ =
  ListMissionProfilesResponse'
    { missionProfileList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of mission profiles.
listMissionProfilesResponse_missionProfileList :: Lens.Lens' ListMissionProfilesResponse (Prelude.Maybe [MissionProfileListItem])
listMissionProfilesResponse_missionProfileList = Lens.lens (\ListMissionProfilesResponse' {missionProfileList} -> missionProfileList) (\s@ListMissionProfilesResponse' {} a -> s {missionProfileList = a} :: ListMissionProfilesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Next token returned in the response of a previous @ListMissionProfiles@
-- call. Used to get the next page of results.
listMissionProfilesResponse_nextToken :: Lens.Lens' ListMissionProfilesResponse (Prelude.Maybe Prelude.Text)
listMissionProfilesResponse_nextToken = Lens.lens (\ListMissionProfilesResponse' {nextToken} -> nextToken) (\s@ListMissionProfilesResponse' {} a -> s {nextToken = a} :: ListMissionProfilesResponse)

-- | The response's http status code.
listMissionProfilesResponse_httpStatus :: Lens.Lens' ListMissionProfilesResponse Prelude.Int
listMissionProfilesResponse_httpStatus = Lens.lens (\ListMissionProfilesResponse' {httpStatus} -> httpStatus) (\s@ListMissionProfilesResponse' {} a -> s {httpStatus = a} :: ListMissionProfilesResponse)

instance Prelude.NFData ListMissionProfilesResponse where
  rnf ListMissionProfilesResponse' {..} =
    Prelude.rnf missionProfileList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
