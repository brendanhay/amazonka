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
-- Module      : Amazonka.IoT1ClickProjects.ListPlacements
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the placement(s) of a project.
--
-- This operation returns paginated results.
module Amazonka.IoT1ClickProjects.ListPlacements
  ( -- * Creating a Request
    ListPlacements (..),
    newListPlacements,

    -- * Request Lenses
    listPlacements_nextToken,
    listPlacements_maxResults,
    listPlacements_projectName,

    -- * Destructuring the Response
    ListPlacementsResponse (..),
    newListPlacementsResponse,

    -- * Response Lenses
    listPlacementsResponse_nextToken,
    listPlacementsResponse_httpStatus,
    listPlacementsResponse_placements,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT1ClickProjects.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPlacements' smart constructor.
data ListPlacements = ListPlacements'
  { -- | The token to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per request. If not set, a
    -- default value of 100 is used.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The project containing the placements to be listed.
    projectName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPlacements' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPlacements_nextToken' - The token to retrieve the next set of results.
--
-- 'maxResults', 'listPlacements_maxResults' - The maximum number of results to return per request. If not set, a
-- default value of 100 is used.
--
-- 'projectName', 'listPlacements_projectName' - The project containing the placements to be listed.
newListPlacements ::
  -- | 'projectName'
  Prelude.Text ->
  ListPlacements
newListPlacements pProjectName_ =
  ListPlacements'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      projectName = pProjectName_
    }

-- | The token to retrieve the next set of results.
listPlacements_nextToken :: Lens.Lens' ListPlacements (Prelude.Maybe Prelude.Text)
listPlacements_nextToken = Lens.lens (\ListPlacements' {nextToken} -> nextToken) (\s@ListPlacements' {} a -> s {nextToken = a} :: ListPlacements)

-- | The maximum number of results to return per request. If not set, a
-- default value of 100 is used.
listPlacements_maxResults :: Lens.Lens' ListPlacements (Prelude.Maybe Prelude.Natural)
listPlacements_maxResults = Lens.lens (\ListPlacements' {maxResults} -> maxResults) (\s@ListPlacements' {} a -> s {maxResults = a} :: ListPlacements)

-- | The project containing the placements to be listed.
listPlacements_projectName :: Lens.Lens' ListPlacements Prelude.Text
listPlacements_projectName = Lens.lens (\ListPlacements' {projectName} -> projectName) (\s@ListPlacements' {} a -> s {projectName = a} :: ListPlacements)

instance Core.AWSPager ListPlacements where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPlacementsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listPlacementsResponse_placements) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPlacements_nextToken
          Lens..~ rs
          Lens.^? listPlacementsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListPlacements where
  type
    AWSResponse ListPlacements =
      ListPlacementsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPlacementsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "placements" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListPlacements where
  hashWithSalt _salt ListPlacements' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` projectName

instance Prelude.NFData ListPlacements where
  rnf ListPlacements' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf projectName

instance Core.ToHeaders ListPlacements where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListPlacements where
  toPath ListPlacements' {..} =
    Prelude.mconcat
      ["/projects/", Core.toBS projectName, "/placements"]

instance Core.ToQuery ListPlacements where
  toQuery ListPlacements' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListPlacementsResponse' smart constructor.
data ListPlacementsResponse = ListPlacementsResponse'
  { -- | The token used to retrieve the next set of results - will be effectively
    -- empty if there are no further results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An object listing the requested placements.
    placements :: [PlacementSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPlacementsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPlacementsResponse_nextToken' - The token used to retrieve the next set of results - will be effectively
-- empty if there are no further results.
--
-- 'httpStatus', 'listPlacementsResponse_httpStatus' - The response's http status code.
--
-- 'placements', 'listPlacementsResponse_placements' - An object listing the requested placements.
newListPlacementsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPlacementsResponse
newListPlacementsResponse pHttpStatus_ =
  ListPlacementsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      placements = Prelude.mempty
    }

-- | The token used to retrieve the next set of results - will be effectively
-- empty if there are no further results.
listPlacementsResponse_nextToken :: Lens.Lens' ListPlacementsResponse (Prelude.Maybe Prelude.Text)
listPlacementsResponse_nextToken = Lens.lens (\ListPlacementsResponse' {nextToken} -> nextToken) (\s@ListPlacementsResponse' {} a -> s {nextToken = a} :: ListPlacementsResponse)

-- | The response's http status code.
listPlacementsResponse_httpStatus :: Lens.Lens' ListPlacementsResponse Prelude.Int
listPlacementsResponse_httpStatus = Lens.lens (\ListPlacementsResponse' {httpStatus} -> httpStatus) (\s@ListPlacementsResponse' {} a -> s {httpStatus = a} :: ListPlacementsResponse)

-- | An object listing the requested placements.
listPlacementsResponse_placements :: Lens.Lens' ListPlacementsResponse [PlacementSummary]
listPlacementsResponse_placements = Lens.lens (\ListPlacementsResponse' {placements} -> placements) (\s@ListPlacementsResponse' {} a -> s {placements = a} :: ListPlacementsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListPlacementsResponse where
  rnf ListPlacementsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf placements
