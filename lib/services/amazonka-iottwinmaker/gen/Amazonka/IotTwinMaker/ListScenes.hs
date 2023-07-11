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
-- Module      : Amazonka.IotTwinMaker.ListScenes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all scenes in a workspace.
module Amazonka.IotTwinMaker.ListScenes
  ( -- * Creating a Request
    ListScenes (..),
    newListScenes,

    -- * Request Lenses
    listScenes_maxResults,
    listScenes_nextToken,
    listScenes_workspaceId,

    -- * Destructuring the Response
    ListScenesResponse (..),
    newListScenesResponse,

    -- * Response Lenses
    listScenesResponse_nextToken,
    listScenesResponse_sceneSummaries,
    listScenesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListScenes' smart constructor.
data ListScenes = ListScenes'
  { -- | Specifies the maximum number of results to display.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The string that specifies the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the workspace that contains the scenes.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListScenes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listScenes_maxResults' - Specifies the maximum number of results to display.
--
-- 'nextToken', 'listScenes_nextToken' - The string that specifies the next page of results.
--
-- 'workspaceId', 'listScenes_workspaceId' - The ID of the workspace that contains the scenes.
newListScenes ::
  -- | 'workspaceId'
  Prelude.Text ->
  ListScenes
newListScenes pWorkspaceId_ =
  ListScenes'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      workspaceId = pWorkspaceId_
    }

-- | Specifies the maximum number of results to display.
listScenes_maxResults :: Lens.Lens' ListScenes (Prelude.Maybe Prelude.Natural)
listScenes_maxResults = Lens.lens (\ListScenes' {maxResults} -> maxResults) (\s@ListScenes' {} a -> s {maxResults = a} :: ListScenes)

-- | The string that specifies the next page of results.
listScenes_nextToken :: Lens.Lens' ListScenes (Prelude.Maybe Prelude.Text)
listScenes_nextToken = Lens.lens (\ListScenes' {nextToken} -> nextToken) (\s@ListScenes' {} a -> s {nextToken = a} :: ListScenes)

-- | The ID of the workspace that contains the scenes.
listScenes_workspaceId :: Lens.Lens' ListScenes Prelude.Text
listScenes_workspaceId = Lens.lens (\ListScenes' {workspaceId} -> workspaceId) (\s@ListScenes' {} a -> s {workspaceId = a} :: ListScenes)

instance Core.AWSRequest ListScenes where
  type AWSResponse ListScenes = ListScenesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListScenesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "sceneSummaries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListScenes where
  hashWithSalt _salt ListScenes' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData ListScenes where
  rnf ListScenes' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workspaceId

instance Data.ToHeaders ListScenes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListScenes where
  toJSON ListScenes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListScenes where
  toPath ListScenes' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/scenes-list"
      ]

instance Data.ToQuery ListScenes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListScenesResponse' smart constructor.
data ListScenesResponse = ListScenesResponse'
  { -- | The string that specifies the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of objects that contain information about the scenes.
    sceneSummaries :: Prelude.Maybe [SceneSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListScenesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listScenesResponse_nextToken' - The string that specifies the next page of results.
--
-- 'sceneSummaries', 'listScenesResponse_sceneSummaries' - A list of objects that contain information about the scenes.
--
-- 'httpStatus', 'listScenesResponse_httpStatus' - The response's http status code.
newListScenesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListScenesResponse
newListScenesResponse pHttpStatus_ =
  ListScenesResponse'
    { nextToken = Prelude.Nothing,
      sceneSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string that specifies the next page of results.
listScenesResponse_nextToken :: Lens.Lens' ListScenesResponse (Prelude.Maybe Prelude.Text)
listScenesResponse_nextToken = Lens.lens (\ListScenesResponse' {nextToken} -> nextToken) (\s@ListScenesResponse' {} a -> s {nextToken = a} :: ListScenesResponse)

-- | A list of objects that contain information about the scenes.
listScenesResponse_sceneSummaries :: Lens.Lens' ListScenesResponse (Prelude.Maybe [SceneSummary])
listScenesResponse_sceneSummaries = Lens.lens (\ListScenesResponse' {sceneSummaries} -> sceneSummaries) (\s@ListScenesResponse' {} a -> s {sceneSummaries = a} :: ListScenesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listScenesResponse_httpStatus :: Lens.Lens' ListScenesResponse Prelude.Int
listScenesResponse_httpStatus = Lens.lens (\ListScenesResponse' {httpStatus} -> httpStatus) (\s@ListScenesResponse' {} a -> s {httpStatus = a} :: ListScenesResponse)

instance Prelude.NFData ListScenesResponse where
  rnf ListScenesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sceneSummaries
      `Prelude.seq` Prelude.rnf httpStatus
