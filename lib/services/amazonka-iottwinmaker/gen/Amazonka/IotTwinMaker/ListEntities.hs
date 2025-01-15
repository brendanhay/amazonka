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
-- Module      : Amazonka.IotTwinMaker.ListEntities
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all entities in a workspace.
module Amazonka.IotTwinMaker.ListEntities
  ( -- * Creating a Request
    ListEntities (..),
    newListEntities,

    -- * Request Lenses
    listEntities_filters,
    listEntities_maxResults,
    listEntities_nextToken,
    listEntities_workspaceId,

    -- * Destructuring the Response
    ListEntitiesResponse (..),
    newListEntitiesResponse,

    -- * Response Lenses
    listEntitiesResponse_entitySummaries,
    listEntitiesResponse_nextToken,
    listEntitiesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEntities' smart constructor.
data ListEntities = ListEntities'
  { -- | A list of objects that filter the request.
    --
    -- Only one object is accepted as a valid input.
    filters :: Prelude.Maybe [ListEntitiesFilter],
    -- | The maximum number of results to return at one time. The default is 25.
    --
    -- Valid Range: Minimum value of 1. Maximum value of 250.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The string that specifies the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the workspace.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEntities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listEntities_filters' - A list of objects that filter the request.
--
-- Only one object is accepted as a valid input.
--
-- 'maxResults', 'listEntities_maxResults' - The maximum number of results to return at one time. The default is 25.
--
-- Valid Range: Minimum value of 1. Maximum value of 250.
--
-- 'nextToken', 'listEntities_nextToken' - The string that specifies the next page of results.
--
-- 'workspaceId', 'listEntities_workspaceId' - The ID of the workspace.
newListEntities ::
  -- | 'workspaceId'
  Prelude.Text ->
  ListEntities
newListEntities pWorkspaceId_ =
  ListEntities'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      workspaceId = pWorkspaceId_
    }

-- | A list of objects that filter the request.
--
-- Only one object is accepted as a valid input.
listEntities_filters :: Lens.Lens' ListEntities (Prelude.Maybe [ListEntitiesFilter])
listEntities_filters = Lens.lens (\ListEntities' {filters} -> filters) (\s@ListEntities' {} a -> s {filters = a} :: ListEntities) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return at one time. The default is 25.
--
-- Valid Range: Minimum value of 1. Maximum value of 250.
listEntities_maxResults :: Lens.Lens' ListEntities (Prelude.Maybe Prelude.Natural)
listEntities_maxResults = Lens.lens (\ListEntities' {maxResults} -> maxResults) (\s@ListEntities' {} a -> s {maxResults = a} :: ListEntities)

-- | The string that specifies the next page of results.
listEntities_nextToken :: Lens.Lens' ListEntities (Prelude.Maybe Prelude.Text)
listEntities_nextToken = Lens.lens (\ListEntities' {nextToken} -> nextToken) (\s@ListEntities' {} a -> s {nextToken = a} :: ListEntities)

-- | The ID of the workspace.
listEntities_workspaceId :: Lens.Lens' ListEntities Prelude.Text
listEntities_workspaceId = Lens.lens (\ListEntities' {workspaceId} -> workspaceId) (\s@ListEntities' {} a -> s {workspaceId = a} :: ListEntities)

instance Core.AWSRequest ListEntities where
  type AWSResponse ListEntities = ListEntitiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEntitiesResponse'
            Prelude.<$> ( x
                            Data..?> "entitySummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEntities where
  hashWithSalt _salt ListEntities' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData ListEntities where
  rnf ListEntities' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf workspaceId

instance Data.ToHeaders ListEntities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEntities where
  toJSON ListEntities' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListEntities where
  toPath ListEntities' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/entities-list"
      ]

instance Data.ToQuery ListEntities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEntitiesResponse' smart constructor.
data ListEntitiesResponse = ListEntitiesResponse'
  { -- | A list of objects that contain information about the entities.
    entitySummaries :: Prelude.Maybe [EntitySummary],
    -- | The string that specifies the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEntitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entitySummaries', 'listEntitiesResponse_entitySummaries' - A list of objects that contain information about the entities.
--
-- 'nextToken', 'listEntitiesResponse_nextToken' - The string that specifies the next page of results.
--
-- 'httpStatus', 'listEntitiesResponse_httpStatus' - The response's http status code.
newListEntitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEntitiesResponse
newListEntitiesResponse pHttpStatus_ =
  ListEntitiesResponse'
    { entitySummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of objects that contain information about the entities.
listEntitiesResponse_entitySummaries :: Lens.Lens' ListEntitiesResponse (Prelude.Maybe [EntitySummary])
listEntitiesResponse_entitySummaries = Lens.lens (\ListEntitiesResponse' {entitySummaries} -> entitySummaries) (\s@ListEntitiesResponse' {} a -> s {entitySummaries = a} :: ListEntitiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The string that specifies the next page of results.
listEntitiesResponse_nextToken :: Lens.Lens' ListEntitiesResponse (Prelude.Maybe Prelude.Text)
listEntitiesResponse_nextToken = Lens.lens (\ListEntitiesResponse' {nextToken} -> nextToken) (\s@ListEntitiesResponse' {} a -> s {nextToken = a} :: ListEntitiesResponse)

-- | The response's http status code.
listEntitiesResponse_httpStatus :: Lens.Lens' ListEntitiesResponse Prelude.Int
listEntitiesResponse_httpStatus = Lens.lens (\ListEntitiesResponse' {httpStatus} -> httpStatus) (\s@ListEntitiesResponse' {} a -> s {httpStatus = a} :: ListEntitiesResponse)

instance Prelude.NFData ListEntitiesResponse where
  rnf ListEntitiesResponse' {..} =
    Prelude.rnf entitySummaries `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
