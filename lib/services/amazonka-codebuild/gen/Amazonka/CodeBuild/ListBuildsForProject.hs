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
-- Module      : Amazonka.CodeBuild.ListBuildsForProject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of build identifiers for the specified build project, with
-- each build identifier representing a single build.
--
-- This operation returns paginated results.
module Amazonka.CodeBuild.ListBuildsForProject
  ( -- * Creating a Request
    ListBuildsForProject (..),
    newListBuildsForProject,

    -- * Request Lenses
    listBuildsForProject_sortOrder,
    listBuildsForProject_nextToken,
    listBuildsForProject_projectName,

    -- * Destructuring the Response
    ListBuildsForProjectResponse (..),
    newListBuildsForProjectResponse,

    -- * Response Lenses
    listBuildsForProjectResponse_nextToken,
    listBuildsForProjectResponse_ids,
    listBuildsForProjectResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBuildsForProject' smart constructor.
data ListBuildsForProject = ListBuildsForProject'
  { -- | The order to sort the results in. The results are sorted by build
    -- number, not the build identifier. If this is not specified, the results
    -- are sorted in descending order.
    --
    -- Valid values include:
    --
    -- -   @ASCENDING@: List the build identifiers in ascending order, by build
    --     number.
    --
    -- -   @DESCENDING@: List the build identifiers in descending order, by
    --     build number.
    --
    -- If the project has more than 100 builds, setting the sort order will
    -- result in an error.
    sortOrder :: Prelude.Maybe SortOrderType,
    -- | During a previous call, if there are more than 100 items in the list,
    -- only the first 100 items are returned, along with a unique string called
    -- a /nextToken/. To get the next batch of items in the list, call this
    -- operation again, adding the next token to the call. To get all of the
    -- items in the list, keep calling this operation with each subsequent next
    -- token that is returned, until no more next tokens are returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the CodeBuild project.
    projectName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBuildsForProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listBuildsForProject_sortOrder' - The order to sort the results in. The results are sorted by build
-- number, not the build identifier. If this is not specified, the results
-- are sorted in descending order.
--
-- Valid values include:
--
-- -   @ASCENDING@: List the build identifiers in ascending order, by build
--     number.
--
-- -   @DESCENDING@: List the build identifiers in descending order, by
--     build number.
--
-- If the project has more than 100 builds, setting the sort order will
-- result in an error.
--
-- 'nextToken', 'listBuildsForProject_nextToken' - During a previous call, if there are more than 100 items in the list,
-- only the first 100 items are returned, along with a unique string called
-- a /nextToken/. To get the next batch of items in the list, call this
-- operation again, adding the next token to the call. To get all of the
-- items in the list, keep calling this operation with each subsequent next
-- token that is returned, until no more next tokens are returned.
--
-- 'projectName', 'listBuildsForProject_projectName' - The name of the CodeBuild project.
newListBuildsForProject ::
  -- | 'projectName'
  Prelude.Text ->
  ListBuildsForProject
newListBuildsForProject pProjectName_ =
  ListBuildsForProject'
    { sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      projectName = pProjectName_
    }

-- | The order to sort the results in. The results are sorted by build
-- number, not the build identifier. If this is not specified, the results
-- are sorted in descending order.
--
-- Valid values include:
--
-- -   @ASCENDING@: List the build identifiers in ascending order, by build
--     number.
--
-- -   @DESCENDING@: List the build identifiers in descending order, by
--     build number.
--
-- If the project has more than 100 builds, setting the sort order will
-- result in an error.
listBuildsForProject_sortOrder :: Lens.Lens' ListBuildsForProject (Prelude.Maybe SortOrderType)
listBuildsForProject_sortOrder = Lens.lens (\ListBuildsForProject' {sortOrder} -> sortOrder) (\s@ListBuildsForProject' {} a -> s {sortOrder = a} :: ListBuildsForProject)

-- | During a previous call, if there are more than 100 items in the list,
-- only the first 100 items are returned, along with a unique string called
-- a /nextToken/. To get the next batch of items in the list, call this
-- operation again, adding the next token to the call. To get all of the
-- items in the list, keep calling this operation with each subsequent next
-- token that is returned, until no more next tokens are returned.
listBuildsForProject_nextToken :: Lens.Lens' ListBuildsForProject (Prelude.Maybe Prelude.Text)
listBuildsForProject_nextToken = Lens.lens (\ListBuildsForProject' {nextToken} -> nextToken) (\s@ListBuildsForProject' {} a -> s {nextToken = a} :: ListBuildsForProject)

-- | The name of the CodeBuild project.
listBuildsForProject_projectName :: Lens.Lens' ListBuildsForProject Prelude.Text
listBuildsForProject_projectName = Lens.lens (\ListBuildsForProject' {projectName} -> projectName) (\s@ListBuildsForProject' {} a -> s {projectName = a} :: ListBuildsForProject)

instance Core.AWSPager ListBuildsForProject where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBuildsForProjectResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBuildsForProjectResponse_ids Prelude.. Lens._Just
              Prelude.. Lens.to Prelude.toList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listBuildsForProject_nextToken
          Lens..~ rs
          Lens.^? listBuildsForProjectResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListBuildsForProject where
  type
    AWSResponse ListBuildsForProject =
      ListBuildsForProjectResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBuildsForProjectResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "ids")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBuildsForProject where
  hashWithSalt _salt ListBuildsForProject' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` projectName

instance Prelude.NFData ListBuildsForProject where
  rnf ListBuildsForProject' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf projectName

instance Data.ToHeaders ListBuildsForProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.ListBuildsForProject" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListBuildsForProject where
  toJSON ListBuildsForProject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sortOrder" Data..=) Prelude.<$> sortOrder,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("projectName" Data..= projectName)
          ]
      )

instance Data.ToPath ListBuildsForProject where
  toPath = Prelude.const "/"

instance Data.ToQuery ListBuildsForProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBuildsForProjectResponse' smart constructor.
data ListBuildsForProjectResponse = ListBuildsForProjectResponse'
  { -- | If there are more than 100 items in the list, only the first 100 items
    -- are returned, along with a unique string called a /nextToken/. To get
    -- the next batch of items in the list, call this operation again, adding
    -- the next token to the call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of build identifiers for the specified build project, with each
    -- build ID representing a single build.
    ids :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBuildsForProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBuildsForProjectResponse_nextToken' - If there are more than 100 items in the list, only the first 100 items
-- are returned, along with a unique string called a /nextToken/. To get
-- the next batch of items in the list, call this operation again, adding
-- the next token to the call.
--
-- 'ids', 'listBuildsForProjectResponse_ids' - A list of build identifiers for the specified build project, with each
-- build ID representing a single build.
--
-- 'httpStatus', 'listBuildsForProjectResponse_httpStatus' - The response's http status code.
newListBuildsForProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBuildsForProjectResponse
newListBuildsForProjectResponse pHttpStatus_ =
  ListBuildsForProjectResponse'
    { nextToken =
        Prelude.Nothing,
      ids = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are more than 100 items in the list, only the first 100 items
-- are returned, along with a unique string called a /nextToken/. To get
-- the next batch of items in the list, call this operation again, adding
-- the next token to the call.
listBuildsForProjectResponse_nextToken :: Lens.Lens' ListBuildsForProjectResponse (Prelude.Maybe Prelude.Text)
listBuildsForProjectResponse_nextToken = Lens.lens (\ListBuildsForProjectResponse' {nextToken} -> nextToken) (\s@ListBuildsForProjectResponse' {} a -> s {nextToken = a} :: ListBuildsForProjectResponse)

-- | A list of build identifiers for the specified build project, with each
-- build ID representing a single build.
listBuildsForProjectResponse_ids :: Lens.Lens' ListBuildsForProjectResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listBuildsForProjectResponse_ids = Lens.lens (\ListBuildsForProjectResponse' {ids} -> ids) (\s@ListBuildsForProjectResponse' {} a -> s {ids = a} :: ListBuildsForProjectResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listBuildsForProjectResponse_httpStatus :: Lens.Lens' ListBuildsForProjectResponse Prelude.Int
listBuildsForProjectResponse_httpStatus = Lens.lens (\ListBuildsForProjectResponse' {httpStatus} -> httpStatus) (\s@ListBuildsForProjectResponse' {} a -> s {httpStatus = a} :: ListBuildsForProjectResponse)

instance Prelude.NFData ListBuildsForProjectResponse where
  rnf ListBuildsForProjectResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf ids
      `Prelude.seq` Prelude.rnf httpStatus
