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
-- Module      : Amazonka.CloudDirectory.ListDirectories
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists directories created within an account.
--
-- This operation returns paginated results.
module Amazonka.CloudDirectory.ListDirectories
  ( -- * Creating a Request
    ListDirectories (..),
    newListDirectories,

    -- * Request Lenses
    listDirectories_nextToken,
    listDirectories_state,
    listDirectories_maxResults,

    -- * Destructuring the Response
    ListDirectoriesResponse (..),
    newListDirectoriesResponse,

    -- * Response Lenses
    listDirectoriesResponse_nextToken,
    listDirectoriesResponse_httpStatus,
    listDirectoriesResponse_directories,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDirectories' smart constructor.
data ListDirectories = ListDirectories'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The state of the directories in the list. Can be either Enabled,
    -- Disabled, or Deleted.
    state :: Prelude.Maybe DirectoryState,
    -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDirectories' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDirectories_nextToken' - The pagination token.
--
-- 'state', 'listDirectories_state' - The state of the directories in the list. Can be either Enabled,
-- Disabled, or Deleted.
--
-- 'maxResults', 'listDirectories_maxResults' - The maximum number of results to retrieve.
newListDirectories ::
  ListDirectories
newListDirectories =
  ListDirectories'
    { nextToken = Prelude.Nothing,
      state = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token.
listDirectories_nextToken :: Lens.Lens' ListDirectories (Prelude.Maybe Prelude.Text)
listDirectories_nextToken = Lens.lens (\ListDirectories' {nextToken} -> nextToken) (\s@ListDirectories' {} a -> s {nextToken = a} :: ListDirectories)

-- | The state of the directories in the list. Can be either Enabled,
-- Disabled, or Deleted.
listDirectories_state :: Lens.Lens' ListDirectories (Prelude.Maybe DirectoryState)
listDirectories_state = Lens.lens (\ListDirectories' {state} -> state) (\s@ListDirectories' {} a -> s {state = a} :: ListDirectories)

-- | The maximum number of results to retrieve.
listDirectories_maxResults :: Lens.Lens' ListDirectories (Prelude.Maybe Prelude.Natural)
listDirectories_maxResults = Lens.lens (\ListDirectories' {maxResults} -> maxResults) (\s@ListDirectories' {} a -> s {maxResults = a} :: ListDirectories)

instance Core.AWSPager ListDirectories where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDirectoriesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listDirectoriesResponse_directories) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDirectories_nextToken
          Lens..~ rs
          Lens.^? listDirectoriesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDirectories where
  type
    AWSResponse ListDirectories =
      ListDirectoriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDirectoriesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "Directories" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListDirectories where
  hashWithSalt _salt ListDirectories' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListDirectories where
  rnf ListDirectories' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListDirectories where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON ListDirectories where
  toJSON ListDirectories' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("state" Core..=) Prelude.<$> state,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListDirectories where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/directory/list"

instance Core.ToQuery ListDirectories where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDirectoriesResponse' smart constructor.
data ListDirectoriesResponse = ListDirectoriesResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Lists all directories that are associated with your account in
    -- pagination fashion.
    directories :: [Directory]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDirectoriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDirectoriesResponse_nextToken' - The pagination token.
--
-- 'httpStatus', 'listDirectoriesResponse_httpStatus' - The response's http status code.
--
-- 'directories', 'listDirectoriesResponse_directories' - Lists all directories that are associated with your account in
-- pagination fashion.
newListDirectoriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDirectoriesResponse
newListDirectoriesResponse pHttpStatus_ =
  ListDirectoriesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      directories = Prelude.mempty
    }

-- | The pagination token.
listDirectoriesResponse_nextToken :: Lens.Lens' ListDirectoriesResponse (Prelude.Maybe Prelude.Text)
listDirectoriesResponse_nextToken = Lens.lens (\ListDirectoriesResponse' {nextToken} -> nextToken) (\s@ListDirectoriesResponse' {} a -> s {nextToken = a} :: ListDirectoriesResponse)

-- | The response's http status code.
listDirectoriesResponse_httpStatus :: Lens.Lens' ListDirectoriesResponse Prelude.Int
listDirectoriesResponse_httpStatus = Lens.lens (\ListDirectoriesResponse' {httpStatus} -> httpStatus) (\s@ListDirectoriesResponse' {} a -> s {httpStatus = a} :: ListDirectoriesResponse)

-- | Lists all directories that are associated with your account in
-- pagination fashion.
listDirectoriesResponse_directories :: Lens.Lens' ListDirectoriesResponse [Directory]
listDirectoriesResponse_directories = Lens.lens (\ListDirectoriesResponse' {directories} -> directories) (\s@ListDirectoriesResponse' {} a -> s {directories = a} :: ListDirectoriesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListDirectoriesResponse where
  rnf ListDirectoriesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf directories
