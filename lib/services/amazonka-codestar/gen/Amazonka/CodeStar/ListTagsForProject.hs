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
-- Module      : Amazonka.CodeStar.ListTagsForProject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the tags for a project.
module Amazonka.CodeStar.ListTagsForProject
  ( -- * Creating a Request
    ListTagsForProject (..),
    newListTagsForProject,

    -- * Request Lenses
    listTagsForProject_maxResults,
    listTagsForProject_nextToken,
    listTagsForProject_id,

    -- * Destructuring the Response
    ListTagsForProjectResponse (..),
    newListTagsForProjectResponse,

    -- * Response Lenses
    listTagsForProjectResponse_nextToken,
    listTagsForProjectResponse_tags,
    listTagsForProjectResponse_httpStatus,
  )
where

import Amazonka.CodeStar.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTagsForProject' smart constructor.
data ListTagsForProject = ListTagsForProject'
  { -- | Reserved for future use.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Reserved for future use.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the project to get tags for.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTagsForProject_maxResults' - Reserved for future use.
--
-- 'nextToken', 'listTagsForProject_nextToken' - Reserved for future use.
--
-- 'id', 'listTagsForProject_id' - The ID of the project to get tags for.
newListTagsForProject ::
  -- | 'id'
  Prelude.Text ->
  ListTagsForProject
newListTagsForProject pId_ =
  ListTagsForProject'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      id = pId_
    }

-- | Reserved for future use.
listTagsForProject_maxResults :: Lens.Lens' ListTagsForProject (Prelude.Maybe Prelude.Natural)
listTagsForProject_maxResults = Lens.lens (\ListTagsForProject' {maxResults} -> maxResults) (\s@ListTagsForProject' {} a -> s {maxResults = a} :: ListTagsForProject)

-- | Reserved for future use.
listTagsForProject_nextToken :: Lens.Lens' ListTagsForProject (Prelude.Maybe Prelude.Text)
listTagsForProject_nextToken = Lens.lens (\ListTagsForProject' {nextToken} -> nextToken) (\s@ListTagsForProject' {} a -> s {nextToken = a} :: ListTagsForProject)

-- | The ID of the project to get tags for.
listTagsForProject_id :: Lens.Lens' ListTagsForProject Prelude.Text
listTagsForProject_id = Lens.lens (\ListTagsForProject' {id} -> id) (\s@ListTagsForProject' {} a -> s {id = a} :: ListTagsForProject)

instance Core.AWSRequest ListTagsForProject where
  type
    AWSResponse ListTagsForProject =
      ListTagsForProjectResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForProjectResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTagsForProject where
  hashWithSalt _salt ListTagsForProject' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` id

instance Prelude.NFData ListTagsForProject where
  rnf ListTagsForProject' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders ListTagsForProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeStar_20170419.ListTagsForProject" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTagsForProject where
  toJSON ListTagsForProject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("id" Data..= id)
          ]
      )

instance Data.ToPath ListTagsForProject where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTagsForProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTagsForProjectResponse' smart constructor.
data ListTagsForProjectResponse = ListTagsForProjectResponse'
  { -- | Reserved for future use.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The tags for the project.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTagsForProjectResponse_nextToken' - Reserved for future use.
--
-- 'tags', 'listTagsForProjectResponse_tags' - The tags for the project.
--
-- 'httpStatus', 'listTagsForProjectResponse_httpStatus' - The response's http status code.
newListTagsForProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTagsForProjectResponse
newListTagsForProjectResponse pHttpStatus_ =
  ListTagsForProjectResponse'
    { nextToken =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Reserved for future use.
listTagsForProjectResponse_nextToken :: Lens.Lens' ListTagsForProjectResponse (Prelude.Maybe Prelude.Text)
listTagsForProjectResponse_nextToken = Lens.lens (\ListTagsForProjectResponse' {nextToken} -> nextToken) (\s@ListTagsForProjectResponse' {} a -> s {nextToken = a} :: ListTagsForProjectResponse)

-- | The tags for the project.
listTagsForProjectResponse_tags :: Lens.Lens' ListTagsForProjectResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listTagsForProjectResponse_tags = Lens.lens (\ListTagsForProjectResponse' {tags} -> tags) (\s@ListTagsForProjectResponse' {} a -> s {tags = a} :: ListTagsForProjectResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTagsForProjectResponse_httpStatus :: Lens.Lens' ListTagsForProjectResponse Prelude.Int
listTagsForProjectResponse_httpStatus = Lens.lens (\ListTagsForProjectResponse' {httpStatus} -> httpStatus) (\s@ListTagsForProjectResponse' {} a -> s {httpStatus = a} :: ListTagsForProjectResponse)

instance Prelude.NFData ListTagsForProjectResponse where
  rnf ListTagsForProjectResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
