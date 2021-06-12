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
-- Module      : Network.AWS.CodeStar.ListTagsForProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the tags for a project.
module Network.AWS.CodeStar.ListTagsForProject
  ( -- * Creating a Request
    ListTagsForProject (..),
    newListTagsForProject,

    -- * Request Lenses
    listTagsForProject_nextToken,
    listTagsForProject_maxResults,
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

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTagsForProject' smart constructor.
data ListTagsForProject = ListTagsForProject'
  { -- | Reserved for future use.
    nextToken :: Core.Maybe Core.Text,
    -- | Reserved for future use.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ID of the project to get tags for.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagsForProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTagsForProject_nextToken' - Reserved for future use.
--
-- 'maxResults', 'listTagsForProject_maxResults' - Reserved for future use.
--
-- 'id', 'listTagsForProject_id' - The ID of the project to get tags for.
newListTagsForProject ::
  -- | 'id'
  Core.Text ->
  ListTagsForProject
newListTagsForProject pId_ =
  ListTagsForProject'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      id = pId_
    }

-- | Reserved for future use.
listTagsForProject_nextToken :: Lens.Lens' ListTagsForProject (Core.Maybe Core.Text)
listTagsForProject_nextToken = Lens.lens (\ListTagsForProject' {nextToken} -> nextToken) (\s@ListTagsForProject' {} a -> s {nextToken = a} :: ListTagsForProject)

-- | Reserved for future use.
listTagsForProject_maxResults :: Lens.Lens' ListTagsForProject (Core.Maybe Core.Natural)
listTagsForProject_maxResults = Lens.lens (\ListTagsForProject' {maxResults} -> maxResults) (\s@ListTagsForProject' {} a -> s {maxResults = a} :: ListTagsForProject)

-- | The ID of the project to get tags for.
listTagsForProject_id :: Lens.Lens' ListTagsForProject Core.Text
listTagsForProject_id = Lens.lens (\ListTagsForProject' {id} -> id) (\s@ListTagsForProject' {} a -> s {id = a} :: ListTagsForProject)

instance Core.AWSRequest ListTagsForProject where
  type
    AWSResponse ListTagsForProject =
      ListTagsForProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForProjectResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTagsForProject

instance Core.NFData ListTagsForProject

instance Core.ToHeaders ListTagsForProject where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeStar_20170419.ListTagsForProject" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTagsForProject where
  toJSON ListTagsForProject' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            Core.Just ("id" Core..= id)
          ]
      )

instance Core.ToPath ListTagsForProject where
  toPath = Core.const "/"

instance Core.ToQuery ListTagsForProject where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTagsForProjectResponse' smart constructor.
data ListTagsForProjectResponse = ListTagsForProjectResponse'
  { -- | Reserved for future use.
    nextToken :: Core.Maybe Core.Text,
    -- | The tags for the project.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListTagsForProjectResponse
newListTagsForProjectResponse pHttpStatus_ =
  ListTagsForProjectResponse'
    { nextToken =
        Core.Nothing,
      tags = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Reserved for future use.
listTagsForProjectResponse_nextToken :: Lens.Lens' ListTagsForProjectResponse (Core.Maybe Core.Text)
listTagsForProjectResponse_nextToken = Lens.lens (\ListTagsForProjectResponse' {nextToken} -> nextToken) (\s@ListTagsForProjectResponse' {} a -> s {nextToken = a} :: ListTagsForProjectResponse)

-- | The tags for the project.
listTagsForProjectResponse_tags :: Lens.Lens' ListTagsForProjectResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
listTagsForProjectResponse_tags = Lens.lens (\ListTagsForProjectResponse' {tags} -> tags) (\s@ListTagsForProjectResponse' {} a -> s {tags = a} :: ListTagsForProjectResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTagsForProjectResponse_httpStatus :: Lens.Lens' ListTagsForProjectResponse Core.Int
listTagsForProjectResponse_httpStatus = Lens.lens (\ListTagsForProjectResponse' {httpStatus} -> httpStatus) (\s@ListTagsForProjectResponse' {} a -> s {httpStatus = a} :: ListTagsForProjectResponse)

instance Core.NFData ListTagsForProjectResponse
