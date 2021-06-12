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
-- Module      : Network.AWS.CodeStar.ListTeamMembers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all team members associated with a project.
--
-- This operation returns paginated results.
module Network.AWS.CodeStar.ListTeamMembers
  ( -- * Creating a Request
    ListTeamMembers (..),
    newListTeamMembers,

    -- * Request Lenses
    listTeamMembers_nextToken,
    listTeamMembers_maxResults,
    listTeamMembers_projectId,

    -- * Destructuring the Response
    ListTeamMembersResponse (..),
    newListTeamMembersResponse,

    -- * Response Lenses
    listTeamMembersResponse_nextToken,
    listTeamMembersResponse_httpStatus,
    listTeamMembersResponse_teamMembers,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTeamMembers' smart constructor.
data ListTeamMembers = ListTeamMembers'
  { -- | The continuation token for the next set of results, if the results
    -- cannot be returned in one response.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of team members you want returned in a response.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ID of the project for which you want to list team members.
    projectId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTeamMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTeamMembers_nextToken' - The continuation token for the next set of results, if the results
-- cannot be returned in one response.
--
-- 'maxResults', 'listTeamMembers_maxResults' - The maximum number of team members you want returned in a response.
--
-- 'projectId', 'listTeamMembers_projectId' - The ID of the project for which you want to list team members.
newListTeamMembers ::
  -- | 'projectId'
  Core.Text ->
  ListTeamMembers
newListTeamMembers pProjectId_ =
  ListTeamMembers'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      projectId = pProjectId_
    }

-- | The continuation token for the next set of results, if the results
-- cannot be returned in one response.
listTeamMembers_nextToken :: Lens.Lens' ListTeamMembers (Core.Maybe Core.Text)
listTeamMembers_nextToken = Lens.lens (\ListTeamMembers' {nextToken} -> nextToken) (\s@ListTeamMembers' {} a -> s {nextToken = a} :: ListTeamMembers)

-- | The maximum number of team members you want returned in a response.
listTeamMembers_maxResults :: Lens.Lens' ListTeamMembers (Core.Maybe Core.Natural)
listTeamMembers_maxResults = Lens.lens (\ListTeamMembers' {maxResults} -> maxResults) (\s@ListTeamMembers' {} a -> s {maxResults = a} :: ListTeamMembers)

-- | The ID of the project for which you want to list team members.
listTeamMembers_projectId :: Lens.Lens' ListTeamMembers Core.Text
listTeamMembers_projectId = Lens.lens (\ListTeamMembers' {projectId} -> projectId) (\s@ListTeamMembers' {} a -> s {projectId = a} :: ListTeamMembers)

instance Core.AWSPager ListTeamMembers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTeamMembersResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^. listTeamMembersResponse_teamMembers) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listTeamMembers_nextToken
          Lens..~ rs
          Lens.^? listTeamMembersResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListTeamMembers where
  type
    AWSResponse ListTeamMembers =
      ListTeamMembersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTeamMembersResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "teamMembers" Core..!@ Core.mempty)
      )

instance Core.Hashable ListTeamMembers

instance Core.NFData ListTeamMembers

instance Core.ToHeaders ListTeamMembers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeStar_20170419.ListTeamMembers" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTeamMembers where
  toJSON ListTeamMembers' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            Core.Just ("projectId" Core..= projectId)
          ]
      )

instance Core.ToPath ListTeamMembers where
  toPath = Core.const "/"

instance Core.ToQuery ListTeamMembers where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTeamMembersResponse' smart constructor.
data ListTeamMembersResponse = ListTeamMembersResponse'
  { -- | The continuation token to use when requesting the next set of results,
    -- if there are more results to be returned.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of team member objects for the project.
    teamMembers :: [TeamMember]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTeamMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTeamMembersResponse_nextToken' - The continuation token to use when requesting the next set of results,
-- if there are more results to be returned.
--
-- 'httpStatus', 'listTeamMembersResponse_httpStatus' - The response's http status code.
--
-- 'teamMembers', 'listTeamMembersResponse_teamMembers' - A list of team member objects for the project.
newListTeamMembersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTeamMembersResponse
newListTeamMembersResponse pHttpStatus_ =
  ListTeamMembersResponse'
    { nextToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      teamMembers = Core.mempty
    }

-- | The continuation token to use when requesting the next set of results,
-- if there are more results to be returned.
listTeamMembersResponse_nextToken :: Lens.Lens' ListTeamMembersResponse (Core.Maybe Core.Text)
listTeamMembersResponse_nextToken = Lens.lens (\ListTeamMembersResponse' {nextToken} -> nextToken) (\s@ListTeamMembersResponse' {} a -> s {nextToken = a} :: ListTeamMembersResponse)

-- | The response's http status code.
listTeamMembersResponse_httpStatus :: Lens.Lens' ListTeamMembersResponse Core.Int
listTeamMembersResponse_httpStatus = Lens.lens (\ListTeamMembersResponse' {httpStatus} -> httpStatus) (\s@ListTeamMembersResponse' {} a -> s {httpStatus = a} :: ListTeamMembersResponse)

-- | A list of team member objects for the project.
listTeamMembersResponse_teamMembers :: Lens.Lens' ListTeamMembersResponse [TeamMember]
listTeamMembersResponse_teamMembers = Lens.lens (\ListTeamMembersResponse' {teamMembers} -> teamMembers) (\s@ListTeamMembersResponse' {} a -> s {teamMembers = a} :: ListTeamMembersResponse) Core.. Lens._Coerce

instance Core.NFData ListTeamMembersResponse
