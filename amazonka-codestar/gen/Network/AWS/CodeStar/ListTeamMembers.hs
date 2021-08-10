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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTeamMembers' smart constructor.
data ListTeamMembers = ListTeamMembers'
  { -- | The continuation token for the next set of results, if the results
    -- cannot be returned in one response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of team members you want returned in a response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the project for which you want to list team members.
    projectId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListTeamMembers
newListTeamMembers pProjectId_ =
  ListTeamMembers'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      projectId = pProjectId_
    }

-- | The continuation token for the next set of results, if the results
-- cannot be returned in one response.
listTeamMembers_nextToken :: Lens.Lens' ListTeamMembers (Prelude.Maybe Prelude.Text)
listTeamMembers_nextToken = Lens.lens (\ListTeamMembers' {nextToken} -> nextToken) (\s@ListTeamMembers' {} a -> s {nextToken = a} :: ListTeamMembers)

-- | The maximum number of team members you want returned in a response.
listTeamMembers_maxResults :: Lens.Lens' ListTeamMembers (Prelude.Maybe Prelude.Natural)
listTeamMembers_maxResults = Lens.lens (\ListTeamMembers' {maxResults} -> maxResults) (\s@ListTeamMembers' {} a -> s {maxResults = a} :: ListTeamMembers)

-- | The ID of the project for which you want to list team members.
listTeamMembers_projectId :: Lens.Lens' ListTeamMembers Prelude.Text
listTeamMembers_projectId = Lens.lens (\ListTeamMembers' {projectId} -> projectId) (\s@ListTeamMembers' {} a -> s {projectId = a} :: ListTeamMembers)

instance Core.AWSPager ListTeamMembers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTeamMembersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listTeamMembersResponse_teamMembers) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTeamMembers_nextToken
          Lens..~ rs
          Lens.^? listTeamMembersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListTeamMembers where
  type
    AWSResponse ListTeamMembers =
      ListTeamMembersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTeamMembersResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "teamMembers" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListTeamMembers

instance Prelude.NFData ListTeamMembers

instance Core.ToHeaders ListTeamMembers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeStar_20170419.ListTeamMembers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTeamMembers where
  toJSON ListTeamMembers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("projectId" Core..= projectId)
          ]
      )

instance Core.ToPath ListTeamMembers where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTeamMembers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTeamMembersResponse' smart constructor.
data ListTeamMembersResponse = ListTeamMembersResponse'
  { -- | The continuation token to use when requesting the next set of results,
    -- if there are more results to be returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of team member objects for the project.
    teamMembers :: [TeamMember]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListTeamMembersResponse
newListTeamMembersResponse pHttpStatus_ =
  ListTeamMembersResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      teamMembers = Prelude.mempty
    }

-- | The continuation token to use when requesting the next set of results,
-- if there are more results to be returned.
listTeamMembersResponse_nextToken :: Lens.Lens' ListTeamMembersResponse (Prelude.Maybe Prelude.Text)
listTeamMembersResponse_nextToken = Lens.lens (\ListTeamMembersResponse' {nextToken} -> nextToken) (\s@ListTeamMembersResponse' {} a -> s {nextToken = a} :: ListTeamMembersResponse)

-- | The response's http status code.
listTeamMembersResponse_httpStatus :: Lens.Lens' ListTeamMembersResponse Prelude.Int
listTeamMembersResponse_httpStatus = Lens.lens (\ListTeamMembersResponse' {httpStatus} -> httpStatus) (\s@ListTeamMembersResponse' {} a -> s {httpStatus = a} :: ListTeamMembersResponse)

-- | A list of team member objects for the project.
listTeamMembersResponse_teamMembers :: Lens.Lens' ListTeamMembersResponse [TeamMember]
listTeamMembersResponse_teamMembers = Lens.lens (\ListTeamMembersResponse' {teamMembers} -> teamMembers) (\s@ListTeamMembersResponse' {} a -> s {teamMembers = a} :: ListTeamMembersResponse) Prelude.. Lens._Coerce

instance Prelude.NFData ListTeamMembersResponse
