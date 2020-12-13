{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.ListTeamMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all team members associated with a project.
--
-- This operation returns paginated results.
module Network.AWS.CodeStar.ListTeamMembers
  ( -- * Creating a request
    ListTeamMembers (..),
    mkListTeamMembers,

    -- ** Request lenses
    ltmNextToken,
    ltmProjectId,
    ltmMaxResults,

    -- * Destructuring the response
    ListTeamMembersResponse (..),
    mkListTeamMembersResponse,

    -- ** Response lenses
    ltmrsNextToken,
    ltmrsTeamMembers,
    ltmrsResponseStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTeamMembers' smart constructor.
data ListTeamMembers = ListTeamMembers'
  { -- | The continuation token for the next set of results, if the results cannot be returned in one response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the project for which you want to list team members.
    projectId :: Lude.Text,
    -- | The maximum number of team members you want returned in a response.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTeamMembers' with the minimum fields required to make a request.
--
-- * 'nextToken' - The continuation token for the next set of results, if the results cannot be returned in one response.
-- * 'projectId' - The ID of the project for which you want to list team members.
-- * 'maxResults' - The maximum number of team members you want returned in a response.
mkListTeamMembers ::
  -- | 'projectId'
  Lude.Text ->
  ListTeamMembers
mkListTeamMembers pProjectId_ =
  ListTeamMembers'
    { nextToken = Lude.Nothing,
      projectId = pProjectId_,
      maxResults = Lude.Nothing
    }

-- | The continuation token for the next set of results, if the results cannot be returned in one response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmNextToken :: Lens.Lens' ListTeamMembers (Lude.Maybe Lude.Text)
ltmNextToken = Lens.lens (nextToken :: ListTeamMembers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTeamMembers)
{-# DEPRECATED ltmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the project for which you want to list team members.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmProjectId :: Lens.Lens' ListTeamMembers Lude.Text
ltmProjectId = Lens.lens (projectId :: ListTeamMembers -> Lude.Text) (\s a -> s {projectId = a} :: ListTeamMembers)
{-# DEPRECATED ltmProjectId "Use generic-lens or generic-optics with 'projectId' instead." #-}

-- | The maximum number of team members you want returned in a response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmMaxResults :: Lens.Lens' ListTeamMembers (Lude.Maybe Lude.Natural)
ltmMaxResults = Lens.lens (maxResults :: ListTeamMembers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTeamMembers)
{-# DEPRECATED ltmMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListTeamMembers where
  page rq rs
    | Page.stop (rs Lens.^. ltmrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltmrsTeamMembers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltmNextToken Lens..~ rs Lens.^. ltmrsNextToken

instance Lude.AWSRequest ListTeamMembers where
  type Rs ListTeamMembers = ListTeamMembersResponse
  request = Req.postJSON codeStarService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTeamMembersResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "teamMembers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTeamMembers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeStar_20170419.ListTeamMembers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTeamMembers where
  toJSON ListTeamMembers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("projectId" Lude..= projectId),
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListTeamMembers where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTeamMembers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTeamMembersResponse' smart constructor.
data ListTeamMembersResponse = ListTeamMembersResponse'
  { -- | The continuation token to use when requesting the next set of results, if there are more results to be returned.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of team member objects for the project.
    teamMembers :: [TeamMember],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTeamMembersResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The continuation token to use when requesting the next set of results, if there are more results to be returned.
-- * 'teamMembers' - A list of team member objects for the project.
-- * 'responseStatus' - The response status code.
mkListTeamMembersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTeamMembersResponse
mkListTeamMembersResponse pResponseStatus_ =
  ListTeamMembersResponse'
    { nextToken = Lude.Nothing,
      teamMembers = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | The continuation token to use when requesting the next set of results, if there are more results to be returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmrsNextToken :: Lens.Lens' ListTeamMembersResponse (Lude.Maybe Lude.Text)
ltmrsNextToken = Lens.lens (nextToken :: ListTeamMembersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTeamMembersResponse)
{-# DEPRECATED ltmrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of team member objects for the project.
--
-- /Note:/ Consider using 'teamMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmrsTeamMembers :: Lens.Lens' ListTeamMembersResponse [TeamMember]
ltmrsTeamMembers = Lens.lens (teamMembers :: ListTeamMembersResponse -> [TeamMember]) (\s a -> s {teamMembers = a} :: ListTeamMembersResponse)
{-# DEPRECATED ltmrsTeamMembers "Use generic-lens or generic-optics with 'teamMembers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmrsResponseStatus :: Lens.Lens' ListTeamMembersResponse Lude.Int
ltmrsResponseStatus = Lens.lens (responseStatus :: ListTeamMembersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTeamMembersResponse)
{-# DEPRECATED ltmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
