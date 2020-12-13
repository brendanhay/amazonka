{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdatePullRequestDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the contents of the description of a pull request.
module Network.AWS.CodeCommit.UpdatePullRequestDescription
  ( -- * Creating a request
    UpdatePullRequestDescription (..),
    mkUpdatePullRequestDescription,

    -- ** Request lenses
    uprdPullRequestId,
    uprdDescription,

    -- * Destructuring the response
    UpdatePullRequestDescriptionResponse (..),
    mkUpdatePullRequestDescriptionResponse,

    -- ** Response lenses
    uprdrsPullRequest,
    uprdrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdatePullRequestDescription' smart constructor.
data UpdatePullRequestDescription = UpdatePullRequestDescription'
  { -- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
    pullRequestId :: Lude.Text,
    -- | The updated content of the description for the pull request. This content replaces the existing description.
    description :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePullRequestDescription' with the minimum fields required to make a request.
--
-- * 'pullRequestId' - The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
-- * 'description' - The updated content of the description for the pull request. This content replaces the existing description.
mkUpdatePullRequestDescription ::
  -- | 'pullRequestId'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  UpdatePullRequestDescription
mkUpdatePullRequestDescription pPullRequestId_ pDescription_ =
  UpdatePullRequestDescription'
    { pullRequestId = pPullRequestId_,
      description = pDescription_
    }

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprdPullRequestId :: Lens.Lens' UpdatePullRequestDescription Lude.Text
uprdPullRequestId = Lens.lens (pullRequestId :: UpdatePullRequestDescription -> Lude.Text) (\s a -> s {pullRequestId = a} :: UpdatePullRequestDescription)
{-# DEPRECATED uprdPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The updated content of the description for the pull request. This content replaces the existing description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprdDescription :: Lens.Lens' UpdatePullRequestDescription Lude.Text
uprdDescription = Lens.lens (description :: UpdatePullRequestDescription -> Lude.Text) (\s a -> s {description = a} :: UpdatePullRequestDescription)
{-# DEPRECATED uprdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdatePullRequestDescription where
  type
    Rs UpdatePullRequestDescription =
      UpdatePullRequestDescriptionResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdatePullRequestDescriptionResponse'
            Lude.<$> (x Lude..:> "pullRequest") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdatePullRequestDescription where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.UpdatePullRequestDescription" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdatePullRequestDescription where
  toJSON UpdatePullRequestDescription' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pullRequestId" Lude..= pullRequestId),
            Lude.Just ("description" Lude..= description)
          ]
      )

instance Lude.ToPath UpdatePullRequestDescription where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdatePullRequestDescription where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdatePullRequestDescriptionResponse' smart constructor.
data UpdatePullRequestDescriptionResponse = UpdatePullRequestDescriptionResponse'
  { -- | Information about the updated pull request.
    pullRequest :: PullRequest,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePullRequestDescriptionResponse' with the minimum fields required to make a request.
--
-- * 'pullRequest' - Information about the updated pull request.
-- * 'responseStatus' - The response status code.
mkUpdatePullRequestDescriptionResponse ::
  -- | 'pullRequest'
  PullRequest ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdatePullRequestDescriptionResponse
mkUpdatePullRequestDescriptionResponse
  pPullRequest_
  pResponseStatus_ =
    UpdatePullRequestDescriptionResponse'
      { pullRequest =
          pPullRequest_,
        responseStatus = pResponseStatus_
      }

-- | Information about the updated pull request.
--
-- /Note:/ Consider using 'pullRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprdrsPullRequest :: Lens.Lens' UpdatePullRequestDescriptionResponse PullRequest
uprdrsPullRequest = Lens.lens (pullRequest :: UpdatePullRequestDescriptionResponse -> PullRequest) (\s a -> s {pullRequest = a} :: UpdatePullRequestDescriptionResponse)
{-# DEPRECATED uprdrsPullRequest "Use generic-lens or generic-optics with 'pullRequest' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprdrsResponseStatus :: Lens.Lens' UpdatePullRequestDescriptionResponse Lude.Int
uprdrsResponseStatus = Lens.lens (responseStatus :: UpdatePullRequestDescriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdatePullRequestDescriptionResponse)
{-# DEPRECATED uprdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
