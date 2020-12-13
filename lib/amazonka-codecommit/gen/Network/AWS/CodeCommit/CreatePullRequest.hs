{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.CreatePullRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a pull request in the specified repository.
module Network.AWS.CodeCommit.CreatePullRequest
  ( -- * Creating a request
    CreatePullRequest (..),
    mkCreatePullRequest,

    -- ** Request lenses
    cprTargets,
    cprTitle,
    cprClientRequestToken,
    cprDescription,

    -- * Destructuring the response
    CreatePullRequestResponse (..),
    mkCreatePullRequestResponse,

    -- ** Response lenses
    cprrsPullRequest,
    cprrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreatePullRequest' smart constructor.
data CreatePullRequest = CreatePullRequest'
  { -- | The targets for the pull request, including the source of the code to be reviewed (the source branch) and the destination where the creator of the pull request intends the code to be merged after the pull request is closed (the destination branch).
    targets :: [Target],
    -- | The title of the pull request. This title is used to identify the pull request to other users in the repository.
    title :: Lude.Text,
    -- | A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
    clientRequestToken :: Lude.Maybe Lude.Text,
    -- | A description of the pull request.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePullRequest' with the minimum fields required to make a request.
--
-- * 'targets' - The targets for the pull request, including the source of the code to be reviewed (the source branch) and the destination where the creator of the pull request intends the code to be merged after the pull request is closed (the destination branch).
-- * 'title' - The title of the pull request. This title is used to identify the pull request to other users in the repository.
-- * 'clientRequestToken' - A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
-- * 'description' - A description of the pull request.
mkCreatePullRequest ::
  -- | 'title'
  Lude.Text ->
  CreatePullRequest
mkCreatePullRequest pTitle_ =
  CreatePullRequest'
    { targets = Lude.mempty,
      title = pTitle_,
      clientRequestToken = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The targets for the pull request, including the source of the code to be reviewed (the source branch) and the destination where the creator of the pull request intends the code to be merged after the pull request is closed (the destination branch).
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprTargets :: Lens.Lens' CreatePullRequest [Target]
cprTargets = Lens.lens (targets :: CreatePullRequest -> [Target]) (\s a -> s {targets = a} :: CreatePullRequest)
{-# DEPRECATED cprTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The title of the pull request. This title is used to identify the pull request to other users in the repository.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprTitle :: Lens.Lens' CreatePullRequest Lude.Text
cprTitle = Lens.lens (title :: CreatePullRequest -> Lude.Text) (\s a -> s {title = a} :: CreatePullRequest)
{-# DEPRECATED cprTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprClientRequestToken :: Lens.Lens' CreatePullRequest (Lude.Maybe Lude.Text)
cprClientRequestToken = Lens.lens (clientRequestToken :: CreatePullRequest -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: CreatePullRequest)
{-# DEPRECATED cprClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | A description of the pull request.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprDescription :: Lens.Lens' CreatePullRequest (Lude.Maybe Lude.Text)
cprDescription = Lens.lens (description :: CreatePullRequest -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreatePullRequest)
{-# DEPRECATED cprDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest CreatePullRequest where
  type Rs CreatePullRequest = CreatePullRequestResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePullRequestResponse'
            Lude.<$> (x Lude..:> "pullRequest") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePullRequest where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.CreatePullRequest" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePullRequest where
  toJSON CreatePullRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("targets" Lude..= targets),
            Lude.Just ("title" Lude..= title),
            ("clientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            ("description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath CreatePullRequest where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePullRequest where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreatePullRequestResponse' smart constructor.
data CreatePullRequestResponse = CreatePullRequestResponse'
  { -- | Information about the newly created pull request.
    pullRequest :: PullRequest,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePullRequestResponse' with the minimum fields required to make a request.
--
-- * 'pullRequest' - Information about the newly created pull request.
-- * 'responseStatus' - The response status code.
mkCreatePullRequestResponse ::
  -- | 'pullRequest'
  PullRequest ->
  -- | 'responseStatus'
  Lude.Int ->
  CreatePullRequestResponse
mkCreatePullRequestResponse pPullRequest_ pResponseStatus_ =
  CreatePullRequestResponse'
    { pullRequest = pPullRequest_,
      responseStatus = pResponseStatus_
    }

-- | Information about the newly created pull request.
--
-- /Note:/ Consider using 'pullRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsPullRequest :: Lens.Lens' CreatePullRequestResponse PullRequest
cprrsPullRequest = Lens.lens (pullRequest :: CreatePullRequestResponse -> PullRequest) (\s a -> s {pullRequest = a} :: CreatePullRequestResponse)
{-# DEPRECATED cprrsPullRequest "Use generic-lens or generic-optics with 'pullRequest' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreatePullRequestResponse Lude.Int
cprrsResponseStatus = Lens.lens (responseStatus :: CreatePullRequestResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePullRequestResponse)
{-# DEPRECATED cprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
