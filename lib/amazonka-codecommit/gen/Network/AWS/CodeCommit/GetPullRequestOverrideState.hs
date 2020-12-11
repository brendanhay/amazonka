{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetPullRequestOverrideState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about whether approval rules have been set aside (overridden) for a pull request, and if so, the Amazon Resource Name (ARN) of the user or identity that overrode the rules and their requirements for the pull request.
module Network.AWS.CodeCommit.GetPullRequestOverrideState
  ( -- * Creating a request
    GetPullRequestOverrideState (..),
    mkGetPullRequestOverrideState,

    -- ** Request lenses
    gprosPullRequestId,
    gprosRevisionId,

    -- * Destructuring the response
    GetPullRequestOverrideStateResponse (..),
    mkGetPullRequestOverrideStateResponse,

    -- ** Response lenses
    gprosrsOverridden,
    gprosrsOverrider,
    gprosrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetPullRequestOverrideState' smart constructor.
data GetPullRequestOverrideState = GetPullRequestOverrideState'
  { pullRequestId ::
      Lude.Text,
    revisionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPullRequestOverrideState' with the minimum fields required to make a request.
--
-- * 'pullRequestId' - The ID of the pull request for which you want to get information about whether approval rules have been set aside (overridden).
-- * 'revisionId' - The system-generated ID of the revision for the pull request. To retrieve the most recent revision ID, use 'GetPullRequest' .
mkGetPullRequestOverrideState ::
  -- | 'pullRequestId'
  Lude.Text ->
  -- | 'revisionId'
  Lude.Text ->
  GetPullRequestOverrideState
mkGetPullRequestOverrideState pPullRequestId_ pRevisionId_ =
  GetPullRequestOverrideState'
    { pullRequestId = pPullRequestId_,
      revisionId = pRevisionId_
    }

-- | The ID of the pull request for which you want to get information about whether approval rules have been set aside (overridden).
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprosPullRequestId :: Lens.Lens' GetPullRequestOverrideState Lude.Text
gprosPullRequestId = Lens.lens (pullRequestId :: GetPullRequestOverrideState -> Lude.Text) (\s a -> s {pullRequestId = a} :: GetPullRequestOverrideState)
{-# DEPRECATED gprosPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The system-generated ID of the revision for the pull request. To retrieve the most recent revision ID, use 'GetPullRequest' .
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprosRevisionId :: Lens.Lens' GetPullRequestOverrideState Lude.Text
gprosRevisionId = Lens.lens (revisionId :: GetPullRequestOverrideState -> Lude.Text) (\s a -> s {revisionId = a} :: GetPullRequestOverrideState)
{-# DEPRECATED gprosRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

instance Lude.AWSRequest GetPullRequestOverrideState where
  type
    Rs GetPullRequestOverrideState =
      GetPullRequestOverrideStateResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPullRequestOverrideStateResponse'
            Lude.<$> (x Lude..?> "overridden")
            Lude.<*> (x Lude..?> "overrider")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPullRequestOverrideState where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.GetPullRequestOverrideState" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetPullRequestOverrideState where
  toJSON GetPullRequestOverrideState' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pullRequestId" Lude..= pullRequestId),
            Lude.Just ("revisionId" Lude..= revisionId)
          ]
      )

instance Lude.ToPath GetPullRequestOverrideState where
  toPath = Lude.const "/"

instance Lude.ToQuery GetPullRequestOverrideState where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetPullRequestOverrideStateResponse' smart constructor.
data GetPullRequestOverrideStateResponse = GetPullRequestOverrideStateResponse'
  { overridden ::
      Lude.Maybe
        Lude.Bool,
    overrider ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPullRequestOverrideStateResponse' with the minimum fields required to make a request.
--
-- * 'overridden' - A Boolean value that indicates whether a pull request has had its rules set aside (TRUE) or whether all approval rules still apply (FALSE).
-- * 'overrider' - The Amazon Resource Name (ARN) of the user or identity that overrode the rules and their requirements for the pull request.
-- * 'responseStatus' - The response status code.
mkGetPullRequestOverrideStateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPullRequestOverrideStateResponse
mkGetPullRequestOverrideStateResponse pResponseStatus_ =
  GetPullRequestOverrideStateResponse'
    { overridden = Lude.Nothing,
      overrider = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A Boolean value that indicates whether a pull request has had its rules set aside (TRUE) or whether all approval rules still apply (FALSE).
--
-- /Note:/ Consider using 'overridden' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprosrsOverridden :: Lens.Lens' GetPullRequestOverrideStateResponse (Lude.Maybe Lude.Bool)
gprosrsOverridden = Lens.lens (overridden :: GetPullRequestOverrideStateResponse -> Lude.Maybe Lude.Bool) (\s a -> s {overridden = a} :: GetPullRequestOverrideStateResponse)
{-# DEPRECATED gprosrsOverridden "Use generic-lens or generic-optics with 'overridden' instead." #-}

-- | The Amazon Resource Name (ARN) of the user or identity that overrode the rules and their requirements for the pull request.
--
-- /Note:/ Consider using 'overrider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprosrsOverrider :: Lens.Lens' GetPullRequestOverrideStateResponse (Lude.Maybe Lude.Text)
gprosrsOverrider = Lens.lens (overrider :: GetPullRequestOverrideStateResponse -> Lude.Maybe Lude.Text) (\s a -> s {overrider = a} :: GetPullRequestOverrideStateResponse)
{-# DEPRECATED gprosrsOverrider "Use generic-lens or generic-optics with 'overrider' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprosrsResponseStatus :: Lens.Lens' GetPullRequestOverrideStateResponse Lude.Int
gprosrsResponseStatus = Lens.lens (responseStatus :: GetPullRequestOverrideStateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPullRequestOverrideStateResponse)
{-# DEPRECATED gprosrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
