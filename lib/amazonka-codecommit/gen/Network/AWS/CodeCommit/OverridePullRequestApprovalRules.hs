{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.OverridePullRequestApprovalRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets aside (overrides) all approval rule requirements for a specified pull request.
module Network.AWS.CodeCommit.OverridePullRequestApprovalRules
  ( -- * Creating a request
    OverridePullRequestApprovalRules (..),
    mkOverridePullRequestApprovalRules,

    -- ** Request lenses
    oprarPullRequestId,
    oprarOverrideStatus,
    oprarRevisionId,

    -- * Destructuring the response
    OverridePullRequestApprovalRulesResponse (..),
    mkOverridePullRequestApprovalRulesResponse,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkOverridePullRequestApprovalRules' smart constructor.
data OverridePullRequestApprovalRules = OverridePullRequestApprovalRules'
  { -- | The system-generated ID of the pull request for which you want to override all approval rule requirements. To get this information, use 'GetPullRequest' .
    pullRequestId :: Lude.Text,
    -- | Whether you want to set aside approval rule requirements for the pull request (OVERRIDE) or revoke a previous override and apply approval rule requirements (REVOKE). REVOKE status is not stored.
    overrideStatus :: OverrideStatus,
    -- | The system-generated ID of the most recent revision of the pull request. You cannot override approval rules for anything but the most recent revision of a pull request. To get the revision ID, use GetPullRequest.
    revisionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OverridePullRequestApprovalRules' with the minimum fields required to make a request.
--
-- * 'pullRequestId' - The system-generated ID of the pull request for which you want to override all approval rule requirements. To get this information, use 'GetPullRequest' .
-- * 'overrideStatus' - Whether you want to set aside approval rule requirements for the pull request (OVERRIDE) or revoke a previous override and apply approval rule requirements (REVOKE). REVOKE status is not stored.
-- * 'revisionId' - The system-generated ID of the most recent revision of the pull request. You cannot override approval rules for anything but the most recent revision of a pull request. To get the revision ID, use GetPullRequest.
mkOverridePullRequestApprovalRules ::
  -- | 'pullRequestId'
  Lude.Text ->
  -- | 'overrideStatus'
  OverrideStatus ->
  -- | 'revisionId'
  Lude.Text ->
  OverridePullRequestApprovalRules
mkOverridePullRequestApprovalRules
  pPullRequestId_
  pOverrideStatus_
  pRevisionId_ =
    OverridePullRequestApprovalRules'
      { pullRequestId =
          pPullRequestId_,
        overrideStatus = pOverrideStatus_,
        revisionId = pRevisionId_
      }

-- | The system-generated ID of the pull request for which you want to override all approval rule requirements. To get this information, use 'GetPullRequest' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oprarPullRequestId :: Lens.Lens' OverridePullRequestApprovalRules Lude.Text
oprarPullRequestId = Lens.lens (pullRequestId :: OverridePullRequestApprovalRules -> Lude.Text) (\s a -> s {pullRequestId = a} :: OverridePullRequestApprovalRules)
{-# DEPRECATED oprarPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | Whether you want to set aside approval rule requirements for the pull request (OVERRIDE) or revoke a previous override and apply approval rule requirements (REVOKE). REVOKE status is not stored.
--
-- /Note:/ Consider using 'overrideStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oprarOverrideStatus :: Lens.Lens' OverridePullRequestApprovalRules OverrideStatus
oprarOverrideStatus = Lens.lens (overrideStatus :: OverridePullRequestApprovalRules -> OverrideStatus) (\s a -> s {overrideStatus = a} :: OverridePullRequestApprovalRules)
{-# DEPRECATED oprarOverrideStatus "Use generic-lens or generic-optics with 'overrideStatus' instead." #-}

-- | The system-generated ID of the most recent revision of the pull request. You cannot override approval rules for anything but the most recent revision of a pull request. To get the revision ID, use GetPullRequest.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oprarRevisionId :: Lens.Lens' OverridePullRequestApprovalRules Lude.Text
oprarRevisionId = Lens.lens (revisionId :: OverridePullRequestApprovalRules -> Lude.Text) (\s a -> s {revisionId = a} :: OverridePullRequestApprovalRules)
{-# DEPRECATED oprarRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

instance Lude.AWSRequest OverridePullRequestApprovalRules where
  type
    Rs OverridePullRequestApprovalRules =
      OverridePullRequestApprovalRulesResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveNull OverridePullRequestApprovalRulesResponse'

instance Lude.ToHeaders OverridePullRequestApprovalRules where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.OverridePullRequestApprovalRules" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON OverridePullRequestApprovalRules where
  toJSON OverridePullRequestApprovalRules' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pullRequestId" Lude..= pullRequestId),
            Lude.Just ("overrideStatus" Lude..= overrideStatus),
            Lude.Just ("revisionId" Lude..= revisionId)
          ]
      )

instance Lude.ToPath OverridePullRequestApprovalRules where
  toPath = Lude.const "/"

instance Lude.ToQuery OverridePullRequestApprovalRules where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkOverridePullRequestApprovalRulesResponse' smart constructor.
data OverridePullRequestApprovalRulesResponse = OverridePullRequestApprovalRulesResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OverridePullRequestApprovalRulesResponse' with the minimum fields required to make a request.
mkOverridePullRequestApprovalRulesResponse ::
  OverridePullRequestApprovalRulesResponse
mkOverridePullRequestApprovalRulesResponse =
  OverridePullRequestApprovalRulesResponse'
