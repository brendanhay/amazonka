{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.AssociateApprovalRuleTemplateWithRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an association between an approval rule template and a specified repository. Then, the next time a pull request is created in the repository where the destination reference (if specified) matches the destination reference (branch) for the pull request, an approval rule that matches the template conditions is automatically created for that pull request. If no destination references are specified in the template, an approval rule that matches the template contents is created for all pull requests in that repository.
module Network.AWS.CodeCommit.AssociateApprovalRuleTemplateWithRepository
  ( -- * Creating a request
    AssociateApprovalRuleTemplateWithRepository (..),
    mkAssociateApprovalRuleTemplateWithRepository,

    -- ** Request lenses
    aartwrApprovalRuleTemplateName,
    aartwrRepositoryName,

    -- * Destructuring the response
    AssociateApprovalRuleTemplateWithRepositoryResponse (..),
    mkAssociateApprovalRuleTemplateWithRepositoryResponse,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateApprovalRuleTemplateWithRepository' smart constructor.
data AssociateApprovalRuleTemplateWithRepository = AssociateApprovalRuleTemplateWithRepository'
  { approvalRuleTemplateName ::
      Lude.Text,
    repositoryName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateApprovalRuleTemplateWithRepository' with the minimum fields required to make a request.
--
-- * 'approvalRuleTemplateName' - The name for the approval rule template.
-- * 'repositoryName' - The name of the repository that you want to associate with the template.
mkAssociateApprovalRuleTemplateWithRepository ::
  -- | 'approvalRuleTemplateName'
  Lude.Text ->
  -- | 'repositoryName'
  Lude.Text ->
  AssociateApprovalRuleTemplateWithRepository
mkAssociateApprovalRuleTemplateWithRepository
  pApprovalRuleTemplateName_
  pRepositoryName_ =
    AssociateApprovalRuleTemplateWithRepository'
      { approvalRuleTemplateName =
          pApprovalRuleTemplateName_,
        repositoryName = pRepositoryName_
      }

-- | The name for the approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aartwrApprovalRuleTemplateName :: Lens.Lens' AssociateApprovalRuleTemplateWithRepository Lude.Text
aartwrApprovalRuleTemplateName = Lens.lens (approvalRuleTemplateName :: AssociateApprovalRuleTemplateWithRepository -> Lude.Text) (\s a -> s {approvalRuleTemplateName = a} :: AssociateApprovalRuleTemplateWithRepository)
{-# DEPRECATED aartwrApprovalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead." #-}

-- | The name of the repository that you want to associate with the template.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aartwrRepositoryName :: Lens.Lens' AssociateApprovalRuleTemplateWithRepository Lude.Text
aartwrRepositoryName = Lens.lens (repositoryName :: AssociateApprovalRuleTemplateWithRepository -> Lude.Text) (\s a -> s {repositoryName = a} :: AssociateApprovalRuleTemplateWithRepository)
{-# DEPRECATED aartwrRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance
  Lude.AWSRequest
    AssociateApprovalRuleTemplateWithRepository
  where
  type
    Rs AssociateApprovalRuleTemplateWithRepository =
      AssociateApprovalRuleTemplateWithRepositoryResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveNull
      AssociateApprovalRuleTemplateWithRepositoryResponse'

instance Lude.ToHeaders AssociateApprovalRuleTemplateWithRepository where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.AssociateApprovalRuleTemplateWithRepository" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateApprovalRuleTemplateWithRepository where
  toJSON AssociateApprovalRuleTemplateWithRepository' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("approvalRuleTemplateName" Lude..= approvalRuleTemplateName),
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath AssociateApprovalRuleTemplateWithRepository where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateApprovalRuleTemplateWithRepository where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateApprovalRuleTemplateWithRepositoryResponse' smart constructor.
data AssociateApprovalRuleTemplateWithRepositoryResponse = AssociateApprovalRuleTemplateWithRepositoryResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'AssociateApprovalRuleTemplateWithRepositoryResponse' with the minimum fields required to make a request.
mkAssociateApprovalRuleTemplateWithRepositoryResponse ::
  AssociateApprovalRuleTemplateWithRepositoryResponse
mkAssociateApprovalRuleTemplateWithRepositoryResponse =
  AssociateApprovalRuleTemplateWithRepositoryResponse'
