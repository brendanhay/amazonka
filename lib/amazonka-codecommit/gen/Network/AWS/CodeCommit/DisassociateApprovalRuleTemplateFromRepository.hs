{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.DisassociateApprovalRuleTemplateFromRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the association between a template and a repository so that approval rules based on the template are not automatically created when pull requests are created in the specified repository. This does not delete any approval rules previously created for pull requests through the template association.
module Network.AWS.CodeCommit.DisassociateApprovalRuleTemplateFromRepository
  ( -- * Creating a request
    DisassociateApprovalRuleTemplateFromRepository (..),
    mkDisassociateApprovalRuleTemplateFromRepository,

    -- ** Request lenses
    dartfrApprovalRuleTemplateName,
    dartfrRepositoryName,

    -- * Destructuring the response
    DisassociateApprovalRuleTemplateFromRepositoryResponse (..),
    mkDisassociateApprovalRuleTemplateFromRepositoryResponse,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateApprovalRuleTemplateFromRepository' smart constructor.
data DisassociateApprovalRuleTemplateFromRepository = DisassociateApprovalRuleTemplateFromRepository'
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DisassociateApprovalRuleTemplateFromRepository' with the minimum fields required to make a request.
--
-- * 'approvalRuleTemplateName' - The name of the approval rule template to disassociate from a specified repository.
-- * 'repositoryName' - The name of the repository you want to disassociate from the template.
mkDisassociateApprovalRuleTemplateFromRepository ::
  -- | 'approvalRuleTemplateName'
  Lude.Text ->
  -- | 'repositoryName'
  Lude.Text ->
  DisassociateApprovalRuleTemplateFromRepository
mkDisassociateApprovalRuleTemplateFromRepository
  pApprovalRuleTemplateName_
  pRepositoryName_ =
    DisassociateApprovalRuleTemplateFromRepository'
      { approvalRuleTemplateName =
          pApprovalRuleTemplateName_,
        repositoryName = pRepositoryName_
      }

-- | The name of the approval rule template to disassociate from a specified repository.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dartfrApprovalRuleTemplateName :: Lens.Lens' DisassociateApprovalRuleTemplateFromRepository Lude.Text
dartfrApprovalRuleTemplateName = Lens.lens (approvalRuleTemplateName :: DisassociateApprovalRuleTemplateFromRepository -> Lude.Text) (\s a -> s {approvalRuleTemplateName = a} :: DisassociateApprovalRuleTemplateFromRepository)
{-# DEPRECATED dartfrApprovalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead." #-}

-- | The name of the repository you want to disassociate from the template.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dartfrRepositoryName :: Lens.Lens' DisassociateApprovalRuleTemplateFromRepository Lude.Text
dartfrRepositoryName = Lens.lens (repositoryName :: DisassociateApprovalRuleTemplateFromRepository -> Lude.Text) (\s a -> s {repositoryName = a} :: DisassociateApprovalRuleTemplateFromRepository)
{-# DEPRECATED dartfrRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance
  Lude.AWSRequest
    DisassociateApprovalRuleTemplateFromRepository
  where
  type
    Rs DisassociateApprovalRuleTemplateFromRepository =
      DisassociateApprovalRuleTemplateFromRepositoryResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveNull
      DisassociateApprovalRuleTemplateFromRepositoryResponse'

instance
  Lude.ToHeaders
    DisassociateApprovalRuleTemplateFromRepository
  where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.DisassociateApprovalRuleTemplateFromRepository" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateApprovalRuleTemplateFromRepository where
  toJSON DisassociateApprovalRuleTemplateFromRepository' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("approvalRuleTemplateName" Lude..= approvalRuleTemplateName),
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath DisassociateApprovalRuleTemplateFromRepository where
  toPath = Lude.const "/"

instance
  Lude.ToQuery
    DisassociateApprovalRuleTemplateFromRepository
  where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateApprovalRuleTemplateFromRepositoryResponse' smart constructor.
data DisassociateApprovalRuleTemplateFromRepositoryResponse = DisassociateApprovalRuleTemplateFromRepositoryResponse'
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

-- | Creates a value of 'DisassociateApprovalRuleTemplateFromRepositoryResponse' with the minimum fields required to make a request.
mkDisassociateApprovalRuleTemplateFromRepositoryResponse ::
  DisassociateApprovalRuleTemplateFromRepositoryResponse
mkDisassociateApprovalRuleTemplateFromRepositoryResponse =
  DisassociateApprovalRuleTemplateFromRepositoryResponse'
