{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.BatchDisassociateApprovalRuleTemplateFromRepositories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the association between an approval rule template and one or more specified repositories.
module Network.AWS.CodeCommit.BatchDisassociateApprovalRuleTemplateFromRepositories
  ( -- * Creating a request
    BatchDisassociateApprovalRuleTemplateFromRepositories (..),
    mkBatchDisassociateApprovalRuleTemplateFromRepositories,

    -- ** Request lenses
    bdartfrRepositoryNames,
    bdartfrApprovalRuleTemplateName,

    -- * Destructuring the response
    BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse (..),
    mkBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse,

    -- ** Response lenses
    bdartfrrsDisassociatedRepositoryNames,
    bdartfrrsErrors,
    bdartfrrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchDisassociateApprovalRuleTemplateFromRepositories' smart constructor.
data BatchDisassociateApprovalRuleTemplateFromRepositories = BatchDisassociateApprovalRuleTemplateFromRepositories'
  { -- | The repository names that you want to disassociate from the approval rule template.
    repositoryNames :: [Lude.Text],
    -- | The name of the template that you want to disassociate from one or more repositories.
    approvalRuleTemplateName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDisassociateApprovalRuleTemplateFromRepositories' with the minimum fields required to make a request.
--
-- * 'repositoryNames' - The repository names that you want to disassociate from the approval rule template.
-- * 'approvalRuleTemplateName' - The name of the template that you want to disassociate from one or more repositories.
mkBatchDisassociateApprovalRuleTemplateFromRepositories ::
  -- | 'approvalRuleTemplateName'
  Lude.Text ->
  BatchDisassociateApprovalRuleTemplateFromRepositories
mkBatchDisassociateApprovalRuleTemplateFromRepositories
  pApprovalRuleTemplateName_ =
    BatchDisassociateApprovalRuleTemplateFromRepositories'
      { repositoryNames =
          Lude.mempty,
        approvalRuleTemplateName =
          pApprovalRuleTemplateName_
      }

-- | The repository names that you want to disassociate from the approval rule template.
--
-- /Note:/ Consider using 'repositoryNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdartfrRepositoryNames :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositories [Lude.Text]
bdartfrRepositoryNames = Lens.lens (repositoryNames :: BatchDisassociateApprovalRuleTemplateFromRepositories -> [Lude.Text]) (\s a -> s {repositoryNames = a} :: BatchDisassociateApprovalRuleTemplateFromRepositories)
{-# DEPRECATED bdartfrRepositoryNames "Use generic-lens or generic-optics with 'repositoryNames' instead." #-}

-- | The name of the template that you want to disassociate from one or more repositories.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdartfrApprovalRuleTemplateName :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositories Lude.Text
bdartfrApprovalRuleTemplateName = Lens.lens (approvalRuleTemplateName :: BatchDisassociateApprovalRuleTemplateFromRepositories -> Lude.Text) (\s a -> s {approvalRuleTemplateName = a} :: BatchDisassociateApprovalRuleTemplateFromRepositories)
{-# DEPRECATED bdartfrApprovalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead." #-}

instance
  Lude.AWSRequest
    BatchDisassociateApprovalRuleTemplateFromRepositories
  where
  type
    Rs BatchDisassociateApprovalRuleTemplateFromRepositories =
      BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse'
            Lude.<$> (x Lude..?> "disassociatedRepositoryNames" Lude..!@ Lude.mempty)
              Lude.<*> (x Lude..?> "errors" Lude..!@ Lude.mempty)
              Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    BatchDisassociateApprovalRuleTemplateFromRepositories
  where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.BatchDisassociateApprovalRuleTemplateFromRepositories" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance
  Lude.ToJSON
    BatchDisassociateApprovalRuleTemplateFromRepositories
  where
  toJSON BatchDisassociateApprovalRuleTemplateFromRepositories' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("repositoryNames" Lude..= repositoryNames),
            Lude.Just
              ("approvalRuleTemplateName" Lude..= approvalRuleTemplateName)
          ]
      )

instance
  Lude.ToPath
    BatchDisassociateApprovalRuleTemplateFromRepositories
  where
  toPath = Lude.const "/"

instance
  Lude.ToQuery
    BatchDisassociateApprovalRuleTemplateFromRepositories
  where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse' smart constructor.
data BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse = BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse'
  { -- | A list of repository names that have had their association with the template removed.
    disassociatedRepositoryNames :: [Lude.Text],
    -- | A list of any errors that might have occurred while attempting to remove the association between the template and the repositories.
    errors :: [BatchDisassociateApprovalRuleTemplateFromRepositoriesError],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse' with the minimum fields required to make a request.
--
-- * 'disassociatedRepositoryNames' - A list of repository names that have had their association with the template removed.
-- * 'errors' - A list of any errors that might have occurred while attempting to remove the association between the template and the repositories.
-- * 'responseStatus' - The response status code.
mkBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
mkBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
  pResponseStatus_ =
    BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse'
      { disassociatedRepositoryNames =
          Lude.mempty,
        errors = Lude.mempty,
        responseStatus =
          pResponseStatus_
      }

-- | A list of repository names that have had their association with the template removed.
--
-- /Note:/ Consider using 'disassociatedRepositoryNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdartfrrsDisassociatedRepositoryNames :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse [Lude.Text]
bdartfrrsDisassociatedRepositoryNames = Lens.lens (disassociatedRepositoryNames :: BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse -> [Lude.Text]) (\s a -> s {disassociatedRepositoryNames = a} :: BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse)
{-# DEPRECATED bdartfrrsDisassociatedRepositoryNames "Use generic-lens or generic-optics with 'disassociatedRepositoryNames' instead." #-}

-- | A list of any errors that might have occurred while attempting to remove the association between the template and the repositories.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdartfrrsErrors :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse [BatchDisassociateApprovalRuleTemplateFromRepositoriesError]
bdartfrrsErrors = Lens.lens (errors :: BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse -> [BatchDisassociateApprovalRuleTemplateFromRepositoriesError]) (\s a -> s {errors = a} :: BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse)
{-# DEPRECATED bdartfrrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdartfrrsResponseStatus :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse Lude.Int
bdartfrrsResponseStatus = Lens.lens (responseStatus :: BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse)
{-# DEPRECATED bdartfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
