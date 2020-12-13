{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.BatchAssociateApprovalRuleTemplateWithRepositories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an association between an approval rule template and one or more specified repositories.
module Network.AWS.CodeCommit.BatchAssociateApprovalRuleTemplateWithRepositories
  ( -- * Creating a request
    BatchAssociateApprovalRuleTemplateWithRepositories (..),
    mkBatchAssociateApprovalRuleTemplateWithRepositories,

    -- ** Request lenses
    baartwrRepositoryNames,
    baartwrApprovalRuleTemplateName,

    -- * Destructuring the response
    BatchAssociateApprovalRuleTemplateWithRepositoriesResponse (..),
    mkBatchAssociateApprovalRuleTemplateWithRepositoriesResponse,

    -- ** Response lenses
    baartwrrsErrors,
    baartwrrsAssociatedRepositoryNames,
    baartwrrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchAssociateApprovalRuleTemplateWithRepositories' smart constructor.
data BatchAssociateApprovalRuleTemplateWithRepositories = BatchAssociateApprovalRuleTemplateWithRepositories'
  { -- | The names of the repositories you want to associate with the template.
    repositoryNames :: [Lude.Text],
    -- | The name of the template you want to associate with one or more repositories.
    approvalRuleTemplateName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchAssociateApprovalRuleTemplateWithRepositories' with the minimum fields required to make a request.
--
-- * 'repositoryNames' - The names of the repositories you want to associate with the template.
-- * 'approvalRuleTemplateName' - The name of the template you want to associate with one or more repositories.
mkBatchAssociateApprovalRuleTemplateWithRepositories ::
  -- | 'approvalRuleTemplateName'
  Lude.Text ->
  BatchAssociateApprovalRuleTemplateWithRepositories
mkBatchAssociateApprovalRuleTemplateWithRepositories
  pApprovalRuleTemplateName_ =
    BatchAssociateApprovalRuleTemplateWithRepositories'
      { repositoryNames =
          Lude.mempty,
        approvalRuleTemplateName =
          pApprovalRuleTemplateName_
      }

-- | The names of the repositories you want to associate with the template.
--
-- /Note:/ Consider using 'repositoryNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baartwrRepositoryNames :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositories [Lude.Text]
baartwrRepositoryNames = Lens.lens (repositoryNames :: BatchAssociateApprovalRuleTemplateWithRepositories -> [Lude.Text]) (\s a -> s {repositoryNames = a} :: BatchAssociateApprovalRuleTemplateWithRepositories)
{-# DEPRECATED baartwrRepositoryNames "Use generic-lens or generic-optics with 'repositoryNames' instead." #-}

-- | The name of the template you want to associate with one or more repositories.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baartwrApprovalRuleTemplateName :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositories Lude.Text
baartwrApprovalRuleTemplateName = Lens.lens (approvalRuleTemplateName :: BatchAssociateApprovalRuleTemplateWithRepositories -> Lude.Text) (\s a -> s {approvalRuleTemplateName = a} :: BatchAssociateApprovalRuleTemplateWithRepositories)
{-# DEPRECATED baartwrApprovalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead." #-}

instance
  Lude.AWSRequest
    BatchAssociateApprovalRuleTemplateWithRepositories
  where
  type
    Rs BatchAssociateApprovalRuleTemplateWithRepositories =
      BatchAssociateApprovalRuleTemplateWithRepositoriesResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchAssociateApprovalRuleTemplateWithRepositoriesResponse'
            Lude.<$> (x Lude..?> "errors" Lude..!@ Lude.mempty)
              Lude.<*> (x Lude..?> "associatedRepositoryNames" Lude..!@ Lude.mempty)
              Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    BatchAssociateApprovalRuleTemplateWithRepositories
  where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.BatchAssociateApprovalRuleTemplateWithRepositories" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance
  Lude.ToJSON
    BatchAssociateApprovalRuleTemplateWithRepositories
  where
  toJSON BatchAssociateApprovalRuleTemplateWithRepositories' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("repositoryNames" Lude..= repositoryNames),
            Lude.Just
              ("approvalRuleTemplateName" Lude..= approvalRuleTemplateName)
          ]
      )

instance
  Lude.ToPath
    BatchAssociateApprovalRuleTemplateWithRepositories
  where
  toPath = Lude.const "/"

instance
  Lude.ToQuery
    BatchAssociateApprovalRuleTemplateWithRepositories
  where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchAssociateApprovalRuleTemplateWithRepositoriesResponse' smart constructor.
data BatchAssociateApprovalRuleTemplateWithRepositoriesResponse = BatchAssociateApprovalRuleTemplateWithRepositoriesResponse'
  { -- | A list of any errors that might have occurred while attempting to create the association between the template and the repositories.
    errors :: [BatchAssociateApprovalRuleTemplateWithRepositoriesError],
    -- | A list of names of the repositories that have been associated with the template.
    associatedRepositoryNames :: [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchAssociateApprovalRuleTemplateWithRepositoriesResponse' with the minimum fields required to make a request.
--
-- * 'errors' - A list of any errors that might have occurred while attempting to create the association between the template and the repositories.
-- * 'associatedRepositoryNames' - A list of names of the repositories that have been associated with the template.
-- * 'responseStatus' - The response status code.
mkBatchAssociateApprovalRuleTemplateWithRepositoriesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchAssociateApprovalRuleTemplateWithRepositoriesResponse
mkBatchAssociateApprovalRuleTemplateWithRepositoriesResponse
  pResponseStatus_ =
    BatchAssociateApprovalRuleTemplateWithRepositoriesResponse'
      { errors =
          Lude.mempty,
        associatedRepositoryNames =
          Lude.mempty,
        responseStatus = pResponseStatus_
      }

-- | A list of any errors that might have occurred while attempting to create the association between the template and the repositories.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baartwrrsErrors :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesResponse [BatchAssociateApprovalRuleTemplateWithRepositoriesError]
baartwrrsErrors = Lens.lens (errors :: BatchAssociateApprovalRuleTemplateWithRepositoriesResponse -> [BatchAssociateApprovalRuleTemplateWithRepositoriesError]) (\s a -> s {errors = a} :: BatchAssociateApprovalRuleTemplateWithRepositoriesResponse)
{-# DEPRECATED baartwrrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | A list of names of the repositories that have been associated with the template.
--
-- /Note:/ Consider using 'associatedRepositoryNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baartwrrsAssociatedRepositoryNames :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesResponse [Lude.Text]
baartwrrsAssociatedRepositoryNames = Lens.lens (associatedRepositoryNames :: BatchAssociateApprovalRuleTemplateWithRepositoriesResponse -> [Lude.Text]) (\s a -> s {associatedRepositoryNames = a} :: BatchAssociateApprovalRuleTemplateWithRepositoriesResponse)
{-# DEPRECATED baartwrrsAssociatedRepositoryNames "Use generic-lens or generic-optics with 'associatedRepositoryNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baartwrrsResponseStatus :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesResponse Lude.Int
baartwrrsResponseStatus = Lens.lens (responseStatus :: BatchAssociateApprovalRuleTemplateWithRepositoriesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchAssociateApprovalRuleTemplateWithRepositoriesResponse)
{-# DEPRECATED baartwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
