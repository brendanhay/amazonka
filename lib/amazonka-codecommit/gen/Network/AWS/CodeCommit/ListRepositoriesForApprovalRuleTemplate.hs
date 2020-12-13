{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.ListRepositoriesForApprovalRuleTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all repositories associated with the specified approval rule template.
module Network.AWS.CodeCommit.ListRepositoriesForApprovalRuleTemplate
  ( -- * Creating a request
    ListRepositoriesForApprovalRuleTemplate (..),
    mkListRepositoriesForApprovalRuleTemplate,

    -- ** Request lenses
    lrfartNextToken,
    lrfartApprovalRuleTemplateName,
    lrfartMaxResults,

    -- * Destructuring the response
    ListRepositoriesForApprovalRuleTemplateResponse (..),
    mkListRepositoriesForApprovalRuleTemplateResponse,

    -- ** Response lenses
    lrfartrsRepositoryNames,
    lrfartrsNextToken,
    lrfartrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListRepositoriesForApprovalRuleTemplate' smart constructor.
data ListRepositoriesForApprovalRuleTemplate = ListRepositoriesForApprovalRuleTemplate'
  { -- | An enumeration token that, when provided in a request, returns the next batch of the results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name of the approval rule template for which you want to list repositories that are associated with that template.
    approvalRuleTemplateName :: Lude.Text,
    -- | A non-zero, non-negative integer used to limit the number of returned results.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRepositoriesForApprovalRuleTemplate' with the minimum fields required to make a request.
--
-- * 'nextToken' - An enumeration token that, when provided in a request, returns the next batch of the results.
-- * 'approvalRuleTemplateName' - The name of the approval rule template for which you want to list repositories that are associated with that template.
-- * 'maxResults' - A non-zero, non-negative integer used to limit the number of returned results.
mkListRepositoriesForApprovalRuleTemplate ::
  -- | 'approvalRuleTemplateName'
  Lude.Text ->
  ListRepositoriesForApprovalRuleTemplate
mkListRepositoriesForApprovalRuleTemplate
  pApprovalRuleTemplateName_ =
    ListRepositoriesForApprovalRuleTemplate'
      { nextToken =
          Lude.Nothing,
        approvalRuleTemplateName = pApprovalRuleTemplateName_,
        maxResults = Lude.Nothing
      }

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfartNextToken :: Lens.Lens' ListRepositoriesForApprovalRuleTemplate (Lude.Maybe Lude.Text)
lrfartNextToken = Lens.lens (nextToken :: ListRepositoriesForApprovalRuleTemplate -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRepositoriesForApprovalRuleTemplate)
{-# DEPRECATED lrfartNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the approval rule template for which you want to list repositories that are associated with that template.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfartApprovalRuleTemplateName :: Lens.Lens' ListRepositoriesForApprovalRuleTemplate Lude.Text
lrfartApprovalRuleTemplateName = Lens.lens (approvalRuleTemplateName :: ListRepositoriesForApprovalRuleTemplate -> Lude.Text) (\s a -> s {approvalRuleTemplateName = a} :: ListRepositoriesForApprovalRuleTemplate)
{-# DEPRECATED lrfartApprovalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead." #-}

-- | A non-zero, non-negative integer used to limit the number of returned results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfartMaxResults :: Lens.Lens' ListRepositoriesForApprovalRuleTemplate (Lude.Maybe Lude.Int)
lrfartMaxResults = Lens.lens (maxResults :: ListRepositoriesForApprovalRuleTemplate -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListRepositoriesForApprovalRuleTemplate)
{-# DEPRECATED lrfartMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListRepositoriesForApprovalRuleTemplate where
  type
    Rs ListRepositoriesForApprovalRuleTemplate =
      ListRepositoriesForApprovalRuleTemplateResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListRepositoriesForApprovalRuleTemplateResponse'
            Lude.<$> (x Lude..?> "repositoryNames" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListRepositoriesForApprovalRuleTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.ListRepositoriesForApprovalRuleTemplate" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListRepositoriesForApprovalRuleTemplate where
  toJSON ListRepositoriesForApprovalRuleTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just
              ("approvalRuleTemplateName" Lude..= approvalRuleTemplateName),
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListRepositoriesForApprovalRuleTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery ListRepositoriesForApprovalRuleTemplate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListRepositoriesForApprovalRuleTemplateResponse' smart constructor.
data ListRepositoriesForApprovalRuleTemplateResponse = ListRepositoriesForApprovalRuleTemplateResponse'
  { -- | A list of repository names that are associated with the specified approval rule template.
    repositoryNames :: Lude.Maybe [Lude.Text],
    -- | An enumeration token that allows the operation to batch the next results of the operation.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRepositoriesForApprovalRuleTemplateResponse' with the minimum fields required to make a request.
--
-- * 'repositoryNames' - A list of repository names that are associated with the specified approval rule template.
-- * 'nextToken' - An enumeration token that allows the operation to batch the next results of the operation.
-- * 'responseStatus' - The response status code.
mkListRepositoriesForApprovalRuleTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRepositoriesForApprovalRuleTemplateResponse
mkListRepositoriesForApprovalRuleTemplateResponse pResponseStatus_ =
  ListRepositoriesForApprovalRuleTemplateResponse'
    { repositoryNames =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of repository names that are associated with the specified approval rule template.
--
-- /Note:/ Consider using 'repositoryNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfartrsRepositoryNames :: Lens.Lens' ListRepositoriesForApprovalRuleTemplateResponse (Lude.Maybe [Lude.Text])
lrfartrsRepositoryNames = Lens.lens (repositoryNames :: ListRepositoriesForApprovalRuleTemplateResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {repositoryNames = a} :: ListRepositoriesForApprovalRuleTemplateResponse)
{-# DEPRECATED lrfartrsRepositoryNames "Use generic-lens or generic-optics with 'repositoryNames' instead." #-}

-- | An enumeration token that allows the operation to batch the next results of the operation.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfartrsNextToken :: Lens.Lens' ListRepositoriesForApprovalRuleTemplateResponse (Lude.Maybe Lude.Text)
lrfartrsNextToken = Lens.lens (nextToken :: ListRepositoriesForApprovalRuleTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRepositoriesForApprovalRuleTemplateResponse)
{-# DEPRECATED lrfartrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfartrsResponseStatus :: Lens.Lens' ListRepositoriesForApprovalRuleTemplateResponse Lude.Int
lrfartrsResponseStatus = Lens.lens (responseStatus :: ListRepositoriesForApprovalRuleTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRepositoriesForApprovalRuleTemplateResponse)
{-# DEPRECATED lrfartrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
