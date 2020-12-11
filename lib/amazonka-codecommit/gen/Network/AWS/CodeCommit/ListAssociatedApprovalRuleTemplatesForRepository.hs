{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.ListAssociatedApprovalRuleTemplatesForRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all approval rule templates that are associated with a specified repository.
module Network.AWS.CodeCommit.ListAssociatedApprovalRuleTemplatesForRepository
  ( -- * Creating a request
    ListAssociatedApprovalRuleTemplatesForRepository (..),
    mkListAssociatedApprovalRuleTemplatesForRepository,

    -- ** Request lenses
    laartfrNextToken,
    laartfrMaxResults,
    laartfrRepositoryName,

    -- * Destructuring the response
    ListAssociatedApprovalRuleTemplatesForRepositoryResponse (..),
    mkListAssociatedApprovalRuleTemplatesForRepositoryResponse,

    -- ** Response lenses
    laartfrrsNextToken,
    laartfrrsApprovalRuleTemplateNames,
    laartfrrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAssociatedApprovalRuleTemplatesForRepository' smart constructor.
data ListAssociatedApprovalRuleTemplatesForRepository = ListAssociatedApprovalRuleTemplatesForRepository'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    maxResults ::
      Lude.Maybe
        Lude.Int,
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

-- | Creates a value of 'ListAssociatedApprovalRuleTemplatesForRepository' with the minimum fields required to make a request.
--
-- * 'maxResults' - A non-zero, non-negative integer used to limit the number of returned results.
-- * 'nextToken' - An enumeration token that, when provided in a request, returns the next batch of the results.
-- * 'repositoryName' - The name of the repository for which you want to list all associated approval rule templates.
mkListAssociatedApprovalRuleTemplatesForRepository ::
  -- | 'repositoryName'
  Lude.Text ->
  ListAssociatedApprovalRuleTemplatesForRepository
mkListAssociatedApprovalRuleTemplatesForRepository pRepositoryName_ =
  ListAssociatedApprovalRuleTemplatesForRepository'
    { nextToken =
        Lude.Nothing,
      maxResults = Lude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laartfrNextToken :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepository (Lude.Maybe Lude.Text)
laartfrNextToken = Lens.lens (nextToken :: ListAssociatedApprovalRuleTemplatesForRepository -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAssociatedApprovalRuleTemplatesForRepository)
{-# DEPRECATED laartfrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A non-zero, non-negative integer used to limit the number of returned results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laartfrMaxResults :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepository (Lude.Maybe Lude.Int)
laartfrMaxResults = Lens.lens (maxResults :: ListAssociatedApprovalRuleTemplatesForRepository -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListAssociatedApprovalRuleTemplatesForRepository)
{-# DEPRECATED laartfrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the repository for which you want to list all associated approval rule templates.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laartfrRepositoryName :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepository Lude.Text
laartfrRepositoryName = Lens.lens (repositoryName :: ListAssociatedApprovalRuleTemplatesForRepository -> Lude.Text) (\s a -> s {repositoryName = a} :: ListAssociatedApprovalRuleTemplatesForRepository)
{-# DEPRECATED laartfrRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance
  Lude.AWSRequest
    ListAssociatedApprovalRuleTemplatesForRepository
  where
  type
    Rs ListAssociatedApprovalRuleTemplatesForRepository =
      ListAssociatedApprovalRuleTemplatesForRepositoryResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAssociatedApprovalRuleTemplatesForRepositoryResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "approvalRuleTemplateNames" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    ListAssociatedApprovalRuleTemplatesForRepository
  where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.ListAssociatedApprovalRuleTemplatesForRepository" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance
  Lude.ToJSON
    ListAssociatedApprovalRuleTemplatesForRepository
  where
  toJSON ListAssociatedApprovalRuleTemplatesForRepository' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance
  Lude.ToPath
    ListAssociatedApprovalRuleTemplatesForRepository
  where
  toPath = Lude.const "/"

instance
  Lude.ToQuery
    ListAssociatedApprovalRuleTemplatesForRepository
  where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAssociatedApprovalRuleTemplatesForRepositoryResponse' smart constructor.
data ListAssociatedApprovalRuleTemplatesForRepositoryResponse = ListAssociatedApprovalRuleTemplatesForRepositoryResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    approvalRuleTemplateNames ::
      Lude.Maybe
        [Lude.Text],
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'ListAssociatedApprovalRuleTemplatesForRepositoryResponse' with the minimum fields required to make a request.
--
-- * 'approvalRuleTemplateNames' - The names of all approval rule templates associated with the repository.
-- * 'nextToken' - An enumeration token that allows the operation to batch the next results of the operation.
-- * 'responseStatus' - The response status code.
mkListAssociatedApprovalRuleTemplatesForRepositoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAssociatedApprovalRuleTemplatesForRepositoryResponse
mkListAssociatedApprovalRuleTemplatesForRepositoryResponse
  pResponseStatus_ =
    ListAssociatedApprovalRuleTemplatesForRepositoryResponse'
      { nextToken =
          Lude.Nothing,
        approvalRuleTemplateNames =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | An enumeration token that allows the operation to batch the next results of the operation.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laartfrrsNextToken :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepositoryResponse (Lude.Maybe Lude.Text)
laartfrrsNextToken = Lens.lens (nextToken :: ListAssociatedApprovalRuleTemplatesForRepositoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAssociatedApprovalRuleTemplatesForRepositoryResponse)
{-# DEPRECATED laartfrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The names of all approval rule templates associated with the repository.
--
-- /Note:/ Consider using 'approvalRuleTemplateNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laartfrrsApprovalRuleTemplateNames :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepositoryResponse (Lude.Maybe [Lude.Text])
laartfrrsApprovalRuleTemplateNames = Lens.lens (approvalRuleTemplateNames :: ListAssociatedApprovalRuleTemplatesForRepositoryResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {approvalRuleTemplateNames = a} :: ListAssociatedApprovalRuleTemplatesForRepositoryResponse)
{-# DEPRECATED laartfrrsApprovalRuleTemplateNames "Use generic-lens or generic-optics with 'approvalRuleTemplateNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laartfrrsResponseStatus :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepositoryResponse Lude.Int
laartfrrsResponseStatus = Lens.lens (responseStatus :: ListAssociatedApprovalRuleTemplatesForRepositoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAssociatedApprovalRuleTemplatesForRepositoryResponse)
{-# DEPRECATED laartfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
