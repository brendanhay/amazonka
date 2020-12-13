{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.ListApprovalRuleTemplates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all approval rule templates in the specified AWS Region in your AWS account. If an AWS Region is not specified, the AWS Region where you are signed in is used.
module Network.AWS.CodeCommit.ListApprovalRuleTemplates
  ( -- * Creating a request
    ListApprovalRuleTemplates (..),
    mkListApprovalRuleTemplates,

    -- ** Request lenses
    lartNextToken,
    lartMaxResults,

    -- * Destructuring the response
    ListApprovalRuleTemplatesResponse (..),
    mkListApprovalRuleTemplatesResponse,

    -- ** Response lenses
    lartrsNextToken,
    lartrsApprovalRuleTemplateNames,
    lartrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListApprovalRuleTemplates' smart constructor.
data ListApprovalRuleTemplates = ListApprovalRuleTemplates'
  { -- | An enumeration token that, when provided in a request, returns the next batch of the results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A non-zero, non-negative integer used to limit the number of returned results.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListApprovalRuleTemplates' with the minimum fields required to make a request.
--
-- * 'nextToken' - An enumeration token that, when provided in a request, returns the next batch of the results.
-- * 'maxResults' - A non-zero, non-negative integer used to limit the number of returned results.
mkListApprovalRuleTemplates ::
  ListApprovalRuleTemplates
mkListApprovalRuleTemplates =
  ListApprovalRuleTemplates'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lartNextToken :: Lens.Lens' ListApprovalRuleTemplates (Lude.Maybe Lude.Text)
lartNextToken = Lens.lens (nextToken :: ListApprovalRuleTemplates -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListApprovalRuleTemplates)
{-# DEPRECATED lartNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A non-zero, non-negative integer used to limit the number of returned results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lartMaxResults :: Lens.Lens' ListApprovalRuleTemplates (Lude.Maybe Lude.Int)
lartMaxResults = Lens.lens (maxResults :: ListApprovalRuleTemplates -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListApprovalRuleTemplates)
{-# DEPRECATED lartMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListApprovalRuleTemplates where
  type
    Rs ListApprovalRuleTemplates =
      ListApprovalRuleTemplatesResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListApprovalRuleTemplatesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "approvalRuleTemplateNames" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListApprovalRuleTemplates where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.ListApprovalRuleTemplates" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListApprovalRuleTemplates where
  toJSON ListApprovalRuleTemplates' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListApprovalRuleTemplates where
  toPath = Lude.const "/"

instance Lude.ToQuery ListApprovalRuleTemplates where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListApprovalRuleTemplatesResponse' smart constructor.
data ListApprovalRuleTemplatesResponse = ListApprovalRuleTemplatesResponse'
  { -- | An enumeration token that allows the operation to batch the next results of the operation.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The names of all the approval rule templates found in the AWS Region for your AWS account.
    approvalRuleTemplateNames :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListApprovalRuleTemplatesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - An enumeration token that allows the operation to batch the next results of the operation.
-- * 'approvalRuleTemplateNames' - The names of all the approval rule templates found in the AWS Region for your AWS account.
-- * 'responseStatus' - The response status code.
mkListApprovalRuleTemplatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListApprovalRuleTemplatesResponse
mkListApprovalRuleTemplatesResponse pResponseStatus_ =
  ListApprovalRuleTemplatesResponse'
    { nextToken = Lude.Nothing,
      approvalRuleTemplateNames = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An enumeration token that allows the operation to batch the next results of the operation.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lartrsNextToken :: Lens.Lens' ListApprovalRuleTemplatesResponse (Lude.Maybe Lude.Text)
lartrsNextToken = Lens.lens (nextToken :: ListApprovalRuleTemplatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListApprovalRuleTemplatesResponse)
{-# DEPRECATED lartrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The names of all the approval rule templates found in the AWS Region for your AWS account.
--
-- /Note:/ Consider using 'approvalRuleTemplateNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lartrsApprovalRuleTemplateNames :: Lens.Lens' ListApprovalRuleTemplatesResponse (Lude.Maybe [Lude.Text])
lartrsApprovalRuleTemplateNames = Lens.lens (approvalRuleTemplateNames :: ListApprovalRuleTemplatesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {approvalRuleTemplateNames = a} :: ListApprovalRuleTemplatesResponse)
{-# DEPRECATED lartrsApprovalRuleTemplateNames "Use generic-lens or generic-optics with 'approvalRuleTemplateNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lartrsResponseStatus :: Lens.Lens' ListApprovalRuleTemplatesResponse Lude.Int
lartrsResponseStatus = Lens.lens (responseStatus :: ListApprovalRuleTemplatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListApprovalRuleTemplatesResponse)
{-# DEPRECATED lartrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
