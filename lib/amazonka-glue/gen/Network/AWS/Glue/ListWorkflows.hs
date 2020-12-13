{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ListWorkflows
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists names of workflows created in the account.
module Network.AWS.Glue.ListWorkflows
  ( -- * Creating a request
    ListWorkflows (..),
    mkListWorkflows,

    -- ** Request lenses
    lwNextToken,
    lwMaxResults,

    -- * Destructuring the response
    ListWorkflowsResponse (..),
    mkListWorkflowsResponse,

    -- ** Response lenses
    lwrsNextToken,
    lwrsWorkflows,
    lwrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListWorkflows' smart constructor.
data ListWorkflows = ListWorkflows'
  { -- | A continuation token, if this is a continuation request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum size of a list to return.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListWorkflows' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, if this is a continuation request.
-- * 'maxResults' - The maximum size of a list to return.
mkListWorkflows ::
  ListWorkflows
mkListWorkflows =
  ListWorkflows'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A continuation token, if this is a continuation request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwNextToken :: Lens.Lens' ListWorkflows (Lude.Maybe Lude.Text)
lwNextToken = Lens.lens (nextToken :: ListWorkflows -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListWorkflows)
{-# DEPRECATED lwNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum size of a list to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwMaxResults :: Lens.Lens' ListWorkflows (Lude.Maybe Lude.Natural)
lwMaxResults = Lens.lens (maxResults :: ListWorkflows -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListWorkflows)
{-# DEPRECATED lwMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListWorkflows where
  type Rs ListWorkflows = ListWorkflowsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListWorkflowsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Workflows")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListWorkflows where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.ListWorkflows" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListWorkflows where
  toJSON ListWorkflows' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListWorkflows where
  toPath = Lude.const "/"

instance Lude.ToQuery ListWorkflows where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListWorkflowsResponse' smart constructor.
data ListWorkflowsResponse = ListWorkflowsResponse'
  { -- | A continuation token, if not all workflow names have been returned.
    nextToken :: Lude.Maybe Lude.Text,
    -- | List of names of workflows in the account.
    workflows :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListWorkflowsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, if not all workflow names have been returned.
-- * 'workflows' - List of names of workflows in the account.
-- * 'responseStatus' - The response status code.
mkListWorkflowsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListWorkflowsResponse
mkListWorkflowsResponse pResponseStatus_ =
  ListWorkflowsResponse'
    { nextToken = Lude.Nothing,
      workflows = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A continuation token, if not all workflow names have been returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwrsNextToken :: Lens.Lens' ListWorkflowsResponse (Lude.Maybe Lude.Text)
lwrsNextToken = Lens.lens (nextToken :: ListWorkflowsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListWorkflowsResponse)
{-# DEPRECATED lwrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | List of names of workflows in the account.
--
-- /Note:/ Consider using 'workflows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwrsWorkflows :: Lens.Lens' ListWorkflowsResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
lwrsWorkflows = Lens.lens (workflows :: ListWorkflowsResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {workflows = a} :: ListWorkflowsResponse)
{-# DEPRECATED lwrsWorkflows "Use generic-lens or generic-optics with 'workflows' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwrsResponseStatus :: Lens.Lens' ListWorkflowsResponse Lude.Int
lwrsResponseStatus = Lens.lens (responseStatus :: ListWorkflowsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListWorkflowsResponse)
{-# DEPRECATED lwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
