{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchGetWorkflows
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resource metadata for a given list of workflow names. After calling the @ListWorkflows@ operation, you can call this operation to access the data to which you have been granted permissions. This operation supports all IAM permissions, including permission conditions that uses tags.
module Network.AWS.Glue.BatchGetWorkflows
  ( -- * Creating a request
    BatchGetWorkflows (..),
    mkBatchGetWorkflows,

    -- ** Request lenses
    bgwIncludeGraph,
    bgwNames,

    -- * Destructuring the response
    BatchGetWorkflowsResponse (..),
    mkBatchGetWorkflowsResponse,

    -- ** Response lenses
    bgwrsMissingWorkflows,
    bgwrsWorkflows,
    bgwrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchGetWorkflows' smart constructor.
data BatchGetWorkflows = BatchGetWorkflows'
  { -- | Specifies whether to include a graph when returning the workflow resource metadata.
    includeGraph :: Lude.Maybe Lude.Bool,
    -- | A list of workflow names, which may be the names returned from the @ListWorkflows@ operation.
    names :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetWorkflows' with the minimum fields required to make a request.
--
-- * 'includeGraph' - Specifies whether to include a graph when returning the workflow resource metadata.
-- * 'names' - A list of workflow names, which may be the names returned from the @ListWorkflows@ operation.
mkBatchGetWorkflows ::
  -- | 'names'
  Lude.NonEmpty Lude.Text ->
  BatchGetWorkflows
mkBatchGetWorkflows pNames_ =
  BatchGetWorkflows' {includeGraph = Lude.Nothing, names = pNames_}

-- | Specifies whether to include a graph when returning the workflow resource metadata.
--
-- /Note:/ Consider using 'includeGraph' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgwIncludeGraph :: Lens.Lens' BatchGetWorkflows (Lude.Maybe Lude.Bool)
bgwIncludeGraph = Lens.lens (includeGraph :: BatchGetWorkflows -> Lude.Maybe Lude.Bool) (\s a -> s {includeGraph = a} :: BatchGetWorkflows)
{-# DEPRECATED bgwIncludeGraph "Use generic-lens or generic-optics with 'includeGraph' instead." #-}

-- | A list of workflow names, which may be the names returned from the @ListWorkflows@ operation.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgwNames :: Lens.Lens' BatchGetWorkflows (Lude.NonEmpty Lude.Text)
bgwNames = Lens.lens (names :: BatchGetWorkflows -> Lude.NonEmpty Lude.Text) (\s a -> s {names = a} :: BatchGetWorkflows)
{-# DEPRECATED bgwNames "Use generic-lens or generic-optics with 'names' instead." #-}

instance Lude.AWSRequest BatchGetWorkflows where
  type Rs BatchGetWorkflows = BatchGetWorkflowsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetWorkflowsResponse'
            Lude.<$> (x Lude..?> "MissingWorkflows")
            Lude.<*> (x Lude..?> "Workflows")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetWorkflows where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.BatchGetWorkflows" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetWorkflows where
  toJSON BatchGetWorkflows' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IncludeGraph" Lude..=) Lude.<$> includeGraph,
            Lude.Just ("Names" Lude..= names)
          ]
      )

instance Lude.ToPath BatchGetWorkflows where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetWorkflows where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchGetWorkflowsResponse' smart constructor.
data BatchGetWorkflowsResponse = BatchGetWorkflowsResponse'
  { -- | A list of names of workflows not found.
    missingWorkflows :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | A list of workflow resource metadata.
    workflows :: Lude.Maybe (Lude.NonEmpty Workflow),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetWorkflowsResponse' with the minimum fields required to make a request.
--
-- * 'missingWorkflows' - A list of names of workflows not found.
-- * 'workflows' - A list of workflow resource metadata.
-- * 'responseStatus' - The response status code.
mkBatchGetWorkflowsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetWorkflowsResponse
mkBatchGetWorkflowsResponse pResponseStatus_ =
  BatchGetWorkflowsResponse'
    { missingWorkflows = Lude.Nothing,
      workflows = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of names of workflows not found.
--
-- /Note:/ Consider using 'missingWorkflows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgwrsMissingWorkflows :: Lens.Lens' BatchGetWorkflowsResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
bgwrsMissingWorkflows = Lens.lens (missingWorkflows :: BatchGetWorkflowsResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {missingWorkflows = a} :: BatchGetWorkflowsResponse)
{-# DEPRECATED bgwrsMissingWorkflows "Use generic-lens or generic-optics with 'missingWorkflows' instead." #-}

-- | A list of workflow resource metadata.
--
-- /Note:/ Consider using 'workflows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgwrsWorkflows :: Lens.Lens' BatchGetWorkflowsResponse (Lude.Maybe (Lude.NonEmpty Workflow))
bgwrsWorkflows = Lens.lens (workflows :: BatchGetWorkflowsResponse -> Lude.Maybe (Lude.NonEmpty Workflow)) (\s a -> s {workflows = a} :: BatchGetWorkflowsResponse)
{-# DEPRECATED bgwrsWorkflows "Use generic-lens or generic-optics with 'workflows' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgwrsResponseStatus :: Lens.Lens' BatchGetWorkflowsResponse Lude.Int
bgwrsResponseStatus = Lens.lens (responseStatus :: BatchGetWorkflowsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetWorkflowsResponse)
{-# DEPRECATED bgwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
