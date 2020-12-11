{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetWorkflow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves resource metadata for a workflow.
module Network.AWS.Glue.GetWorkflow
  ( -- * Creating a request
    GetWorkflow (..),
    mkGetWorkflow,

    -- ** Request lenses
    gwIncludeGraph,
    gwName,

    -- * Destructuring the response
    GetWorkflowResponse (..),
    mkGetWorkflowResponse,

    -- ** Response lenses
    gwrsWorkflow,
    gwrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetWorkflow' smart constructor.
data GetWorkflow = GetWorkflow'
  { includeGraph ::
      Lude.Maybe Lude.Bool,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetWorkflow' with the minimum fields required to make a request.
--
-- * 'includeGraph' - Specifies whether to include a graph when returning the workflow resource metadata.
-- * 'name' - The name of the workflow to retrieve.
mkGetWorkflow ::
  -- | 'name'
  Lude.Text ->
  GetWorkflow
mkGetWorkflow pName_ =
  GetWorkflow' {includeGraph = Lude.Nothing, name = pName_}

-- | Specifies whether to include a graph when returning the workflow resource metadata.
--
-- /Note:/ Consider using 'includeGraph' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwIncludeGraph :: Lens.Lens' GetWorkflow (Lude.Maybe Lude.Bool)
gwIncludeGraph = Lens.lens (includeGraph :: GetWorkflow -> Lude.Maybe Lude.Bool) (\s a -> s {includeGraph = a} :: GetWorkflow)
{-# DEPRECATED gwIncludeGraph "Use generic-lens or generic-optics with 'includeGraph' instead." #-}

-- | The name of the workflow to retrieve.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwName :: Lens.Lens' GetWorkflow Lude.Text
gwName = Lens.lens (name :: GetWorkflow -> Lude.Text) (\s a -> s {name = a} :: GetWorkflow)
{-# DEPRECATED gwName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetWorkflow where
  type Rs GetWorkflow = GetWorkflowResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetWorkflowResponse'
            Lude.<$> (x Lude..?> "Workflow") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetWorkflow where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.GetWorkflow" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetWorkflow where
  toJSON GetWorkflow' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IncludeGraph" Lude..=) Lude.<$> includeGraph,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath GetWorkflow where
  toPath = Lude.const "/"

instance Lude.ToQuery GetWorkflow where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetWorkflowResponse' smart constructor.
data GetWorkflowResponse = GetWorkflowResponse'
  { workflow ::
      Lude.Maybe Workflow,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetWorkflowResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'workflow' - The resource metadata for the workflow.
mkGetWorkflowResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetWorkflowResponse
mkGetWorkflowResponse pResponseStatus_ =
  GetWorkflowResponse'
    { workflow = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The resource metadata for the workflow.
--
-- /Note:/ Consider using 'workflow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrsWorkflow :: Lens.Lens' GetWorkflowResponse (Lude.Maybe Workflow)
gwrsWorkflow = Lens.lens (workflow :: GetWorkflowResponse -> Lude.Maybe Workflow) (\s a -> s {workflow = a} :: GetWorkflowResponse)
{-# DEPRECATED gwrsWorkflow "Use generic-lens or generic-optics with 'workflow' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrsResponseStatus :: Lens.Lens' GetWorkflowResponse Lude.Int
gwrsResponseStatus = Lens.lens (responseStatus :: GetWorkflowResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetWorkflowResponse)
{-# DEPRECATED gwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
