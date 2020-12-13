{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteWorkflow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a workflow.
module Network.AWS.Glue.DeleteWorkflow
  ( -- * Creating a request
    DeleteWorkflow (..),
    mkDeleteWorkflow,

    -- ** Request lenses
    dwName,

    -- * Destructuring the response
    DeleteWorkflowResponse (..),
    mkDeleteWorkflowResponse,

    -- ** Response lenses
    dwrsName,
    dwrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteWorkflow' smart constructor.
newtype DeleteWorkflow = DeleteWorkflow'
  { -- | Name of the workflow to be deleted.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteWorkflow' with the minimum fields required to make a request.
--
-- * 'name' - Name of the workflow to be deleted.
mkDeleteWorkflow ::
  -- | 'name'
  Lude.Text ->
  DeleteWorkflow
mkDeleteWorkflow pName_ = DeleteWorkflow' {name = pName_}

-- | Name of the workflow to be deleted.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwName :: Lens.Lens' DeleteWorkflow Lude.Text
dwName = Lens.lens (name :: DeleteWorkflow -> Lude.Text) (\s a -> s {name = a} :: DeleteWorkflow)
{-# DEPRECATED dwName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteWorkflow where
  type Rs DeleteWorkflow = DeleteWorkflowResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteWorkflowResponse'
            Lude.<$> (x Lude..?> "Name") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteWorkflow where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.DeleteWorkflow" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteWorkflow where
  toJSON DeleteWorkflow' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeleteWorkflow where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteWorkflow where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteWorkflowResponse' smart constructor.
data DeleteWorkflowResponse = DeleteWorkflowResponse'
  { -- | Name of the workflow specified in input.
    name :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteWorkflowResponse' with the minimum fields required to make a request.
--
-- * 'name' - Name of the workflow specified in input.
-- * 'responseStatus' - The response status code.
mkDeleteWorkflowResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteWorkflowResponse
mkDeleteWorkflowResponse pResponseStatus_ =
  DeleteWorkflowResponse'
    { name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Name of the workflow specified in input.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrsName :: Lens.Lens' DeleteWorkflowResponse (Lude.Maybe Lude.Text)
dwrsName = Lens.lens (name :: DeleteWorkflowResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DeleteWorkflowResponse)
{-# DEPRECATED dwrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrsResponseStatus :: Lens.Lens' DeleteWorkflowResponse Lude.Int
dwrsResponseStatus = Lens.lens (responseStatus :: DeleteWorkflowResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteWorkflowResponse)
{-# DEPRECATED dwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
