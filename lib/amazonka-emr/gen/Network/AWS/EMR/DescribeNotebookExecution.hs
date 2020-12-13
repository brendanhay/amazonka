{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.DescribeNotebookExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details of a notebook execution.
module Network.AWS.EMR.DescribeNotebookExecution
  ( -- * Creating a request
    DescribeNotebookExecution (..),
    mkDescribeNotebookExecution,

    -- ** Request lenses
    dneNotebookExecutionId,

    -- * Destructuring the response
    DescribeNotebookExecutionResponse (..),
    mkDescribeNotebookExecutionResponse,

    -- ** Response lenses
    dnersNotebookExecution,
    dnersResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeNotebookExecution' smart constructor.
newtype DescribeNotebookExecution = DescribeNotebookExecution'
  { -- | The unique identifier of the notebook execution.
    notebookExecutionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNotebookExecution' with the minimum fields required to make a request.
--
-- * 'notebookExecutionId' - The unique identifier of the notebook execution.
mkDescribeNotebookExecution ::
  -- | 'notebookExecutionId'
  Lude.Text ->
  DescribeNotebookExecution
mkDescribeNotebookExecution pNotebookExecutionId_ =
  DescribeNotebookExecution'
    { notebookExecutionId =
        pNotebookExecutionId_
    }

-- | The unique identifier of the notebook execution.
--
-- /Note:/ Consider using 'notebookExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dneNotebookExecutionId :: Lens.Lens' DescribeNotebookExecution Lude.Text
dneNotebookExecutionId = Lens.lens (notebookExecutionId :: DescribeNotebookExecution -> Lude.Text) (\s a -> s {notebookExecutionId = a} :: DescribeNotebookExecution)
{-# DEPRECATED dneNotebookExecutionId "Use generic-lens or generic-optics with 'notebookExecutionId' instead." #-}

instance Lude.AWSRequest DescribeNotebookExecution where
  type
    Rs DescribeNotebookExecution =
      DescribeNotebookExecutionResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeNotebookExecutionResponse'
            Lude.<$> (x Lude..?> "NotebookExecution")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeNotebookExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.DescribeNotebookExecution" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeNotebookExecution where
  toJSON DescribeNotebookExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("NotebookExecutionId" Lude..= notebookExecutionId)]
      )

instance Lude.ToPath DescribeNotebookExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeNotebookExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeNotebookExecutionResponse' smart constructor.
data DescribeNotebookExecutionResponse = DescribeNotebookExecutionResponse'
  { -- | Properties of the notebook execution.
    notebookExecution :: Lude.Maybe NotebookExecution,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNotebookExecutionResponse' with the minimum fields required to make a request.
--
-- * 'notebookExecution' - Properties of the notebook execution.
-- * 'responseStatus' - The response status code.
mkDescribeNotebookExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeNotebookExecutionResponse
mkDescribeNotebookExecutionResponse pResponseStatus_ =
  DescribeNotebookExecutionResponse'
    { notebookExecution =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Properties of the notebook execution.
--
-- /Note:/ Consider using 'notebookExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnersNotebookExecution :: Lens.Lens' DescribeNotebookExecutionResponse (Lude.Maybe NotebookExecution)
dnersNotebookExecution = Lens.lens (notebookExecution :: DescribeNotebookExecutionResponse -> Lude.Maybe NotebookExecution) (\s a -> s {notebookExecution = a} :: DescribeNotebookExecutionResponse)
{-# DEPRECATED dnersNotebookExecution "Use generic-lens or generic-optics with 'notebookExecution' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnersResponseStatus :: Lens.Lens' DescribeNotebookExecutionResponse Lude.Int
dnersResponseStatus = Lens.lens (responseStatus :: DescribeNotebookExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeNotebookExecutionResponse)
{-# DEPRECATED dnersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
