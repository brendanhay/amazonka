{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.StopNotebookExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a notebook execution.
module Network.AWS.EMR.StopNotebookExecution
  ( -- * Creating a request
    StopNotebookExecution (..),
    mkStopNotebookExecution,

    -- ** Request lenses
    sneNotebookExecutionId,

    -- * Destructuring the response
    StopNotebookExecutionResponse (..),
    mkStopNotebookExecutionResponse,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopNotebookExecution' smart constructor.
newtype StopNotebookExecution = StopNotebookExecution'
  { notebookExecutionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopNotebookExecution' with the minimum fields required to make a request.
--
-- * 'notebookExecutionId' - The unique identifier of the notebook execution.
mkStopNotebookExecution ::
  -- | 'notebookExecutionId'
  Lude.Text ->
  StopNotebookExecution
mkStopNotebookExecution pNotebookExecutionId_ =
  StopNotebookExecution'
    { notebookExecutionId =
        pNotebookExecutionId_
    }

-- | The unique identifier of the notebook execution.
--
-- /Note:/ Consider using 'notebookExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sneNotebookExecutionId :: Lens.Lens' StopNotebookExecution Lude.Text
sneNotebookExecutionId = Lens.lens (notebookExecutionId :: StopNotebookExecution -> Lude.Text) (\s a -> s {notebookExecutionId = a} :: StopNotebookExecution)
{-# DEPRECATED sneNotebookExecutionId "Use generic-lens or generic-optics with 'notebookExecutionId' instead." #-}

instance Lude.AWSRequest StopNotebookExecution where
  type Rs StopNotebookExecution = StopNotebookExecutionResponse
  request = Req.postJSON emrService
  response = Res.receiveNull StopNotebookExecutionResponse'

instance Lude.ToHeaders StopNotebookExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.StopNotebookExecution" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopNotebookExecution where
  toJSON StopNotebookExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("NotebookExecutionId" Lude..= notebookExecutionId)]
      )

instance Lude.ToPath StopNotebookExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery StopNotebookExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopNotebookExecutionResponse' smart constructor.
data StopNotebookExecutionResponse = StopNotebookExecutionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopNotebookExecutionResponse' with the minimum fields required to make a request.
mkStopNotebookExecutionResponse ::
  StopNotebookExecutionResponse
mkStopNotebookExecutionResponse = StopNotebookExecutionResponse'
