{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.PutActionRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information to AWS CodePipeline about new revisions to a source.
module Network.AWS.CodePipeline.PutActionRevision
  ( -- * Creating a request
    PutActionRevision (..),
    mkPutActionRevision,

    -- ** Request lenses
    pPipelineName,
    pStageName,
    pActionName,
    pActionRevision,

    -- * Destructuring the response
    PutActionRevisionResponse (..),
    mkPutActionRevisionResponse,

    -- ** Response lenses
    prsNewRevision,
    prsPipelineExecutionId,
    prsResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @PutActionRevision@ action.
--
-- /See:/ 'mkPutActionRevision' smart constructor.
data PutActionRevision = PutActionRevision'
  { pipelineName ::
      Lude.Text,
    stageName :: Lude.Text,
    actionName :: Lude.Text,
    actionRevision :: ActionRevision
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutActionRevision' with the minimum fields required to make a request.
--
-- * 'actionName' - The name of the action that processes the revision.
-- * 'actionRevision' - Represents information about the version (or revision) of an action.
-- * 'pipelineName' - The name of the pipeline that starts processing the revision to the source.
-- * 'stageName' - The name of the stage that contains the action that acts on the revision.
mkPutActionRevision ::
  -- | 'pipelineName'
  Lude.Text ->
  -- | 'stageName'
  Lude.Text ->
  -- | 'actionName'
  Lude.Text ->
  -- | 'actionRevision'
  ActionRevision ->
  PutActionRevision
mkPutActionRevision
  pPipelineName_
  pStageName_
  pActionName_
  pActionRevision_ =
    PutActionRevision'
      { pipelineName = pPipelineName_,
        stageName = pStageName_,
        actionName = pActionName_,
        actionRevision = pActionRevision_
      }

-- | The name of the pipeline that starts processing the revision to the source.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPipelineName :: Lens.Lens' PutActionRevision Lude.Text
pPipelineName = Lens.lens (pipelineName :: PutActionRevision -> Lude.Text) (\s a -> s {pipelineName = a} :: PutActionRevision)
{-# DEPRECATED pPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The name of the stage that contains the action that acts on the revision.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pStageName :: Lens.Lens' PutActionRevision Lude.Text
pStageName = Lens.lens (stageName :: PutActionRevision -> Lude.Text) (\s a -> s {stageName = a} :: PutActionRevision)
{-# DEPRECATED pStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

-- | The name of the action that processes the revision.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pActionName :: Lens.Lens' PutActionRevision Lude.Text
pActionName = Lens.lens (actionName :: PutActionRevision -> Lude.Text) (\s a -> s {actionName = a} :: PutActionRevision)
{-# DEPRECATED pActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | Represents information about the version (or revision) of an action.
--
-- /Note:/ Consider using 'actionRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pActionRevision :: Lens.Lens' PutActionRevision ActionRevision
pActionRevision = Lens.lens (actionRevision :: PutActionRevision -> ActionRevision) (\s a -> s {actionRevision = a} :: PutActionRevision)
{-# DEPRECATED pActionRevision "Use generic-lens or generic-optics with 'actionRevision' instead." #-}

instance Lude.AWSRequest PutActionRevision where
  type Rs PutActionRevision = PutActionRevisionResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutActionRevisionResponse'
            Lude.<$> (x Lude..?> "newRevision")
            Lude.<*> (x Lude..?> "pipelineExecutionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutActionRevision where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.PutActionRevision" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutActionRevision where
  toJSON PutActionRevision' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pipelineName" Lude..= pipelineName),
            Lude.Just ("stageName" Lude..= stageName),
            Lude.Just ("actionName" Lude..= actionName),
            Lude.Just ("actionRevision" Lude..= actionRevision)
          ]
      )

instance Lude.ToPath PutActionRevision where
  toPath = Lude.const "/"

instance Lude.ToQuery PutActionRevision where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @PutActionRevision@ action.
--
-- /See:/ 'mkPutActionRevisionResponse' smart constructor.
data PutActionRevisionResponse = PutActionRevisionResponse'
  { newRevision ::
      Lude.Maybe Lude.Bool,
    pipelineExecutionId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'PutActionRevisionResponse' with the minimum fields required to make a request.
--
-- * 'newRevision' - Indicates whether the artifact revision was previously used in an execution of the specified pipeline.
-- * 'pipelineExecutionId' - The ID of the current workflow state of the pipeline.
-- * 'responseStatus' - The response status code.
mkPutActionRevisionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutActionRevisionResponse
mkPutActionRevisionResponse pResponseStatus_ =
  PutActionRevisionResponse'
    { newRevision = Lude.Nothing,
      pipelineExecutionId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Indicates whether the artifact revision was previously used in an execution of the specified pipeline.
--
-- /Note:/ Consider using 'newRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsNewRevision :: Lens.Lens' PutActionRevisionResponse (Lude.Maybe Lude.Bool)
prsNewRevision = Lens.lens (newRevision :: PutActionRevisionResponse -> Lude.Maybe Lude.Bool) (\s a -> s {newRevision = a} :: PutActionRevisionResponse)
{-# DEPRECATED prsNewRevision "Use generic-lens or generic-optics with 'newRevision' instead." #-}

-- | The ID of the current workflow state of the pipeline.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsPipelineExecutionId :: Lens.Lens' PutActionRevisionResponse (Lude.Maybe Lude.Text)
prsPipelineExecutionId = Lens.lens (pipelineExecutionId :: PutActionRevisionResponse -> Lude.Maybe Lude.Text) (\s a -> s {pipelineExecutionId = a} :: PutActionRevisionResponse)
{-# DEPRECATED prsPipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsResponseStatus :: Lens.Lens' PutActionRevisionResponse Lude.Int
prsResponseStatus = Lens.lens (responseStatus :: PutActionRevisionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutActionRevisionResponse)
{-# DEPRECATED prsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
