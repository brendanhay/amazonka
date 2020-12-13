{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    parPipelineName,
    parActionRevision,
    parActionName,
    parStageName,

    -- * Destructuring the response
    PutActionRevisionResponse (..),
    mkPutActionRevisionResponse,

    -- ** Response lenses
    parrsNewRevision,
    parrsPipelineExecutionId,
    parrsResponseStatus,
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
  { -- | The name of the pipeline that starts processing the revision to the source.
    pipelineName :: Lude.Text,
    -- | Represents information about the version (or revision) of an action.
    actionRevision :: ActionRevision,
    -- | The name of the action that processes the revision.
    actionName :: Lude.Text,
    -- | The name of the stage that contains the action that acts on the revision.
    stageName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutActionRevision' with the minimum fields required to make a request.
--
-- * 'pipelineName' - The name of the pipeline that starts processing the revision to the source.
-- * 'actionRevision' - Represents information about the version (or revision) of an action.
-- * 'actionName' - The name of the action that processes the revision.
-- * 'stageName' - The name of the stage that contains the action that acts on the revision.
mkPutActionRevision ::
  -- | 'pipelineName'
  Lude.Text ->
  -- | 'actionRevision'
  ActionRevision ->
  -- | 'actionName'
  Lude.Text ->
  -- | 'stageName'
  Lude.Text ->
  PutActionRevision
mkPutActionRevision
  pPipelineName_
  pActionRevision_
  pActionName_
  pStageName_ =
    PutActionRevision'
      { pipelineName = pPipelineName_,
        actionRevision = pActionRevision_,
        actionName = pActionName_,
        stageName = pStageName_
      }

-- | The name of the pipeline that starts processing the revision to the source.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parPipelineName :: Lens.Lens' PutActionRevision Lude.Text
parPipelineName = Lens.lens (pipelineName :: PutActionRevision -> Lude.Text) (\s a -> s {pipelineName = a} :: PutActionRevision)
{-# DEPRECATED parPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | Represents information about the version (or revision) of an action.
--
-- /Note:/ Consider using 'actionRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parActionRevision :: Lens.Lens' PutActionRevision ActionRevision
parActionRevision = Lens.lens (actionRevision :: PutActionRevision -> ActionRevision) (\s a -> s {actionRevision = a} :: PutActionRevision)
{-# DEPRECATED parActionRevision "Use generic-lens or generic-optics with 'actionRevision' instead." #-}

-- | The name of the action that processes the revision.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parActionName :: Lens.Lens' PutActionRevision Lude.Text
parActionName = Lens.lens (actionName :: PutActionRevision -> Lude.Text) (\s a -> s {actionName = a} :: PutActionRevision)
{-# DEPRECATED parActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | The name of the stage that contains the action that acts on the revision.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parStageName :: Lens.Lens' PutActionRevision Lude.Text
parStageName = Lens.lens (stageName :: PutActionRevision -> Lude.Text) (\s a -> s {stageName = a} :: PutActionRevision)
{-# DEPRECATED parStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

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
            Lude.Just ("actionRevision" Lude..= actionRevision),
            Lude.Just ("actionName" Lude..= actionName),
            Lude.Just ("stageName" Lude..= stageName)
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
  { -- | Indicates whether the artifact revision was previously used in an execution of the specified pipeline.
    newRevision :: Lude.Maybe Lude.Bool,
    -- | The ID of the current workflow state of the pipeline.
    pipelineExecutionId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
parrsNewRevision :: Lens.Lens' PutActionRevisionResponse (Lude.Maybe Lude.Bool)
parrsNewRevision = Lens.lens (newRevision :: PutActionRevisionResponse -> Lude.Maybe Lude.Bool) (\s a -> s {newRevision = a} :: PutActionRevisionResponse)
{-# DEPRECATED parrsNewRevision "Use generic-lens or generic-optics with 'newRevision' instead." #-}

-- | The ID of the current workflow state of the pipeline.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parrsPipelineExecutionId :: Lens.Lens' PutActionRevisionResponse (Lude.Maybe Lude.Text)
parrsPipelineExecutionId = Lens.lens (pipelineExecutionId :: PutActionRevisionResponse -> Lude.Maybe Lude.Text) (\s a -> s {pipelineExecutionId = a} :: PutActionRevisionResponse)
{-# DEPRECATED parrsPipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parrsResponseStatus :: Lens.Lens' PutActionRevisionResponse Lude.Int
parrsResponseStatus = Lens.lens (responseStatus :: PutActionRevisionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutActionRevisionResponse)
{-# DEPRECATED parrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
