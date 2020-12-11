{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.CreatePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a pipeline.
module Network.AWS.CodePipeline.CreatePipeline
  ( -- * Creating a request
    CreatePipeline (..),
    mkCreatePipeline,

    -- ** Request lenses
    cpTags,
    cpPipeline,

    -- * Destructuring the response
    CreatePipelineResponse (..),
    mkCreatePipelineResponse,

    -- ** Response lenses
    cprsPipeline,
    cprsTags,
    cprsResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @CreatePipeline@ action.
--
-- /See:/ 'mkCreatePipeline' smart constructor.
data CreatePipeline = CreatePipeline'
  { tags :: Lude.Maybe [Tag],
    pipeline :: PipelineDeclaration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePipeline' with the minimum fields required to make a request.
--
-- * 'pipeline' - Represents the structure of actions and stages to be performed in the pipeline.
-- * 'tags' - The tags for the pipeline.
mkCreatePipeline ::
  -- | 'pipeline'
  PipelineDeclaration ->
  CreatePipeline
mkCreatePipeline pPipeline_ =
  CreatePipeline' {tags = Lude.Nothing, pipeline = pPipeline_}

-- | The tags for the pipeline.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreatePipeline (Lude.Maybe [Tag])
cpTags = Lens.lens (tags :: CreatePipeline -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreatePipeline)
{-# DEPRECATED cpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Represents the structure of actions and stages to be performed in the pipeline.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPipeline :: Lens.Lens' CreatePipeline PipelineDeclaration
cpPipeline = Lens.lens (pipeline :: CreatePipeline -> PipelineDeclaration) (\s a -> s {pipeline = a} :: CreatePipeline)
{-# DEPRECATED cpPipeline "Use generic-lens or generic-optics with 'pipeline' instead." #-}

instance Lude.AWSRequest CreatePipeline where
  type Rs CreatePipeline = CreatePipelineResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePipelineResponse'
            Lude.<$> (x Lude..?> "pipeline")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePipeline where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.CreatePipeline" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePipeline where
  toJSON CreatePipeline' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("pipeline" Lude..= pipeline)
          ]
      )

instance Lude.ToPath CreatePipeline where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePipeline where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @CreatePipeline@ action.
--
-- /See:/ 'mkCreatePipelineResponse' smart constructor.
data CreatePipelineResponse = CreatePipelineResponse'
  { pipeline ::
      Lude.Maybe PipelineDeclaration,
    tags :: Lude.Maybe [Tag],
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

-- | Creates a value of 'CreatePipelineResponse' with the minimum fields required to make a request.
--
-- * 'pipeline' - Represents the structure of actions and stages to be performed in the pipeline.
-- * 'responseStatus' - The response status code.
-- * 'tags' - Specifies the tags applied to the pipeline.
mkCreatePipelineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePipelineResponse
mkCreatePipelineResponse pResponseStatus_ =
  CreatePipelineResponse'
    { pipeline = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Represents the structure of actions and stages to be performed in the pipeline.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsPipeline :: Lens.Lens' CreatePipelineResponse (Lude.Maybe PipelineDeclaration)
cprsPipeline = Lens.lens (pipeline :: CreatePipelineResponse -> Lude.Maybe PipelineDeclaration) (\s a -> s {pipeline = a} :: CreatePipelineResponse)
{-# DEPRECATED cprsPipeline "Use generic-lens or generic-optics with 'pipeline' instead." #-}

-- | Specifies the tags applied to the pipeline.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsTags :: Lens.Lens' CreatePipelineResponse (Lude.Maybe [Tag])
cprsTags = Lens.lens (tags :: CreatePipelineResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreatePipelineResponse)
{-# DEPRECATED cprsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsResponseStatus :: Lens.Lens' CreatePipelineResponse Lude.Int
cprsResponseStatus = Lens.lens (responseStatus :: CreatePipelineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePipelineResponse)
{-# DEPRECATED cprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
