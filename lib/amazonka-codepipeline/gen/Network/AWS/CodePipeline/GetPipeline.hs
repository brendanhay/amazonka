{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.GetPipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the metadata, structure, stages, and actions of a pipeline. Can be used to return the entire structure of a pipeline in JSON format, which can then be modified and used to update the pipeline structure with 'UpdatePipeline' .
module Network.AWS.CodePipeline.GetPipeline
  ( -- * Creating a request
    GetPipeline (..),
    mkGetPipeline,

    -- ** Request lenses
    gpVersion,
    gpName,

    -- * Destructuring the response
    GetPipelineResponse (..),
    mkGetPipelineResponse,

    -- ** Response lenses
    gprsPipeline,
    gprsMetadata,
    gprsResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @GetPipeline@ action.
--
-- /See:/ 'mkGetPipeline' smart constructor.
data GetPipeline = GetPipeline'
  { version :: Lude.Maybe Lude.Natural,
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

-- | Creates a value of 'GetPipeline' with the minimum fields required to make a request.
--
-- * 'name' - The name of the pipeline for which you want to get information. Pipeline names must be unique under an AWS user account.
-- * 'version' - The version number of the pipeline. If you do not specify a version, defaults to the current version.
mkGetPipeline ::
  -- | 'name'
  Lude.Text ->
  GetPipeline
mkGetPipeline pName_ =
  GetPipeline' {version = Lude.Nothing, name = pName_}

-- | The version number of the pipeline. If you do not specify a version, defaults to the current version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpVersion :: Lens.Lens' GetPipeline (Lude.Maybe Lude.Natural)
gpVersion = Lens.lens (version :: GetPipeline -> Lude.Maybe Lude.Natural) (\s a -> s {version = a} :: GetPipeline)
{-# DEPRECATED gpVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the pipeline for which you want to get information. Pipeline names must be unique under an AWS user account.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpName :: Lens.Lens' GetPipeline Lude.Text
gpName = Lens.lens (name :: GetPipeline -> Lude.Text) (\s a -> s {name = a} :: GetPipeline)
{-# DEPRECATED gpName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetPipeline where
  type Rs GetPipeline = GetPipelineResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPipelineResponse'
            Lude.<$> (x Lude..?> "pipeline")
            Lude.<*> (x Lude..?> "metadata")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPipeline where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.GetPipeline" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetPipeline where
  toJSON GetPipeline' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("version" Lude..=) Lude.<$> version,
            Lude.Just ("name" Lude..= name)
          ]
      )

instance Lude.ToPath GetPipeline where
  toPath = Lude.const "/"

instance Lude.ToQuery GetPipeline where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @GetPipeline@ action.
--
-- /See:/ 'mkGetPipelineResponse' smart constructor.
data GetPipelineResponse = GetPipelineResponse'
  { pipeline ::
      Lude.Maybe PipelineDeclaration,
    metadata :: Lude.Maybe PipelineMetadata,
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

-- | Creates a value of 'GetPipelineResponse' with the minimum fields required to make a request.
--
-- * 'metadata' - Represents the pipeline metadata information returned as part of the output of a @GetPipeline@ action.
-- * 'pipeline' - Represents the structure of actions and stages to be performed in the pipeline.
-- * 'responseStatus' - The response status code.
mkGetPipelineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPipelineResponse
mkGetPipelineResponse pResponseStatus_ =
  GetPipelineResponse'
    { pipeline = Lude.Nothing,
      metadata = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Represents the structure of actions and stages to be performed in the pipeline.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsPipeline :: Lens.Lens' GetPipelineResponse (Lude.Maybe PipelineDeclaration)
gprsPipeline = Lens.lens (pipeline :: GetPipelineResponse -> Lude.Maybe PipelineDeclaration) (\s a -> s {pipeline = a} :: GetPipelineResponse)
{-# DEPRECATED gprsPipeline "Use generic-lens or generic-optics with 'pipeline' instead." #-}

-- | Represents the pipeline metadata information returned as part of the output of a @GetPipeline@ action.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsMetadata :: Lens.Lens' GetPipelineResponse (Lude.Maybe PipelineMetadata)
gprsMetadata = Lens.lens (metadata :: GetPipelineResponse -> Lude.Maybe PipelineMetadata) (\s a -> s {metadata = a} :: GetPipelineResponse)
{-# DEPRECATED gprsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsResponseStatus :: Lens.Lens' GetPipelineResponse Lude.Int
gprsResponseStatus = Lens.lens (responseStatus :: GetPipelineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPipelineResponse)
{-# DEPRECATED gprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
