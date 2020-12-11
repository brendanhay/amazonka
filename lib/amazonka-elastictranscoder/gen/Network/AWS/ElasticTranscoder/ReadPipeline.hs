{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.ReadPipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ReadPipeline operation gets detailed information about a pipeline.
module Network.AWS.ElasticTranscoder.ReadPipeline
  ( -- * Creating a request
    ReadPipeline (..),
    mkReadPipeline,

    -- ** Request lenses
    rId,

    -- * Destructuring the response
    ReadPipelineResponse (..),
    mkReadPipelineResponse,

    -- ** Response lenses
    rrsWarnings,
    rrsPipeline,
    rrsResponseStatus,
  )
where

import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The @ReadPipelineRequest@ structure.
--
-- /See:/ 'mkReadPipeline' smart constructor.
newtype ReadPipeline = ReadPipeline' {id :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReadPipeline' with the minimum fields required to make a request.
--
-- * 'id' - The identifier of the pipeline to read.
mkReadPipeline ::
  -- | 'id'
  Lude.Text ->
  ReadPipeline
mkReadPipeline pId_ = ReadPipeline' {id = pId_}

-- | The identifier of the pipeline to read.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rId :: Lens.Lens' ReadPipeline Lude.Text
rId = Lens.lens (id :: ReadPipeline -> Lude.Text) (\s a -> s {id = a} :: ReadPipeline)
{-# DEPRECATED rId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest ReadPipeline where
  type Rs ReadPipeline = ReadPipelineResponse
  request = Req.get elasticTranscoderService
  response =
    Res.receiveJSON
      ( \s h x ->
          ReadPipelineResponse'
            Lude.<$> (x Lude..?> "Warnings" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Pipeline")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ReadPipeline where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ReadPipeline where
  toPath ReadPipeline' {..} =
    Lude.mconcat ["/2012-09-25/pipelines/", Lude.toBS id]

instance Lude.ToQuery ReadPipeline where
  toQuery = Lude.const Lude.mempty

-- | The @ReadPipelineResponse@ structure.
--
-- /See:/ 'mkReadPipelineResponse' smart constructor.
data ReadPipelineResponse = ReadPipelineResponse'
  { warnings ::
      Lude.Maybe [Warning],
    pipeline :: Lude.Maybe Pipeline,
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

-- | Creates a value of 'ReadPipelineResponse' with the minimum fields required to make a request.
--
-- * 'pipeline' - A section of the response body that provides information about the pipeline.
-- * 'responseStatus' - The response status code.
-- * 'warnings' - Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.
mkReadPipelineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ReadPipelineResponse
mkReadPipelineResponse pResponseStatus_ =
  ReadPipelineResponse'
    { warnings = Lude.Nothing,
      pipeline = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.
--
-- /Note:/ Consider using 'warnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsWarnings :: Lens.Lens' ReadPipelineResponse (Lude.Maybe [Warning])
rrsWarnings = Lens.lens (warnings :: ReadPipelineResponse -> Lude.Maybe [Warning]) (\s a -> s {warnings = a} :: ReadPipelineResponse)
{-# DEPRECATED rrsWarnings "Use generic-lens or generic-optics with 'warnings' instead." #-}

-- | A section of the response body that provides information about the pipeline.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsPipeline :: Lens.Lens' ReadPipelineResponse (Lude.Maybe Pipeline)
rrsPipeline = Lens.lens (pipeline :: ReadPipelineResponse -> Lude.Maybe Pipeline) (\s a -> s {pipeline = a} :: ReadPipelineResponse)
{-# DEPRECATED rrsPipeline "Use generic-lens or generic-optics with 'pipeline' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' ReadPipelineResponse Lude.Int
rrsResponseStatus = Lens.lens (responseStatus :: ReadPipelineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ReadPipelineResponse)
{-# DEPRECATED rrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
