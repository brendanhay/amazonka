{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ReadPipeline (..)
    , mkReadPipeline
    -- ** Request lenses
    , rId

    -- * Destructuring the response
    , ReadPipelineResponse (..)
    , mkReadPipelineResponse
    -- ** Response lenses
    , rrsPipeline
    , rrsWarnings
    , rrsResponseStatus
    ) where

import qualified Network.AWS.ElasticTranscoder.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @ReadPipelineRequest@ structure.
--
-- /See:/ 'mkReadPipeline' smart constructor.
newtype ReadPipeline = ReadPipeline'
  { id :: Types.Id
    -- ^ The identifier of the pipeline to read.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ReadPipeline' value with any optional fields omitted.
mkReadPipeline
    :: Types.Id -- ^ 'id'
    -> ReadPipeline
mkReadPipeline id = ReadPipeline'{id}

-- | The identifier of the pipeline to read.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rId :: Lens.Lens' ReadPipeline Types.Id
rId = Lens.field @"id"
{-# INLINEABLE rId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery ReadPipeline where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ReadPipeline where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ReadPipeline where
        type Rs ReadPipeline = ReadPipelineResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2012-09-25/pipelines/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ReadPipelineResponse' Core.<$>
                   (x Core..:? "Pipeline") Core.<*> x Core..:? "Warnings" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The @ReadPipelineResponse@ structure.
--
-- /See:/ 'mkReadPipelineResponse' smart constructor.
data ReadPipelineResponse = ReadPipelineResponse'
  { pipeline :: Core.Maybe Types.Pipeline
    -- ^ A section of the response body that provides information about the pipeline.
  , warnings :: Core.Maybe [Types.Warning]
    -- ^ Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReadPipelineResponse' value with any optional fields omitted.
mkReadPipelineResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ReadPipelineResponse
mkReadPipelineResponse responseStatus
  = ReadPipelineResponse'{pipeline = Core.Nothing,
                          warnings = Core.Nothing, responseStatus}

-- | A section of the response body that provides information about the pipeline.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsPipeline :: Lens.Lens' ReadPipelineResponse (Core.Maybe Types.Pipeline)
rrsPipeline = Lens.field @"pipeline"
{-# INLINEABLE rrsPipeline #-}
{-# DEPRECATED pipeline "Use generic-lens or generic-optics with 'pipeline' instead"  #-}

-- | Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.
--
-- /Note:/ Consider using 'warnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsWarnings :: Lens.Lens' ReadPipelineResponse (Core.Maybe [Types.Warning])
rrsWarnings = Lens.field @"warnings"
{-# INLINEABLE rrsWarnings #-}
{-# DEPRECATED warnings "Use generic-lens or generic-optics with 'warnings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' ReadPipelineResponse Core.Int
rrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
