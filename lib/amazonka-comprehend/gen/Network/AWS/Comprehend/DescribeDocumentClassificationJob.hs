{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeDocumentClassificationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a document classification job. Use this operation to get the status of a classification job.
module Network.AWS.Comprehend.DescribeDocumentClassificationJob
    (
    -- * Creating a request
      DescribeDocumentClassificationJob (..)
    , mkDescribeDocumentClassificationJob
    -- ** Request lenses
    , ddcjJobId

    -- * Destructuring the response
    , DescribeDocumentClassificationJobResponse (..)
    , mkDescribeDocumentClassificationJobResponse
    -- ** Response lenses
    , ddcjrrsDocumentClassificationJobProperties
    , ddcjrrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDocumentClassificationJob' smart constructor.
newtype DescribeDocumentClassificationJob = DescribeDocumentClassificationJob'
  { jobId :: Types.JobId
    -- ^ The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDocumentClassificationJob' value with any optional fields omitted.
mkDescribeDocumentClassificationJob
    :: Types.JobId -- ^ 'jobId'
    -> DescribeDocumentClassificationJob
mkDescribeDocumentClassificationJob jobId
  = DescribeDocumentClassificationJob'{jobId}

-- | The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcjJobId :: Lens.Lens' DescribeDocumentClassificationJob Types.JobId
ddcjJobId = Lens.field @"jobId"
{-# INLINEABLE ddcjJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.ToQuery DescribeDocumentClassificationJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeDocumentClassificationJob where
        toHeaders DescribeDocumentClassificationJob{..}
          = Core.pure
              ("X-Amz-Target",
               "Comprehend_20171127.DescribeDocumentClassificationJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeDocumentClassificationJob where
        toJSON DescribeDocumentClassificationJob{..}
          = Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest DescribeDocumentClassificationJob where
        type Rs DescribeDocumentClassificationJob =
             DescribeDocumentClassificationJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeDocumentClassificationJobResponse' Core.<$>
                   (x Core..:? "DocumentClassificationJobProperties") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeDocumentClassificationJobResponse' smart constructor.
data DescribeDocumentClassificationJobResponse = DescribeDocumentClassificationJobResponse'
  { documentClassificationJobProperties :: Core.Maybe Types.DocumentClassificationJobProperties
    -- ^ An object that describes the properties associated with the document classification job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeDocumentClassificationJobResponse' value with any optional fields omitted.
mkDescribeDocumentClassificationJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDocumentClassificationJobResponse
mkDescribeDocumentClassificationJobResponse responseStatus
  = DescribeDocumentClassificationJobResponse'{documentClassificationJobProperties
                                                 = Core.Nothing,
                                               responseStatus}

-- | An object that describes the properties associated with the document classification job.
--
-- /Note:/ Consider using 'documentClassificationJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcjrrsDocumentClassificationJobProperties :: Lens.Lens' DescribeDocumentClassificationJobResponse (Core.Maybe Types.DocumentClassificationJobProperties)
ddcjrrsDocumentClassificationJobProperties = Lens.field @"documentClassificationJobProperties"
{-# INLINEABLE ddcjrrsDocumentClassificationJobProperties #-}
{-# DEPRECATED documentClassificationJobProperties "Use generic-lens or generic-optics with 'documentClassificationJobProperties' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcjrrsResponseStatus :: Lens.Lens' DescribeDocumentClassificationJobResponse Core.Int
ddcjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddcjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
