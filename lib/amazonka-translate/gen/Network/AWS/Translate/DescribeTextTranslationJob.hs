{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.DescribeTextTranslationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with an asycnhronous batch translation job including name, ID, status, source and target languages, input/output S3 buckets, and so on.
module Network.AWS.Translate.DescribeTextTranslationJob
    (
    -- * Creating a request
      DescribeTextTranslationJob (..)
    , mkDescribeTextTranslationJob
    -- ** Request lenses
    , dttjJobId

    -- * Destructuring the response
    , DescribeTextTranslationJobResponse (..)
    , mkDescribeTextTranslationJobResponse
    -- ** Response lenses
    , dttjrrsTextTranslationJobProperties
    , dttjrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Translate.Types as Types

-- | /See:/ 'mkDescribeTextTranslationJob' smart constructor.
newtype DescribeTextTranslationJob = DescribeTextTranslationJob'
  { jobId :: Types.JobId
    -- ^ The identifier that Amazon Translate generated for the job. The 'StartTextTranslationJob' operation returns this identifier in its response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTextTranslationJob' value with any optional fields omitted.
mkDescribeTextTranslationJob
    :: Types.JobId -- ^ 'jobId'
    -> DescribeTextTranslationJob
mkDescribeTextTranslationJob jobId
  = DescribeTextTranslationJob'{jobId}

-- | The identifier that Amazon Translate generated for the job. The 'StartTextTranslationJob' operation returns this identifier in its response.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttjJobId :: Lens.Lens' DescribeTextTranslationJob Types.JobId
dttjJobId = Lens.field @"jobId"
{-# INLINEABLE dttjJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.ToQuery DescribeTextTranslationJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeTextTranslationJob where
        toHeaders DescribeTextTranslationJob{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSShineFrontendService_20170701.DescribeTextTranslationJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeTextTranslationJob where
        toJSON DescribeTextTranslationJob{..}
          = Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest DescribeTextTranslationJob where
        type Rs DescribeTextTranslationJob =
             DescribeTextTranslationJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeTextTranslationJobResponse' Core.<$>
                   (x Core..:? "TextTranslationJobProperties") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeTextTranslationJobResponse' smart constructor.
data DescribeTextTranslationJobResponse = DescribeTextTranslationJobResponse'
  { textTranslationJobProperties :: Core.Maybe Types.TextTranslationJobProperties
    -- ^ An object that contains the properties associated with an asynchronous batch translation job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeTextTranslationJobResponse' value with any optional fields omitted.
mkDescribeTextTranslationJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTextTranslationJobResponse
mkDescribeTextTranslationJobResponse responseStatus
  = DescribeTextTranslationJobResponse'{textTranslationJobProperties
                                          = Core.Nothing,
                                        responseStatus}

-- | An object that contains the properties associated with an asynchronous batch translation job.
--
-- /Note:/ Consider using 'textTranslationJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttjrrsTextTranslationJobProperties :: Lens.Lens' DescribeTextTranslationJobResponse (Core.Maybe Types.TextTranslationJobProperties)
dttjrrsTextTranslationJobProperties = Lens.field @"textTranslationJobProperties"
{-# INLINEABLE dttjrrsTextTranslationJobProperties #-}
{-# DEPRECATED textTranslationJobProperties "Use generic-lens or generic-optics with 'textTranslationJobProperties' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttjrrsResponseStatus :: Lens.Lens' DescribeTextTranslationJobResponse Core.Int
dttjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dttjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
