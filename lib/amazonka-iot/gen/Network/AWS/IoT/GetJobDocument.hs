{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetJobDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a job document.
module Network.AWS.IoT.GetJobDocument
    (
    -- * Creating a request
      GetJobDocument (..)
    , mkGetJobDocument
    -- ** Request lenses
    , gjdJobId

    -- * Destructuring the response
    , GetJobDocumentResponse (..)
    , mkGetJobDocumentResponse
    -- ** Response lenses
    , gjdrrsDocument
    , gjdrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetJobDocument' smart constructor.
newtype GetJobDocument = GetJobDocument'
  { jobId :: Types.JobId
    -- ^ The unique identifier you assigned to this job when it was created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetJobDocument' value with any optional fields omitted.
mkGetJobDocument
    :: Types.JobId -- ^ 'jobId'
    -> GetJobDocument
mkGetJobDocument jobId = GetJobDocument'{jobId}

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdJobId :: Lens.Lens' GetJobDocument Types.JobId
gjdJobId = Lens.field @"jobId"
{-# INLINEABLE gjdJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.ToQuery GetJobDocument where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetJobDocument where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetJobDocument where
        type Rs GetJobDocument = GetJobDocumentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/jobs/" Core.<> Core.toText jobId Core.<> "/job-document",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetJobDocumentResponse' Core.<$>
                   (x Core..:? "document") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetJobDocumentResponse' smart constructor.
data GetJobDocumentResponse = GetJobDocumentResponse'
  { document :: Core.Maybe Types.JobDocument
    -- ^ The job document content.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetJobDocumentResponse' value with any optional fields omitted.
mkGetJobDocumentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetJobDocumentResponse
mkGetJobDocumentResponse responseStatus
  = GetJobDocumentResponse'{document = Core.Nothing, responseStatus}

-- | The job document content.
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrrsDocument :: Lens.Lens' GetJobDocumentResponse (Core.Maybe Types.JobDocument)
gjdrrsDocument = Lens.field @"document"
{-# INLINEABLE gjdrrsDocument #-}
{-# DEPRECATED document "Use generic-lens or generic-optics with 'document' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrrsResponseStatus :: Lens.Lens' GetJobDocumentResponse Core.Int
gjdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gjdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
