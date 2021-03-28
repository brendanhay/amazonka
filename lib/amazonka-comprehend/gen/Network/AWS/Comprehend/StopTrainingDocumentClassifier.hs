{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StopTrainingDocumentClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a document classifier training job while in progress.
--
-- If the training job state is @TRAINING@ , the job is marked for termination and put into the @STOP_REQUESTED@ state. If the training job completes before it can be stopped, it is put into the @TRAINED@ ; otherwise the training job is stopped and put into the @STOPPED@ state and the service sends back an HTTP 200 response with an empty HTTP body. 
module Network.AWS.Comprehend.StopTrainingDocumentClassifier
    (
    -- * Creating a request
      StopTrainingDocumentClassifier (..)
    , mkStopTrainingDocumentClassifier
    -- ** Request lenses
    , stdcDocumentClassifierArn

    -- * Destructuring the response
    , StopTrainingDocumentClassifierResponse (..)
    , mkStopTrainingDocumentClassifierResponse
    -- ** Response lenses
    , stdcrrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopTrainingDocumentClassifier' smart constructor.
newtype StopTrainingDocumentClassifier = StopTrainingDocumentClassifier'
  { documentClassifierArn :: Types.DocumentClassifierArn
    -- ^ The Amazon Resource Name (ARN) that identifies the document classifier currently being trained.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopTrainingDocumentClassifier' value with any optional fields omitted.
mkStopTrainingDocumentClassifier
    :: Types.DocumentClassifierArn -- ^ 'documentClassifierArn'
    -> StopTrainingDocumentClassifier
mkStopTrainingDocumentClassifier documentClassifierArn
  = StopTrainingDocumentClassifier'{documentClassifierArn}

-- | The Amazon Resource Name (ARN) that identifies the document classifier currently being trained.
--
-- /Note:/ Consider using 'documentClassifierArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdcDocumentClassifierArn :: Lens.Lens' StopTrainingDocumentClassifier Types.DocumentClassifierArn
stdcDocumentClassifierArn = Lens.field @"documentClassifierArn"
{-# INLINEABLE stdcDocumentClassifierArn #-}
{-# DEPRECATED documentClassifierArn "Use generic-lens or generic-optics with 'documentClassifierArn' instead"  #-}

instance Core.ToQuery StopTrainingDocumentClassifier where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopTrainingDocumentClassifier where
        toHeaders StopTrainingDocumentClassifier{..}
          = Core.pure
              ("X-Amz-Target",
               "Comprehend_20171127.StopTrainingDocumentClassifier")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopTrainingDocumentClassifier where
        toJSON StopTrainingDocumentClassifier{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("DocumentClassifierArn" Core..= documentClassifierArn)])

instance Core.AWSRequest StopTrainingDocumentClassifier where
        type Rs StopTrainingDocumentClassifier =
             StopTrainingDocumentClassifierResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StopTrainingDocumentClassifierResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopTrainingDocumentClassifierResponse' smart constructor.
newtype StopTrainingDocumentClassifierResponse = StopTrainingDocumentClassifierResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopTrainingDocumentClassifierResponse' value with any optional fields omitted.
mkStopTrainingDocumentClassifierResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopTrainingDocumentClassifierResponse
mkStopTrainingDocumentClassifierResponse responseStatus
  = StopTrainingDocumentClassifierResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdcrrsResponseStatus :: Lens.Lens' StopTrainingDocumentClassifierResponse Core.Int
stdcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE stdcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
