{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeDocumentClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a document classifier.
module Network.AWS.Comprehend.DescribeDocumentClassifier
    (
    -- * Creating a request
      DescribeDocumentClassifier (..)
    , mkDescribeDocumentClassifier
    -- ** Request lenses
    , ddcDocumentClassifierArn

    -- * Destructuring the response
    , DescribeDocumentClassifierResponse (..)
    , mkDescribeDocumentClassifierResponse
    -- ** Response lenses
    , ddcrrsDocumentClassifierProperties
    , ddcrrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDocumentClassifier' smart constructor.
newtype DescribeDocumentClassifier = DescribeDocumentClassifier'
  { documentClassifierArn :: Types.DocumentClassifierArn
    -- ^ The Amazon Resource Name (ARN) that identifies the document classifier. The operation returns this identifier in its response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDocumentClassifier' value with any optional fields omitted.
mkDescribeDocumentClassifier
    :: Types.DocumentClassifierArn -- ^ 'documentClassifierArn'
    -> DescribeDocumentClassifier
mkDescribeDocumentClassifier documentClassifierArn
  = DescribeDocumentClassifier'{documentClassifierArn}

-- | The Amazon Resource Name (ARN) that identifies the document classifier. The operation returns this identifier in its response.
--
-- /Note:/ Consider using 'documentClassifierArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcDocumentClassifierArn :: Lens.Lens' DescribeDocumentClassifier Types.DocumentClassifierArn
ddcDocumentClassifierArn = Lens.field @"documentClassifierArn"
{-# INLINEABLE ddcDocumentClassifierArn #-}
{-# DEPRECATED documentClassifierArn "Use generic-lens or generic-optics with 'documentClassifierArn' instead"  #-}

instance Core.ToQuery DescribeDocumentClassifier where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeDocumentClassifier where
        toHeaders DescribeDocumentClassifier{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.DescribeDocumentClassifier")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeDocumentClassifier where
        toJSON DescribeDocumentClassifier{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("DocumentClassifierArn" Core..= documentClassifierArn)])

instance Core.AWSRequest DescribeDocumentClassifier where
        type Rs DescribeDocumentClassifier =
             DescribeDocumentClassifierResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeDocumentClassifierResponse' Core.<$>
                   (x Core..:? "DocumentClassifierProperties") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeDocumentClassifierResponse' smart constructor.
data DescribeDocumentClassifierResponse = DescribeDocumentClassifierResponse'
  { documentClassifierProperties :: Core.Maybe Types.DocumentClassifierProperties
    -- ^ An object that contains the properties associated with a document classifier.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeDocumentClassifierResponse' value with any optional fields omitted.
mkDescribeDocumentClassifierResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDocumentClassifierResponse
mkDescribeDocumentClassifierResponse responseStatus
  = DescribeDocumentClassifierResponse'{documentClassifierProperties
                                          = Core.Nothing,
                                        responseStatus}

-- | An object that contains the properties associated with a document classifier.
--
-- /Note:/ Consider using 'documentClassifierProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrrsDocumentClassifierProperties :: Lens.Lens' DescribeDocumentClassifierResponse (Core.Maybe Types.DocumentClassifierProperties)
ddcrrsDocumentClassifierProperties = Lens.field @"documentClassifierProperties"
{-# INLINEABLE ddcrrsDocumentClassifierProperties #-}
{-# DEPRECATED documentClassifierProperties "Use generic-lens or generic-optics with 'documentClassifierProperties' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrrsResponseStatus :: Lens.Lens' DescribeDocumentClassifierResponse Core.Int
ddcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
