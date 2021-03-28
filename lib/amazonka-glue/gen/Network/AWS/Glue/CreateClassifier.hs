{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a classifier in the user's account. This can be a @GrokClassifier@ , an @XMLClassifier@ , a @JsonClassifier@ , or a @CsvClassifier@ , depending on which field of the request is present.
module Network.AWS.Glue.CreateClassifier
    (
    -- * Creating a request
      CreateClassifier (..)
    , mkCreateClassifier
    -- ** Request lenses
    , ccCsvClassifier
    , ccGrokClassifier
    , ccJsonClassifier
    , ccXMLClassifier

    -- * Destructuring the response
    , CreateClassifierResponse (..)
    , mkCreateClassifierResponse
    -- ** Response lenses
    , ccrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateClassifier' smart constructor.
data CreateClassifier = CreateClassifier'
  { csvClassifier :: Core.Maybe Types.CreateCsvClassifierRequest
    -- ^ A @CsvClassifier@ object specifying the classifier to create.
  , grokClassifier :: Core.Maybe Types.CreateGrokClassifierRequest
    -- ^ A @GrokClassifier@ object specifying the classifier to create.
  , jsonClassifier :: Core.Maybe Types.CreateJsonClassifierRequest
    -- ^ A @JsonClassifier@ object specifying the classifier to create.
  , xMLClassifier :: Core.Maybe Types.CreateXMLClassifierRequest
    -- ^ An @XMLClassifier@ object specifying the classifier to create.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateClassifier' value with any optional fields omitted.
mkCreateClassifier
    :: CreateClassifier
mkCreateClassifier
  = CreateClassifier'{csvClassifier = Core.Nothing,
                      grokClassifier = Core.Nothing, jsonClassifier = Core.Nothing,
                      xMLClassifier = Core.Nothing}

-- | A @CsvClassifier@ object specifying the classifier to create.
--
-- /Note:/ Consider using 'csvClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCsvClassifier :: Lens.Lens' CreateClassifier (Core.Maybe Types.CreateCsvClassifierRequest)
ccCsvClassifier = Lens.field @"csvClassifier"
{-# INLINEABLE ccCsvClassifier #-}
{-# DEPRECATED csvClassifier "Use generic-lens or generic-optics with 'csvClassifier' instead"  #-}

-- | A @GrokClassifier@ object specifying the classifier to create.
--
-- /Note:/ Consider using 'grokClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccGrokClassifier :: Lens.Lens' CreateClassifier (Core.Maybe Types.CreateGrokClassifierRequest)
ccGrokClassifier = Lens.field @"grokClassifier"
{-# INLINEABLE ccGrokClassifier #-}
{-# DEPRECATED grokClassifier "Use generic-lens or generic-optics with 'grokClassifier' instead"  #-}

-- | A @JsonClassifier@ object specifying the classifier to create.
--
-- /Note:/ Consider using 'jsonClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccJsonClassifier :: Lens.Lens' CreateClassifier (Core.Maybe Types.CreateJsonClassifierRequest)
ccJsonClassifier = Lens.field @"jsonClassifier"
{-# INLINEABLE ccJsonClassifier #-}
{-# DEPRECATED jsonClassifier "Use generic-lens or generic-optics with 'jsonClassifier' instead"  #-}

-- | An @XMLClassifier@ object specifying the classifier to create.
--
-- /Note:/ Consider using 'xMLClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccXMLClassifier :: Lens.Lens' CreateClassifier (Core.Maybe Types.CreateXMLClassifierRequest)
ccXMLClassifier = Lens.field @"xMLClassifier"
{-# INLINEABLE ccXMLClassifier #-}
{-# DEPRECATED xMLClassifier "Use generic-lens or generic-optics with 'xMLClassifier' instead"  #-}

instance Core.ToQuery CreateClassifier where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateClassifier where
        toHeaders CreateClassifier{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.CreateClassifier") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateClassifier where
        toJSON CreateClassifier{..}
          = Core.object
              (Core.catMaybes
                 [("CsvClassifier" Core..=) Core.<$> csvClassifier,
                  ("GrokClassifier" Core..=) Core.<$> grokClassifier,
                  ("JsonClassifier" Core..=) Core.<$> jsonClassifier,
                  ("XMLClassifier" Core..=) Core.<$> xMLClassifier])

instance Core.AWSRequest CreateClassifier where
        type Rs CreateClassifier = CreateClassifierResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CreateClassifierResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateClassifierResponse' smart constructor.
newtype CreateClassifierResponse = CreateClassifierResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateClassifierResponse' value with any optional fields omitted.
mkCreateClassifierResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateClassifierResponse
mkCreateClassifierResponse responseStatus
  = CreateClassifierResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateClassifierResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
