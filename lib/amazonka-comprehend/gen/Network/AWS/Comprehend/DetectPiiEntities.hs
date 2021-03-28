{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DetectPiiEntities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects the input text for entities that contain personally identifiable information (PII) and returns information about them.
module Network.AWS.Comprehend.DetectPiiEntities
    (
    -- * Creating a request
      DetectPiiEntities (..)
    , mkDetectPiiEntities
    -- ** Request lenses
    , dpeText
    , dpeLanguageCode

    -- * Destructuring the response
    , DetectPiiEntitiesResponse (..)
    , mkDetectPiiEntitiesResponse
    -- ** Response lenses
    , dperrsEntities
    , dperrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetectPiiEntities' smart constructor.
data DetectPiiEntities = DetectPiiEntities'
  { text :: Core.Text
    -- ^ A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
  , languageCode :: Types.LanguageCode
    -- ^ The language of the input documents.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectPiiEntities' value with any optional fields omitted.
mkDetectPiiEntities
    :: Core.Text -- ^ 'text'
    -> Types.LanguageCode -- ^ 'languageCode'
    -> DetectPiiEntities
mkDetectPiiEntities text languageCode
  = DetectPiiEntities'{text, languageCode}

-- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpeText :: Lens.Lens' DetectPiiEntities Core.Text
dpeText = Lens.field @"text"
{-# INLINEABLE dpeText #-}
{-# DEPRECATED text "Use generic-lens or generic-optics with 'text' instead"  #-}

-- | The language of the input documents.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpeLanguageCode :: Lens.Lens' DetectPiiEntities Types.LanguageCode
dpeLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE dpeLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

instance Core.ToQuery DetectPiiEntities where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DetectPiiEntities where
        toHeaders DetectPiiEntities{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.DetectPiiEntities")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DetectPiiEntities where
        toJSON DetectPiiEntities{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Text" Core..= text),
                  Core.Just ("LanguageCode" Core..= languageCode)])

instance Core.AWSRequest DetectPiiEntities where
        type Rs DetectPiiEntities = DetectPiiEntitiesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DetectPiiEntitiesResponse' Core.<$>
                   (x Core..:? "Entities") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDetectPiiEntitiesResponse' smart constructor.
data DetectPiiEntitiesResponse = DetectPiiEntitiesResponse'
  { entities :: Core.Maybe [Types.PiiEntity]
    -- ^ A collection of PII entities identified in the input text. For each entity, the response provides the entity type, where the entity text begins and ends, and the level of confidence that Amazon Comprehend has in the detection.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectPiiEntitiesResponse' value with any optional fields omitted.
mkDetectPiiEntitiesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DetectPiiEntitiesResponse
mkDetectPiiEntitiesResponse responseStatus
  = DetectPiiEntitiesResponse'{entities = Core.Nothing,
                               responseStatus}

-- | A collection of PII entities identified in the input text. For each entity, the response provides the entity type, where the entity text begins and ends, and the level of confidence that Amazon Comprehend has in the detection.
--
-- /Note:/ Consider using 'entities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dperrsEntities :: Lens.Lens' DetectPiiEntitiesResponse (Core.Maybe [Types.PiiEntity])
dperrsEntities = Lens.field @"entities"
{-# INLINEABLE dperrsEntities #-}
{-# DEPRECATED entities "Use generic-lens or generic-optics with 'entities' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dperrsResponseStatus :: Lens.Lens' DetectPiiEntitiesResponse Core.Int
dperrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dperrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
