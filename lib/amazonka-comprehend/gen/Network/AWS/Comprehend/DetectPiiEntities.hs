{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DetectPiiEntities (..),
    mkDetectPiiEntities,

    -- ** Request lenses
    dpeText,
    dpeLanguageCode,

    -- * Destructuring the response
    DetectPiiEntitiesResponse (..),
    mkDetectPiiEntitiesResponse,

    -- ** Response lenses
    dperrsEntities,
    dperrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetectPiiEntities' smart constructor.
data DetectPiiEntities = DetectPiiEntities'
  { -- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
    text :: Types.String,
    -- | The language of the input documents.
    languageCode :: Types.LanguageCode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectPiiEntities' value with any optional fields omitted.
mkDetectPiiEntities ::
  -- | 'text'
  Types.String ->
  -- | 'languageCode'
  Types.LanguageCode ->
  DetectPiiEntities
mkDetectPiiEntities text languageCode =
  DetectPiiEntities' {text, languageCode}

-- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpeText :: Lens.Lens' DetectPiiEntities Types.String
dpeText = Lens.field @"text"
{-# DEPRECATED dpeText "Use generic-lens or generic-optics with 'text' instead." #-}

-- | The language of the input documents.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpeLanguageCode :: Lens.Lens' DetectPiiEntities Types.LanguageCode
dpeLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED dpeLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

instance Core.FromJSON DetectPiiEntities where
  toJSON DetectPiiEntities {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Text" Core..= text),
            Core.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.AWSRequest DetectPiiEntities where
  type Rs DetectPiiEntities = DetectPiiEntitiesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Comprehend_20171127.DetectPiiEntities")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectPiiEntitiesResponse'
            Core.<$> (x Core..:? "Entities") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDetectPiiEntitiesResponse' smart constructor.
data DetectPiiEntitiesResponse = DetectPiiEntitiesResponse'
  { -- | A collection of PII entities identified in the input text. For each entity, the response provides the entity type, where the entity text begins and ends, and the level of confidence that Amazon Comprehend has in the detection.
    entities :: Core.Maybe [Types.PiiEntity],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectPiiEntitiesResponse' value with any optional fields omitted.
mkDetectPiiEntitiesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DetectPiiEntitiesResponse
mkDetectPiiEntitiesResponse responseStatus =
  DetectPiiEntitiesResponse'
    { entities = Core.Nothing,
      responseStatus
    }

-- | A collection of PII entities identified in the input text. For each entity, the response provides the entity type, where the entity text begins and ends, and the level of confidence that Amazon Comprehend has in the detection.
--
-- /Note:/ Consider using 'entities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dperrsEntities :: Lens.Lens' DetectPiiEntitiesResponse (Core.Maybe [Types.PiiEntity])
dperrsEntities = Lens.field @"entities"
{-# DEPRECATED dperrsEntities "Use generic-lens or generic-optics with 'entities' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dperrsResponseStatus :: Lens.Lens' DetectPiiEntitiesResponse Core.Int
dperrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dperrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
