{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DetectEntities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects text for named entities, and returns information about them. For more information, about named entities, see 'how-entities' .
module Network.AWS.Comprehend.DetectEntities
  ( -- * Creating a request
    DetectEntities (..),
    mkDetectEntities,

    -- ** Request lenses
    dText,
    dEndpointArn,
    dLanguageCode,

    -- * Destructuring the response
    DetectEntitiesResponse (..),
    mkDetectEntitiesResponse,

    -- ** Response lenses
    dergrsEntities,
    dergrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetectEntities' smart constructor.
data DetectEntities = DetectEntities'
  { -- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
    text :: Types.CustomerInputString,
    -- | The Amazon Resource Name of an endpoint that is associated with a custom entity recognition model. Provide an endpoint if you want to detect entities by using your own custom model instead of the default model that is used by Amazon Comprehend.
    --
    -- If you specify an endpoint, Amazon Comprehend uses the language of your custom model, and it ignores any language code that you provide in your request.
    endpointArn :: Core.Maybe Types.EndpointArn,
    -- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
    --
    -- If your request includes the endpoint for a custom entity recognition model, Amazon Comprehend uses the language of your custom model, and it ignores any language code that you specify here.
    languageCode :: Core.Maybe Types.LanguageCode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectEntities' value with any optional fields omitted.
mkDetectEntities ::
  -- | 'text'
  Types.CustomerInputString ->
  DetectEntities
mkDetectEntities text =
  DetectEntities'
    { text,
      endpointArn = Core.Nothing,
      languageCode = Core.Nothing
    }

-- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dText :: Lens.Lens' DetectEntities Types.CustomerInputString
dText = Lens.field @"text"
{-# DEPRECATED dText "Use generic-lens or generic-optics with 'text' instead." #-}

-- | The Amazon Resource Name of an endpoint that is associated with a custom entity recognition model. Provide an endpoint if you want to detect entities by using your own custom model instead of the default model that is used by Amazon Comprehend.
--
-- If you specify an endpoint, Amazon Comprehend uses the language of your custom model, and it ignores any language code that you provide in your request.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEndpointArn :: Lens.Lens' DetectEntities (Core.Maybe Types.EndpointArn)
dEndpointArn = Lens.field @"endpointArn"
{-# DEPRECATED dEndpointArn "Use generic-lens or generic-optics with 'endpointArn' instead." #-}

-- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
--
-- If your request includes the endpoint for a custom entity recognition model, Amazon Comprehend uses the language of your custom model, and it ignores any language code that you specify here.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLanguageCode :: Lens.Lens' DetectEntities (Core.Maybe Types.LanguageCode)
dLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED dLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

instance Core.FromJSON DetectEntities where
  toJSON DetectEntities {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Text" Core..= text),
            ("EndpointArn" Core..=) Core.<$> endpointArn,
            ("LanguageCode" Core..=) Core.<$> languageCode
          ]
      )

instance Core.AWSRequest DetectEntities where
  type Rs DetectEntities = DetectEntitiesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Comprehend_20171127.DetectEntities")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectEntitiesResponse'
            Core.<$> (x Core..:? "Entities") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDetectEntitiesResponse' smart constructor.
data DetectEntitiesResponse = DetectEntitiesResponse'
  { -- | A collection of entities identified in the input text. For each entity, the response provides the entity text, entity type, where the entity text begins and ends, and the level of confidence that Amazon Comprehend has in the detection.
    --
    -- If your request uses a custom entity recognition model, Amazon Comprehend detects the entities that the model is trained to recognize. Otherwise, it detects the default entity types. For a list of default entity types, see 'how-entities' .
    entities :: Core.Maybe [Types.Entity],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectEntitiesResponse' value with any optional fields omitted.
mkDetectEntitiesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DetectEntitiesResponse
mkDetectEntitiesResponse responseStatus =
  DetectEntitiesResponse' {entities = Core.Nothing, responseStatus}

-- | A collection of entities identified in the input text. For each entity, the response provides the entity text, entity type, where the entity text begins and ends, and the level of confidence that Amazon Comprehend has in the detection.
--
-- If your request uses a custom entity recognition model, Amazon Comprehend detects the entities that the model is trained to recognize. Otherwise, it detects the default entity types. For a list of default entity types, see 'how-entities' .
--
-- /Note:/ Consider using 'entities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dergrsEntities :: Lens.Lens' DetectEntitiesResponse (Core.Maybe [Types.Entity])
dergrsEntities = Lens.field @"entities"
{-# DEPRECATED dergrsEntities "Use generic-lens or generic-optics with 'entities' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dergrsResponseStatus :: Lens.Lens' DetectEntitiesResponse Core.Int
dergrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dergrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
