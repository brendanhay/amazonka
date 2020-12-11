{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dLanguageCode,
    dEndpointARN,
    dText,

    -- * Destructuring the response
    DetectEntitiesResponse (..),
    mkDetectEntitiesResponse,

    -- ** Response lenses
    deersEntities,
    deersResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetectEntities' smart constructor.
data DetectEntities = DetectEntities'
  { languageCode ::
      Lude.Maybe LanguageCode,
    endpointARN :: Lude.Maybe Lude.Text,
    text :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectEntities' with the minimum fields required to make a request.
--
-- * 'endpointARN' - The Amazon Resource Name of an endpoint that is associated with a custom entity recognition model. Provide an endpoint if you want to detect entities by using your own custom model instead of the default model that is used by Amazon Comprehend.
--
-- If you specify an endpoint, Amazon Comprehend uses the language of your custom model, and it ignores any language code that you provide in your request.
-- * 'languageCode' - The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
--
-- If your request includes the endpoint for a custom entity recognition model, Amazon Comprehend uses the language of your custom model, and it ignores any language code that you specify here.
-- * 'text' - A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
mkDetectEntities ::
  -- | 'text'
  Lude.Sensitive Lude.Text ->
  DetectEntities
mkDetectEntities pText_ =
  DetectEntities'
    { languageCode = Lude.Nothing,
      endpointARN = Lude.Nothing,
      text = pText_
    }

-- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
--
-- If your request includes the endpoint for a custom entity recognition model, Amazon Comprehend uses the language of your custom model, and it ignores any language code that you specify here.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLanguageCode :: Lens.Lens' DetectEntities (Lude.Maybe LanguageCode)
dLanguageCode = Lens.lens (languageCode :: DetectEntities -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: DetectEntities)
{-# DEPRECATED dLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The Amazon Resource Name of an endpoint that is associated with a custom entity recognition model. Provide an endpoint if you want to detect entities by using your own custom model instead of the default model that is used by Amazon Comprehend.
--
-- If you specify an endpoint, Amazon Comprehend uses the language of your custom model, and it ignores any language code that you provide in your request.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEndpointARN :: Lens.Lens' DetectEntities (Lude.Maybe Lude.Text)
dEndpointARN = Lens.lens (endpointARN :: DetectEntities -> Lude.Maybe Lude.Text) (\s a -> s {endpointARN = a} :: DetectEntities)
{-# DEPRECATED dEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

-- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dText :: Lens.Lens' DetectEntities (Lude.Sensitive Lude.Text)
dText = Lens.lens (text :: DetectEntities -> Lude.Sensitive Lude.Text) (\s a -> s {text = a} :: DetectEntities)
{-# DEPRECATED dText "Use generic-lens or generic-optics with 'text' instead." #-}

instance Lude.AWSRequest DetectEntities where
  type Rs DetectEntities = DetectEntitiesResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          DetectEntitiesResponse'
            Lude.<$> (x Lude..?> "Entities" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetectEntities where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.DetectEntities" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DetectEntities where
  toJSON DetectEntities' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LanguageCode" Lude..=) Lude.<$> languageCode,
            ("EndpointArn" Lude..=) Lude.<$> endpointARN,
            Lude.Just ("Text" Lude..= text)
          ]
      )

instance Lude.ToPath DetectEntities where
  toPath = Lude.const "/"

instance Lude.ToQuery DetectEntities where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetectEntitiesResponse' smart constructor.
data DetectEntitiesResponse = DetectEntitiesResponse'
  { entities ::
      Lude.Maybe [Entity],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectEntitiesResponse' with the minimum fields required to make a request.
--
-- * 'entities' - A collection of entities identified in the input text. For each entity, the response provides the entity text, entity type, where the entity text begins and ends, and the level of confidence that Amazon Comprehend has in the detection.
--
-- If your request uses a custom entity recognition model, Amazon Comprehend detects the entities that the model is trained to recognize. Otherwise, it detects the default entity types. For a list of default entity types, see 'how-entities' .
-- * 'responseStatus' - The response status code.
mkDetectEntitiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetectEntitiesResponse
mkDetectEntitiesResponse pResponseStatus_ =
  DetectEntitiesResponse'
    { entities = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A collection of entities identified in the input text. For each entity, the response provides the entity text, entity type, where the entity text begins and ends, and the level of confidence that Amazon Comprehend has in the detection.
--
-- If your request uses a custom entity recognition model, Amazon Comprehend detects the entities that the model is trained to recognize. Otherwise, it detects the default entity types. For a list of default entity types, see 'how-entities' .
--
-- /Note:/ Consider using 'entities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deersEntities :: Lens.Lens' DetectEntitiesResponse (Lude.Maybe [Entity])
deersEntities = Lens.lens (entities :: DetectEntitiesResponse -> Lude.Maybe [Entity]) (\s a -> s {entities = a} :: DetectEntitiesResponse)
{-# DEPRECATED deersEntities "Use generic-lens or generic-optics with 'entities' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deersResponseStatus :: Lens.Lens' DetectEntitiesResponse Lude.Int
deersResponseStatus = Lens.lens (responseStatus :: DetectEntitiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetectEntitiesResponse)
{-# DEPRECATED deersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
