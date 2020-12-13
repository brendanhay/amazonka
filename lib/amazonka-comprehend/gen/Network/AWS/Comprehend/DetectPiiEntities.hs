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
    dpeLanguageCode,
    dpeText,

    -- * Destructuring the response
    DetectPiiEntitiesResponse (..),
    mkDetectPiiEntitiesResponse,

    -- ** Response lenses
    dpersEntities,
    dpersResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetectPiiEntities' smart constructor.
data DetectPiiEntities = DetectPiiEntities'
  { -- | The language of the input documents.
    languageCode :: LanguageCode,
    -- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
    text :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectPiiEntities' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language of the input documents.
-- * 'text' - A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
mkDetectPiiEntities ::
  -- | 'languageCode'
  LanguageCode ->
  -- | 'text'
  Lude.Text ->
  DetectPiiEntities
mkDetectPiiEntities pLanguageCode_ pText_ =
  DetectPiiEntities' {languageCode = pLanguageCode_, text = pText_}

-- | The language of the input documents.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpeLanguageCode :: Lens.Lens' DetectPiiEntities LanguageCode
dpeLanguageCode = Lens.lens (languageCode :: DetectPiiEntities -> LanguageCode) (\s a -> s {languageCode = a} :: DetectPiiEntities)
{-# DEPRECATED dpeLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpeText :: Lens.Lens' DetectPiiEntities Lude.Text
dpeText = Lens.lens (text :: DetectPiiEntities -> Lude.Text) (\s a -> s {text = a} :: DetectPiiEntities)
{-# DEPRECATED dpeText "Use generic-lens or generic-optics with 'text' instead." #-}

instance Lude.AWSRequest DetectPiiEntities where
  type Rs DetectPiiEntities = DetectPiiEntitiesResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          DetectPiiEntitiesResponse'
            Lude.<$> (x Lude..?> "Entities" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetectPiiEntities where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.DetectPiiEntities" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DetectPiiEntities where
  toJSON DetectPiiEntities' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("LanguageCode" Lude..= languageCode),
            Lude.Just ("Text" Lude..= text)
          ]
      )

instance Lude.ToPath DetectPiiEntities where
  toPath = Lude.const "/"

instance Lude.ToQuery DetectPiiEntities where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetectPiiEntitiesResponse' smart constructor.
data DetectPiiEntitiesResponse = DetectPiiEntitiesResponse'
  { -- | A collection of PII entities identified in the input text. For each entity, the response provides the entity type, where the entity text begins and ends, and the level of confidence that Amazon Comprehend has in the detection.
    entities :: Lude.Maybe [PiiEntity],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectPiiEntitiesResponse' with the minimum fields required to make a request.
--
-- * 'entities' - A collection of PII entities identified in the input text. For each entity, the response provides the entity type, where the entity text begins and ends, and the level of confidence that Amazon Comprehend has in the detection.
-- * 'responseStatus' - The response status code.
mkDetectPiiEntitiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetectPiiEntitiesResponse
mkDetectPiiEntitiesResponse pResponseStatus_ =
  DetectPiiEntitiesResponse'
    { entities = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A collection of PII entities identified in the input text. For each entity, the response provides the entity type, where the entity text begins and ends, and the level of confidence that Amazon Comprehend has in the detection.
--
-- /Note:/ Consider using 'entities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpersEntities :: Lens.Lens' DetectPiiEntitiesResponse (Lude.Maybe [PiiEntity])
dpersEntities = Lens.lens (entities :: DetectPiiEntitiesResponse -> Lude.Maybe [PiiEntity]) (\s a -> s {entities = a} :: DetectPiiEntitiesResponse)
{-# DEPRECATED dpersEntities "Use generic-lens or generic-optics with 'entities' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpersResponseStatus :: Lens.Lens' DetectPiiEntitiesResponse Lude.Int
dpersResponseStatus = Lens.lens (responseStatus :: DetectPiiEntitiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetectPiiEntitiesResponse)
{-# DEPRECATED dpersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
