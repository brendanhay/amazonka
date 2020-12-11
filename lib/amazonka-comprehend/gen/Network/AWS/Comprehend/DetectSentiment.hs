{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DetectSentiment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects text and returns an inference of the prevailing sentiment (@POSITIVE@ , @NEUTRAL@ , @MIXED@ , or @NEGATIVE@ ).
module Network.AWS.Comprehend.DetectSentiment
  ( -- * Creating a request
    DetectSentiment (..),
    mkDetectSentiment,

    -- ** Request lenses
    dsText,
    dsLanguageCode,

    -- * Destructuring the response
    DetectSentimentResponse (..),
    mkDetectSentimentResponse,

    -- ** Response lenses
    detrsSentiment,
    detrsSentimentScore,
    detrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetectSentiment' smart constructor.
data DetectSentiment = DetectSentiment'
  { text ::
      Lude.Sensitive Lude.Text,
    languageCode :: LanguageCode
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectSentiment' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
-- * 'text' - A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
mkDetectSentiment ::
  -- | 'text'
  Lude.Sensitive Lude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  DetectSentiment
mkDetectSentiment pText_ pLanguageCode_ =
  DetectSentiment' {text = pText_, languageCode = pLanguageCode_}

-- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsText :: Lens.Lens' DetectSentiment (Lude.Sensitive Lude.Text)
dsText = Lens.lens (text :: DetectSentiment -> Lude.Sensitive Lude.Text) (\s a -> s {text = a} :: DetectSentiment)
{-# DEPRECATED dsText "Use generic-lens or generic-optics with 'text' instead." #-}

-- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLanguageCode :: Lens.Lens' DetectSentiment LanguageCode
dsLanguageCode = Lens.lens (languageCode :: DetectSentiment -> LanguageCode) (\s a -> s {languageCode = a} :: DetectSentiment)
{-# DEPRECATED dsLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

instance Lude.AWSRequest DetectSentiment where
  type Rs DetectSentiment = DetectSentimentResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          DetectSentimentResponse'
            Lude.<$> (x Lude..?> "Sentiment")
            Lude.<*> (x Lude..?> "SentimentScore")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetectSentiment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.DetectSentiment" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DetectSentiment where
  toJSON DetectSentiment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Text" Lude..= text),
            Lude.Just ("LanguageCode" Lude..= languageCode)
          ]
      )

instance Lude.ToPath DetectSentiment where
  toPath = Lude.const "/"

instance Lude.ToQuery DetectSentiment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetectSentimentResponse' smart constructor.
data DetectSentimentResponse = DetectSentimentResponse'
  { sentiment ::
      Lude.Maybe SentimentType,
    sentimentScore :: Lude.Maybe SentimentScore,
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

-- | Creates a value of 'DetectSentimentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'sentiment' - The inferred sentiment that Amazon Comprehend has the highest level of confidence in.
-- * 'sentimentScore' - An object that lists the sentiments, and their corresponding confidence levels.
mkDetectSentimentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetectSentimentResponse
mkDetectSentimentResponse pResponseStatus_ =
  DetectSentimentResponse'
    { sentiment = Lude.Nothing,
      sentimentScore = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The inferred sentiment that Amazon Comprehend has the highest level of confidence in.
--
-- /Note:/ Consider using 'sentiment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsSentiment :: Lens.Lens' DetectSentimentResponse (Lude.Maybe SentimentType)
detrsSentiment = Lens.lens (sentiment :: DetectSentimentResponse -> Lude.Maybe SentimentType) (\s a -> s {sentiment = a} :: DetectSentimentResponse)
{-# DEPRECATED detrsSentiment "Use generic-lens or generic-optics with 'sentiment' instead." #-}

-- | An object that lists the sentiments, and their corresponding confidence levels.
--
-- /Note:/ Consider using 'sentimentScore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsSentimentScore :: Lens.Lens' DetectSentimentResponse (Lude.Maybe SentimentScore)
detrsSentimentScore = Lens.lens (sentimentScore :: DetectSentimentResponse -> Lude.Maybe SentimentScore) (\s a -> s {sentimentScore = a} :: DetectSentimentResponse)
{-# DEPRECATED detrsSentimentScore "Use generic-lens or generic-optics with 'sentimentScore' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsResponseStatus :: Lens.Lens' DetectSentimentResponse Lude.Int
detrsResponseStatus = Lens.lens (responseStatus :: DetectSentimentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetectSentimentResponse)
{-# DEPRECATED detrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
