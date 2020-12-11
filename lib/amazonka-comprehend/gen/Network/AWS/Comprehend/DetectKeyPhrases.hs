{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DetectKeyPhrases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects the key noun phrases found in the text.
module Network.AWS.Comprehend.DetectKeyPhrases
  ( -- * Creating a request
    DetectKeyPhrases (..),
    mkDetectKeyPhrases,

    -- ** Request lenses
    dkpText,
    dkpLanguageCode,

    -- * Destructuring the response
    DetectKeyPhrasesResponse (..),
    mkDetectKeyPhrasesResponse,

    -- ** Response lenses
    dkprsKeyPhrases,
    dkprsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetectKeyPhrases' smart constructor.
data DetectKeyPhrases = DetectKeyPhrases'
  { text ::
      Lude.Sensitive Lude.Text,
    languageCode :: LanguageCode
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectKeyPhrases' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
-- * 'text' - A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
mkDetectKeyPhrases ::
  -- | 'text'
  Lude.Sensitive Lude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  DetectKeyPhrases
mkDetectKeyPhrases pText_ pLanguageCode_ =
  DetectKeyPhrases' {text = pText_, languageCode = pLanguageCode_}

-- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpText :: Lens.Lens' DetectKeyPhrases (Lude.Sensitive Lude.Text)
dkpText = Lens.lens (text :: DetectKeyPhrases -> Lude.Sensitive Lude.Text) (\s a -> s {text = a} :: DetectKeyPhrases)
{-# DEPRECATED dkpText "Use generic-lens or generic-optics with 'text' instead." #-}

-- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpLanguageCode :: Lens.Lens' DetectKeyPhrases LanguageCode
dkpLanguageCode = Lens.lens (languageCode :: DetectKeyPhrases -> LanguageCode) (\s a -> s {languageCode = a} :: DetectKeyPhrases)
{-# DEPRECATED dkpLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

instance Lude.AWSRequest DetectKeyPhrases where
  type Rs DetectKeyPhrases = DetectKeyPhrasesResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          DetectKeyPhrasesResponse'
            Lude.<$> (x Lude..?> "KeyPhrases" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetectKeyPhrases where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.DetectKeyPhrases" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DetectKeyPhrases where
  toJSON DetectKeyPhrases' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Text" Lude..= text),
            Lude.Just ("LanguageCode" Lude..= languageCode)
          ]
      )

instance Lude.ToPath DetectKeyPhrases where
  toPath = Lude.const "/"

instance Lude.ToQuery DetectKeyPhrases where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetectKeyPhrasesResponse' smart constructor.
data DetectKeyPhrasesResponse = DetectKeyPhrasesResponse'
  { keyPhrases ::
      Lude.Maybe [KeyPhrase],
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

-- | Creates a value of 'DetectKeyPhrasesResponse' with the minimum fields required to make a request.
--
-- * 'keyPhrases' - A collection of key phrases that Amazon Comprehend identified in the input text. For each key phrase, the response provides the text of the key phrase, where the key phrase begins and ends, and the level of confidence that Amazon Comprehend has in the accuracy of the detection.
-- * 'responseStatus' - The response status code.
mkDetectKeyPhrasesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetectKeyPhrasesResponse
mkDetectKeyPhrasesResponse pResponseStatus_ =
  DetectKeyPhrasesResponse'
    { keyPhrases = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A collection of key phrases that Amazon Comprehend identified in the input text. For each key phrase, the response provides the text of the key phrase, where the key phrase begins and ends, and the level of confidence that Amazon Comprehend has in the accuracy of the detection.
--
-- /Note:/ Consider using 'keyPhrases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkprsKeyPhrases :: Lens.Lens' DetectKeyPhrasesResponse (Lude.Maybe [KeyPhrase])
dkprsKeyPhrases = Lens.lens (keyPhrases :: DetectKeyPhrasesResponse -> Lude.Maybe [KeyPhrase]) (\s a -> s {keyPhrases = a} :: DetectKeyPhrasesResponse)
{-# DEPRECATED dkprsKeyPhrases "Use generic-lens or generic-optics with 'keyPhrases' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkprsResponseStatus :: Lens.Lens' DetectKeyPhrasesResponse Lude.Int
dkprsResponseStatus = Lens.lens (responseStatus :: DetectKeyPhrasesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetectKeyPhrasesResponse)
{-# DEPRECATED dkprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
