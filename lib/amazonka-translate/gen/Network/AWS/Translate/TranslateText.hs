{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.TranslateText
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Translates input text from the source language to the target language. For a list of available languages and language codes, see 'what-is-languages' .
module Network.AWS.Translate.TranslateText
  ( -- * Creating a request
    TranslateText (..),
    mkTranslateText,

    -- ** Request lenses
    ttTerminologyNames,
    ttText,
    ttSourceLanguageCode,
    ttTargetLanguageCode,

    -- * Destructuring the response
    TranslateTextResponse (..),
    mkTranslateTextResponse,

    -- ** Response lenses
    ttrsAppliedTerminologies,
    ttrsResponseStatus,
    ttrsTranslatedText,
    ttrsSourceLanguageCode,
    ttrsTargetLanguageCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Translate.Types

-- | /See:/ 'mkTranslateText' smart constructor.
data TranslateText = TranslateText'
  { terminologyNames ::
      Lude.Maybe [Lude.Text],
    text :: Lude.Text,
    sourceLanguageCode :: Lude.Text,
    targetLanguageCode :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TranslateText' with the minimum fields required to make a request.
--
-- * 'sourceLanguageCode' - The language code for the language of the source text. The language must be a language supported by Amazon Translate. For a list of language codes, see 'what-is-languages' .
--
-- To have Amazon Translate determine the source language of your text, you can specify @auto@ in the @SourceLanguageCode@ field. If you specify @auto@ , Amazon Translate will call <https://docs.aws.amazon.com/comprehend/latest/dg/comprehend-general.html Amazon Comprehend> to determine the source language.
-- * 'targetLanguageCode' - The language code requested for the language of the target text. The language must be a language supported by Amazon Translate.
-- * 'terminologyNames' - The name of the terminology list file to be used in the TranslateText request. You can use 1 terminology list at most in a @TranslateText@ request. Terminology lists can contain a maximum of 256 terms.
-- * 'text' - The text to translate. The text string can be a maximum of 5,000 bytes long. Depending on your character set, this may be fewer than 5,000 characters.
mkTranslateText ::
  -- | 'text'
  Lude.Text ->
  -- | 'sourceLanguageCode'
  Lude.Text ->
  -- | 'targetLanguageCode'
  Lude.Text ->
  TranslateText
mkTranslateText pText_ pSourceLanguageCode_ pTargetLanguageCode_ =
  TranslateText'
    { terminologyNames = Lude.Nothing,
      text = pText_,
      sourceLanguageCode = pSourceLanguageCode_,
      targetLanguageCode = pTargetLanguageCode_
    }

-- | The name of the terminology list file to be used in the TranslateText request. You can use 1 terminology list at most in a @TranslateText@ request. Terminology lists can contain a maximum of 256 terms.
--
-- /Note:/ Consider using 'terminologyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttTerminologyNames :: Lens.Lens' TranslateText (Lude.Maybe [Lude.Text])
ttTerminologyNames = Lens.lens (terminologyNames :: TranslateText -> Lude.Maybe [Lude.Text]) (\s a -> s {terminologyNames = a} :: TranslateText)
{-# DEPRECATED ttTerminologyNames "Use generic-lens or generic-optics with 'terminologyNames' instead." #-}

-- | The text to translate. The text string can be a maximum of 5,000 bytes long. Depending on your character set, this may be fewer than 5,000 characters.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttText :: Lens.Lens' TranslateText Lude.Text
ttText = Lens.lens (text :: TranslateText -> Lude.Text) (\s a -> s {text = a} :: TranslateText)
{-# DEPRECATED ttText "Use generic-lens or generic-optics with 'text' instead." #-}

-- | The language code for the language of the source text. The language must be a language supported by Amazon Translate. For a list of language codes, see 'what-is-languages' .
--
-- To have Amazon Translate determine the source language of your text, you can specify @auto@ in the @SourceLanguageCode@ field. If you specify @auto@ , Amazon Translate will call <https://docs.aws.amazon.com/comprehend/latest/dg/comprehend-general.html Amazon Comprehend> to determine the source language.
--
-- /Note:/ Consider using 'sourceLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttSourceLanguageCode :: Lens.Lens' TranslateText Lude.Text
ttSourceLanguageCode = Lens.lens (sourceLanguageCode :: TranslateText -> Lude.Text) (\s a -> s {sourceLanguageCode = a} :: TranslateText)
{-# DEPRECATED ttSourceLanguageCode "Use generic-lens or generic-optics with 'sourceLanguageCode' instead." #-}

-- | The language code requested for the language of the target text. The language must be a language supported by Amazon Translate.
--
-- /Note:/ Consider using 'targetLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttTargetLanguageCode :: Lens.Lens' TranslateText Lude.Text
ttTargetLanguageCode = Lens.lens (targetLanguageCode :: TranslateText -> Lude.Text) (\s a -> s {targetLanguageCode = a} :: TranslateText)
{-# DEPRECATED ttTargetLanguageCode "Use generic-lens or generic-optics with 'targetLanguageCode' instead." #-}

instance Lude.AWSRequest TranslateText where
  type Rs TranslateText = TranslateTextResponse
  request = Req.postJSON translateService
  response =
    Res.receiveJSON
      ( \s h x ->
          TranslateTextResponse'
            Lude.<$> (x Lude..?> "AppliedTerminologies" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "TranslatedText")
            Lude.<*> (x Lude..:> "SourceLanguageCode")
            Lude.<*> (x Lude..:> "TargetLanguageCode")
      )

instance Lude.ToHeaders TranslateText where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSShineFrontendService_20170701.TranslateText" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TranslateText where
  toJSON TranslateText' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TerminologyNames" Lude..=) Lude.<$> terminologyNames,
            Lude.Just ("Text" Lude..= text),
            Lude.Just ("SourceLanguageCode" Lude..= sourceLanguageCode),
            Lude.Just ("TargetLanguageCode" Lude..= targetLanguageCode)
          ]
      )

instance Lude.ToPath TranslateText where
  toPath = Lude.const "/"

instance Lude.ToQuery TranslateText where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTranslateTextResponse' smart constructor.
data TranslateTextResponse = TranslateTextResponse'
  { appliedTerminologies ::
      Lude.Maybe [AppliedTerminology],
    responseStatus :: Lude.Int,
    translatedText :: Lude.Text,
    sourceLanguageCode :: Lude.Text,
    targetLanguageCode :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TranslateTextResponse' with the minimum fields required to make a request.
--
-- * 'appliedTerminologies' - The names of the custom terminologies applied to the input text by Amazon Translate for the translated text response.
-- * 'responseStatus' - The response status code.
-- * 'sourceLanguageCode' - The language code for the language of the source text.
-- * 'targetLanguageCode' - The language code for the language of the target text.
-- * 'translatedText' - The translated text.
mkTranslateTextResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'translatedText'
  Lude.Text ->
  -- | 'sourceLanguageCode'
  Lude.Text ->
  -- | 'targetLanguageCode'
  Lude.Text ->
  TranslateTextResponse
mkTranslateTextResponse
  pResponseStatus_
  pTranslatedText_
  pSourceLanguageCode_
  pTargetLanguageCode_ =
    TranslateTextResponse'
      { appliedTerminologies = Lude.Nothing,
        responseStatus = pResponseStatus_,
        translatedText = pTranslatedText_,
        sourceLanguageCode = pSourceLanguageCode_,
        targetLanguageCode = pTargetLanguageCode_
      }

-- | The names of the custom terminologies applied to the input text by Amazon Translate for the translated text response.
--
-- /Note:/ Consider using 'appliedTerminologies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttrsAppliedTerminologies :: Lens.Lens' TranslateTextResponse (Lude.Maybe [AppliedTerminology])
ttrsAppliedTerminologies = Lens.lens (appliedTerminologies :: TranslateTextResponse -> Lude.Maybe [AppliedTerminology]) (\s a -> s {appliedTerminologies = a} :: TranslateTextResponse)
{-# DEPRECATED ttrsAppliedTerminologies "Use generic-lens or generic-optics with 'appliedTerminologies' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttrsResponseStatus :: Lens.Lens' TranslateTextResponse Lude.Int
ttrsResponseStatus = Lens.lens (responseStatus :: TranslateTextResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TranslateTextResponse)
{-# DEPRECATED ttrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The translated text.
--
-- /Note:/ Consider using 'translatedText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttrsTranslatedText :: Lens.Lens' TranslateTextResponse Lude.Text
ttrsTranslatedText = Lens.lens (translatedText :: TranslateTextResponse -> Lude.Text) (\s a -> s {translatedText = a} :: TranslateTextResponse)
{-# DEPRECATED ttrsTranslatedText "Use generic-lens or generic-optics with 'translatedText' instead." #-}

-- | The language code for the language of the source text.
--
-- /Note:/ Consider using 'sourceLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttrsSourceLanguageCode :: Lens.Lens' TranslateTextResponse Lude.Text
ttrsSourceLanguageCode = Lens.lens (sourceLanguageCode :: TranslateTextResponse -> Lude.Text) (\s a -> s {sourceLanguageCode = a} :: TranslateTextResponse)
{-# DEPRECATED ttrsSourceLanguageCode "Use generic-lens or generic-optics with 'sourceLanguageCode' instead." #-}

-- | The language code for the language of the target text.
--
-- /Note:/ Consider using 'targetLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttrsTargetLanguageCode :: Lens.Lens' TranslateTextResponse Lude.Text
ttrsTargetLanguageCode = Lens.lens (targetLanguageCode :: TranslateTextResponse -> Lude.Text) (\s a -> s {targetLanguageCode = a} :: TranslateTextResponse)
{-# DEPRECATED ttrsTargetLanguageCode "Use generic-lens or generic-optics with 'targetLanguageCode' instead." #-}
