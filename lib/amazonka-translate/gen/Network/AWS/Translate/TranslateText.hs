{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      TranslateText (..)
    , mkTranslateText
    -- ** Request lenses
    , ttText
    , ttSourceLanguageCode
    , ttTargetLanguageCode
    , ttTerminologyNames

    -- * Destructuring the response
    , TranslateTextResponse (..)
    , mkTranslateTextResponse
    -- ** Response lenses
    , ttrrsTranslatedText
    , ttrrsSourceLanguageCode
    , ttrrsTargetLanguageCode
    , ttrrsAppliedTerminologies
    , ttrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Translate.Types as Types

-- | /See:/ 'mkTranslateText' smart constructor.
data TranslateText = TranslateText'
  { text :: Types.Text
    -- ^ The text to translate. The text string can be a maximum of 5,000 bytes long. Depending on your character set, this may be fewer than 5,000 characters.
  , sourceLanguageCode :: Types.SourceLanguageCode
    -- ^ The language code for the language of the source text. The language must be a language supported by Amazon Translate. For a list of language codes, see 'what-is-languages' .
--
-- To have Amazon Translate determine the source language of your text, you can specify @auto@ in the @SourceLanguageCode@ field. If you specify @auto@ , Amazon Translate will call <https://docs.aws.amazon.com/comprehend/latest/dg/comprehend-general.html Amazon Comprehend> to determine the source language.
  , targetLanguageCode :: Types.TargetLanguageCode
    -- ^ The language code requested for the language of the target text. The language must be a language supported by Amazon Translate.
  , terminologyNames :: Core.Maybe [Types.ResourceName]
    -- ^ The name of the terminology list file to be used in the TranslateText request. You can use 1 terminology list at most in a @TranslateText@ request. Terminology lists can contain a maximum of 256 terms.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TranslateText' value with any optional fields omitted.
mkTranslateText
    :: Types.Text -- ^ 'text'
    -> Types.SourceLanguageCode -- ^ 'sourceLanguageCode'
    -> Types.TargetLanguageCode -- ^ 'targetLanguageCode'
    -> TranslateText
mkTranslateText text sourceLanguageCode targetLanguageCode
  = TranslateText'{text, sourceLanguageCode, targetLanguageCode,
                   terminologyNames = Core.Nothing}

-- | The text to translate. The text string can be a maximum of 5,000 bytes long. Depending on your character set, this may be fewer than 5,000 characters.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttText :: Lens.Lens' TranslateText Types.Text
ttText = Lens.field @"text"
{-# INLINEABLE ttText #-}
{-# DEPRECATED text "Use generic-lens or generic-optics with 'text' instead"  #-}

-- | The language code for the language of the source text. The language must be a language supported by Amazon Translate. For a list of language codes, see 'what-is-languages' .
--
-- To have Amazon Translate determine the source language of your text, you can specify @auto@ in the @SourceLanguageCode@ field. If you specify @auto@ , Amazon Translate will call <https://docs.aws.amazon.com/comprehend/latest/dg/comprehend-general.html Amazon Comprehend> to determine the source language.
--
-- /Note:/ Consider using 'sourceLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttSourceLanguageCode :: Lens.Lens' TranslateText Types.SourceLanguageCode
ttSourceLanguageCode = Lens.field @"sourceLanguageCode"
{-# INLINEABLE ttSourceLanguageCode #-}
{-# DEPRECATED sourceLanguageCode "Use generic-lens or generic-optics with 'sourceLanguageCode' instead"  #-}

-- | The language code requested for the language of the target text. The language must be a language supported by Amazon Translate.
--
-- /Note:/ Consider using 'targetLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttTargetLanguageCode :: Lens.Lens' TranslateText Types.TargetLanguageCode
ttTargetLanguageCode = Lens.field @"targetLanguageCode"
{-# INLINEABLE ttTargetLanguageCode #-}
{-# DEPRECATED targetLanguageCode "Use generic-lens or generic-optics with 'targetLanguageCode' instead"  #-}

-- | The name of the terminology list file to be used in the TranslateText request. You can use 1 terminology list at most in a @TranslateText@ request. Terminology lists can contain a maximum of 256 terms.
--
-- /Note:/ Consider using 'terminologyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttTerminologyNames :: Lens.Lens' TranslateText (Core.Maybe [Types.ResourceName])
ttTerminologyNames = Lens.field @"terminologyNames"
{-# INLINEABLE ttTerminologyNames #-}
{-# DEPRECATED terminologyNames "Use generic-lens or generic-optics with 'terminologyNames' instead"  #-}

instance Core.ToQuery TranslateText where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders TranslateText where
        toHeaders TranslateText{..}
          = Core.pure
              ("X-Amz-Target", "AWSShineFrontendService_20170701.TranslateText")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON TranslateText where
        toJSON TranslateText{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Text" Core..= text),
                  Core.Just ("SourceLanguageCode" Core..= sourceLanguageCode),
                  Core.Just ("TargetLanguageCode" Core..= targetLanguageCode),
                  ("TerminologyNames" Core..=) Core.<$> terminologyNames])

instance Core.AWSRequest TranslateText where
        type Rs TranslateText = TranslateTextResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 TranslateTextResponse' Core.<$>
                   (x Core..: "TranslatedText") Core.<*>
                     x Core..: "SourceLanguageCode"
                     Core.<*> x Core..: "TargetLanguageCode"
                     Core.<*> x Core..:? "AppliedTerminologies"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkTranslateTextResponse' smart constructor.
data TranslateTextResponse = TranslateTextResponse'
  { translatedText :: Core.Text
    -- ^ The translated text.
  , sourceLanguageCode :: Types.LanguageCodeString
    -- ^ The language code for the language of the source text.
  , targetLanguageCode :: Types.LanguageCodeString
    -- ^ The language code for the language of the target text. 
  , appliedTerminologies :: Core.Maybe [Types.AppliedTerminology]
    -- ^ The names of the custom terminologies applied to the input text by Amazon Translate for the translated text response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TranslateTextResponse' value with any optional fields omitted.
mkTranslateTextResponse
    :: Core.Text -- ^ 'translatedText'
    -> Types.LanguageCodeString -- ^ 'sourceLanguageCode'
    -> Types.LanguageCodeString -- ^ 'targetLanguageCode'
    -> Core.Int -- ^ 'responseStatus'
    -> TranslateTextResponse
mkTranslateTextResponse translatedText sourceLanguageCode
  targetLanguageCode responseStatus
  = TranslateTextResponse'{translatedText, sourceLanguageCode,
                           targetLanguageCode, appliedTerminologies = Core.Nothing,
                           responseStatus}

-- | The translated text.
--
-- /Note:/ Consider using 'translatedText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttrrsTranslatedText :: Lens.Lens' TranslateTextResponse Core.Text
ttrrsTranslatedText = Lens.field @"translatedText"
{-# INLINEABLE ttrrsTranslatedText #-}
{-# DEPRECATED translatedText "Use generic-lens or generic-optics with 'translatedText' instead"  #-}

-- | The language code for the language of the source text.
--
-- /Note:/ Consider using 'sourceLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttrrsSourceLanguageCode :: Lens.Lens' TranslateTextResponse Types.LanguageCodeString
ttrrsSourceLanguageCode = Lens.field @"sourceLanguageCode"
{-# INLINEABLE ttrrsSourceLanguageCode #-}
{-# DEPRECATED sourceLanguageCode "Use generic-lens or generic-optics with 'sourceLanguageCode' instead"  #-}

-- | The language code for the language of the target text. 
--
-- /Note:/ Consider using 'targetLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttrrsTargetLanguageCode :: Lens.Lens' TranslateTextResponse Types.LanguageCodeString
ttrrsTargetLanguageCode = Lens.field @"targetLanguageCode"
{-# INLINEABLE ttrrsTargetLanguageCode #-}
{-# DEPRECATED targetLanguageCode "Use generic-lens or generic-optics with 'targetLanguageCode' instead"  #-}

-- | The names of the custom terminologies applied to the input text by Amazon Translate for the translated text response.
--
-- /Note:/ Consider using 'appliedTerminologies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttrrsAppliedTerminologies :: Lens.Lens' TranslateTextResponse (Core.Maybe [Types.AppliedTerminology])
ttrrsAppliedTerminologies = Lens.field @"appliedTerminologies"
{-# INLINEABLE ttrrsAppliedTerminologies #-}
{-# DEPRECATED appliedTerminologies "Use generic-lens or generic-optics with 'appliedTerminologies' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttrrsResponseStatus :: Lens.Lens' TranslateTextResponse Core.Int
ttrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ttrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
