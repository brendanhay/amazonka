{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DetectKeyPhrases (..)
    , mkDetectKeyPhrases
    -- ** Request lenses
    , dkpText
    , dkpLanguageCode

    -- * Destructuring the response
    , DetectKeyPhrasesResponse (..)
    , mkDetectKeyPhrasesResponse
    -- ** Response lenses
    , dkprrsKeyPhrases
    , dkprrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetectKeyPhrases' smart constructor.
data DetectKeyPhrases = DetectKeyPhrases'
  { text :: Types.CustomerInputString
    -- ^ A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
  , languageCode :: Types.LanguageCode
    -- ^ The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectKeyPhrases' value with any optional fields omitted.
mkDetectKeyPhrases
    :: Types.CustomerInputString -- ^ 'text'
    -> Types.LanguageCode -- ^ 'languageCode'
    -> DetectKeyPhrases
mkDetectKeyPhrases text languageCode
  = DetectKeyPhrases'{text, languageCode}

-- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpText :: Lens.Lens' DetectKeyPhrases Types.CustomerInputString
dkpText = Lens.field @"text"
{-# INLINEABLE dkpText #-}
{-# DEPRECATED text "Use generic-lens or generic-optics with 'text' instead"  #-}

-- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpLanguageCode :: Lens.Lens' DetectKeyPhrases Types.LanguageCode
dkpLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE dkpLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

instance Core.ToQuery DetectKeyPhrases where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DetectKeyPhrases where
        toHeaders DetectKeyPhrases{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.DetectKeyPhrases")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DetectKeyPhrases where
        toJSON DetectKeyPhrases{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Text" Core..= text),
                  Core.Just ("LanguageCode" Core..= languageCode)])

instance Core.AWSRequest DetectKeyPhrases where
        type Rs DetectKeyPhrases = DetectKeyPhrasesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DetectKeyPhrasesResponse' Core.<$>
                   (x Core..:? "KeyPhrases") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDetectKeyPhrasesResponse' smart constructor.
data DetectKeyPhrasesResponse = DetectKeyPhrasesResponse'
  { keyPhrases :: Core.Maybe [Types.KeyPhrase]
    -- ^ A collection of key phrases that Amazon Comprehend identified in the input text. For each key phrase, the response provides the text of the key phrase, where the key phrase begins and ends, and the level of confidence that Amazon Comprehend has in the accuracy of the detection. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectKeyPhrasesResponse' value with any optional fields omitted.
mkDetectKeyPhrasesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DetectKeyPhrasesResponse
mkDetectKeyPhrasesResponse responseStatus
  = DetectKeyPhrasesResponse'{keyPhrases = Core.Nothing,
                              responseStatus}

-- | A collection of key phrases that Amazon Comprehend identified in the input text. For each key phrase, the response provides the text of the key phrase, where the key phrase begins and ends, and the level of confidence that Amazon Comprehend has in the accuracy of the detection. 
--
-- /Note:/ Consider using 'keyPhrases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkprrsKeyPhrases :: Lens.Lens' DetectKeyPhrasesResponse (Core.Maybe [Types.KeyPhrase])
dkprrsKeyPhrases = Lens.field @"keyPhrases"
{-# INLINEABLE dkprrsKeyPhrases #-}
{-# DEPRECATED keyPhrases "Use generic-lens or generic-optics with 'keyPhrases' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkprrsResponseStatus :: Lens.Lens' DetectKeyPhrasesResponse Core.Int
dkprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dkprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
