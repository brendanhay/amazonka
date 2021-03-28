{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DetectSentiment (..)
    , mkDetectSentiment
    -- ** Request lenses
    , dsText
    , dsLanguageCode

    -- * Destructuring the response
    , DetectSentimentResponse (..)
    , mkDetectSentimentResponse
    -- ** Response lenses
    , dsrfrsSentiment
    , dsrfrsSentimentScore
    , dsrfrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetectSentiment' smart constructor.
data DetectSentiment = DetectSentiment'
  { text :: Types.CustomerInputString
    -- ^ A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
  , languageCode :: Types.LanguageCode
    -- ^ The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectSentiment' value with any optional fields omitted.
mkDetectSentiment
    :: Types.CustomerInputString -- ^ 'text'
    -> Types.LanguageCode -- ^ 'languageCode'
    -> DetectSentiment
mkDetectSentiment text languageCode
  = DetectSentiment'{text, languageCode}

-- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsText :: Lens.Lens' DetectSentiment Types.CustomerInputString
dsText = Lens.field @"text"
{-# INLINEABLE dsText #-}
{-# DEPRECATED text "Use generic-lens or generic-optics with 'text' instead"  #-}

-- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLanguageCode :: Lens.Lens' DetectSentiment Types.LanguageCode
dsLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE dsLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

instance Core.ToQuery DetectSentiment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DetectSentiment where
        toHeaders DetectSentiment{..}
          = Core.pure ("X-Amz-Target", "Comprehend_20171127.DetectSentiment")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DetectSentiment where
        toJSON DetectSentiment{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Text" Core..= text),
                  Core.Just ("LanguageCode" Core..= languageCode)])

instance Core.AWSRequest DetectSentiment where
        type Rs DetectSentiment = DetectSentimentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DetectSentimentResponse' Core.<$>
                   (x Core..:? "Sentiment") Core.<*> x Core..:? "SentimentScore"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDetectSentimentResponse' smart constructor.
data DetectSentimentResponse = DetectSentimentResponse'
  { sentiment :: Core.Maybe Types.SentimentType
    -- ^ The inferred sentiment that Amazon Comprehend has the highest level of confidence in.
  , sentimentScore :: Core.Maybe Types.SentimentScore
    -- ^ An object that lists the sentiments, and their corresponding confidence levels.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectSentimentResponse' value with any optional fields omitted.
mkDetectSentimentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DetectSentimentResponse
mkDetectSentimentResponse responseStatus
  = DetectSentimentResponse'{sentiment = Core.Nothing,
                             sentimentScore = Core.Nothing, responseStatus}

-- | The inferred sentiment that Amazon Comprehend has the highest level of confidence in.
--
-- /Note:/ Consider using 'sentiment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsSentiment :: Lens.Lens' DetectSentimentResponse (Core.Maybe Types.SentimentType)
dsrfrsSentiment = Lens.field @"sentiment"
{-# INLINEABLE dsrfrsSentiment #-}
{-# DEPRECATED sentiment "Use generic-lens or generic-optics with 'sentiment' instead"  #-}

-- | An object that lists the sentiments, and their corresponding confidence levels.
--
-- /Note:/ Consider using 'sentimentScore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsSentimentScore :: Lens.Lens' DetectSentimentResponse (Core.Maybe Types.SentimentScore)
dsrfrsSentimentScore = Lens.field @"sentimentScore"
{-# INLINEABLE dsrfrsSentimentScore #-}
{-# DEPRECATED sentimentScore "Use generic-lens or generic-optics with 'sentimentScore' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsResponseStatus :: Lens.Lens' DetectSentimentResponse Core.Int
dsrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
