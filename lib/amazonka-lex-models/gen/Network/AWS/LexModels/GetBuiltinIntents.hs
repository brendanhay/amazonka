{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetBuiltinIntents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of built-in intents that meet the specified criteria.
--
-- This operation requires permission for the @lex:GetBuiltinIntents@ action.
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetBuiltinIntents
    (
    -- * Creating a request
      GetBuiltinIntents (..)
    , mkGetBuiltinIntents
    -- ** Request lenses
    , gbiLocale
    , gbiMaxResults
    , gbiNextToken
    , gbiSignatureContains

    -- * Destructuring the response
    , GetBuiltinIntentsResponse (..)
    , mkGetBuiltinIntentsResponse
    -- ** Response lenses
    , grsIntents
    , grsNextToken
    , grsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetBuiltinIntents' smart constructor.
data GetBuiltinIntents = GetBuiltinIntents'
  { locale :: Core.Maybe Types.Locale
    -- ^ A list of locales that the intent supports.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of intents to return in the response. The default is 10.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A pagination token that fetches the next page of intents. If this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of intents, use the pagination token in the next request.
  , signatureContains :: Core.Maybe Core.Text
    -- ^ Substring to match in built-in intent signatures. An intent will be returned if any part of its signature matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz." To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBuiltinIntents' value with any optional fields omitted.
mkGetBuiltinIntents
    :: GetBuiltinIntents
mkGetBuiltinIntents
  = GetBuiltinIntents'{locale = Core.Nothing,
                       maxResults = Core.Nothing, nextToken = Core.Nothing,
                       signatureContains = Core.Nothing}

-- | A list of locales that the intent supports.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbiLocale :: Lens.Lens' GetBuiltinIntents (Core.Maybe Types.Locale)
gbiLocale = Lens.field @"locale"
{-# INLINEABLE gbiLocale #-}
{-# DEPRECATED locale "Use generic-lens or generic-optics with 'locale' instead"  #-}

-- | The maximum number of intents to return in the response. The default is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbiMaxResults :: Lens.Lens' GetBuiltinIntents (Core.Maybe Core.Natural)
gbiMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gbiMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A pagination token that fetches the next page of intents. If this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of intents, use the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbiNextToken :: Lens.Lens' GetBuiltinIntents (Core.Maybe Types.NextToken)
gbiNextToken = Lens.field @"nextToken"
{-# INLINEABLE gbiNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Substring to match in built-in intent signatures. An intent will be returned if any part of its signature matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz." To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
--
-- /Note:/ Consider using 'signatureContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbiSignatureContains :: Lens.Lens' GetBuiltinIntents (Core.Maybe Core.Text)
gbiSignatureContains = Lens.field @"signatureContains"
{-# INLINEABLE gbiSignatureContains #-}
{-# DEPRECATED signatureContains "Use generic-lens or generic-optics with 'signatureContains' instead"  #-}

instance Core.ToQuery GetBuiltinIntents where
        toQuery GetBuiltinIntents{..}
          = Core.maybe Core.mempty (Core.toQueryPair "locale") locale Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "signatureContains")
                signatureContains

instance Core.ToHeaders GetBuiltinIntents where
        toHeaders GetBuiltinIntents{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetBuiltinIntents where
        type Rs GetBuiltinIntents = GetBuiltinIntentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/builtins/intents/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetBuiltinIntentsResponse' Core.<$>
                   (x Core..:? "intents") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetBuiltinIntents where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"intents" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetBuiltinIntentsResponse' smart constructor.
data GetBuiltinIntentsResponse = GetBuiltinIntentsResponse'
  { intents :: Core.Maybe [Types.BuiltinIntentMetadata]
    -- ^ An array of @builtinIntentMetadata@ objects, one for each intent in the response.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A pagination token that fetches the next page of intents. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of intents, specify the pagination token in the next request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBuiltinIntentsResponse' value with any optional fields omitted.
mkGetBuiltinIntentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetBuiltinIntentsResponse
mkGetBuiltinIntentsResponse responseStatus
  = GetBuiltinIntentsResponse'{intents = Core.Nothing,
                               nextToken = Core.Nothing, responseStatus}

-- | An array of @builtinIntentMetadata@ objects, one for each intent in the response.
--
-- /Note:/ Consider using 'intents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsIntents :: Lens.Lens' GetBuiltinIntentsResponse (Core.Maybe [Types.BuiltinIntentMetadata])
grsIntents = Lens.field @"intents"
{-# INLINEABLE grsIntents #-}
{-# DEPRECATED intents "Use generic-lens or generic-optics with 'intents' instead"  #-}

-- | A pagination token that fetches the next page of intents. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of intents, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsNextToken :: Lens.Lens' GetBuiltinIntentsResponse (Core.Maybe Types.NextToken)
grsNextToken = Lens.field @"nextToken"
{-# INLINEABLE grsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetBuiltinIntentsResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
