{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.DescribeVoices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of voices that are available for use when requesting speech synthesis. Each voice speaks a specified language, is either male or female, and is identified by an ID, which is the ASCII version of the voice name. 
--
-- When synthesizing speech ( @SynthesizeSpeech@ ), you provide the voice ID for the voice you want from the list of voices returned by @DescribeVoices@ .
-- For example, you want your news reader application to read news in a specific language, but giving a user the option to choose the voice. Using the @DescribeVoices@ operation you can provide the user with a list of available voices to select from.
-- You can optionally specify a language code to filter the available voices. For example, if you specify @en-US@ , the operation returns a list of all available US English voices. 
-- This operation requires permissions to perform the @polly:DescribeVoices@ action.
--
-- This operation returns paginated results.
module Network.AWS.Polly.DescribeVoices
    (
    -- * Creating a request
      DescribeVoices (..)
    , mkDescribeVoices
    -- ** Request lenses
    , dvEngine
    , dvIncludeAdditionalLanguageCodes
    , dvLanguageCode
    , dvNextToken

    -- * Destructuring the response
    , DescribeVoicesResponse (..)
    , mkDescribeVoicesResponse
    -- ** Response lenses
    , dvrrsNextToken
    , dvrrsVoices
    , dvrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Polly.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeVoices' smart constructor.
data DescribeVoices = DescribeVoices'
  { engine :: Core.Maybe Types.Engine
    -- ^ Specifies the engine (@standard@ or @neural@ ) used by Amazon Polly when processing input text for speech synthesis. 
  , includeAdditionalLanguageCodes :: Core.Maybe Core.Bool
    -- ^ Boolean value indicating whether to return any bilingual voices that use the specified language as an additional language. For instance, if you request all languages that use US English (es-US), and there is an Italian voice that speaks both Italian (it-IT) and US English, that voice will be included if you specify @yes@ but not if you specify @no@ .
  , languageCode :: Core.Maybe Types.LanguageCode
    -- ^ The language identification tag (ISO 639 code for the language name-ISO 3166 country code) for filtering the list of voices returned. If you don't specify this optional parameter, all available voices are returned. 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An opaque pagination token returned from the previous @DescribeVoices@ operation. If present, this indicates where to continue the listing.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVoices' value with any optional fields omitted.
mkDescribeVoices
    :: DescribeVoices
mkDescribeVoices
  = DescribeVoices'{engine = Core.Nothing,
                    includeAdditionalLanguageCodes = Core.Nothing,
                    languageCode = Core.Nothing, nextToken = Core.Nothing}

-- | Specifies the engine (@standard@ or @neural@ ) used by Amazon Polly when processing input text for speech synthesis. 
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvEngine :: Lens.Lens' DescribeVoices (Core.Maybe Types.Engine)
dvEngine = Lens.field @"engine"
{-# INLINEABLE dvEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | Boolean value indicating whether to return any bilingual voices that use the specified language as an additional language. For instance, if you request all languages that use US English (es-US), and there is an Italian voice that speaks both Italian (it-IT) and US English, that voice will be included if you specify @yes@ but not if you specify @no@ .
--
-- /Note:/ Consider using 'includeAdditionalLanguageCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvIncludeAdditionalLanguageCodes :: Lens.Lens' DescribeVoices (Core.Maybe Core.Bool)
dvIncludeAdditionalLanguageCodes = Lens.field @"includeAdditionalLanguageCodes"
{-# INLINEABLE dvIncludeAdditionalLanguageCodes #-}
{-# DEPRECATED includeAdditionalLanguageCodes "Use generic-lens or generic-optics with 'includeAdditionalLanguageCodes' instead"  #-}

-- | The language identification tag (ISO 639 code for the language name-ISO 3166 country code) for filtering the list of voices returned. If you don't specify this optional parameter, all available voices are returned. 
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvLanguageCode :: Lens.Lens' DescribeVoices (Core.Maybe Types.LanguageCode)
dvLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE dvLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | An opaque pagination token returned from the previous @DescribeVoices@ operation. If present, this indicates where to continue the listing.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvNextToken :: Lens.Lens' DescribeVoices (Core.Maybe Types.NextToken)
dvNextToken = Lens.field @"nextToken"
{-# INLINEABLE dvNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeVoices where
        toQuery DescribeVoices{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Engine") engine Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "IncludeAdditionalLanguageCodes")
                includeAdditionalLanguageCodes
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LanguageCode")
                languageCode
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeVoices where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeVoices where
        type Rs DescribeVoices = DescribeVoicesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/v1/voices",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeVoicesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Voices" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeVoices where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"voices" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeVoicesResponse' smart constructor.
data DescribeVoicesResponse = DescribeVoicesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token to use in the next request to continue the listing of voices. @NextToken@ is returned only if the response is truncated.
  , voices :: Core.Maybe [Types.Voice]
    -- ^ A list of voices with their properties.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVoicesResponse' value with any optional fields omitted.
mkDescribeVoicesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeVoicesResponse
mkDescribeVoicesResponse responseStatus
  = DescribeVoicesResponse'{nextToken = Core.Nothing,
                            voices = Core.Nothing, responseStatus}

-- | The pagination token to use in the next request to continue the listing of voices. @NextToken@ is returned only if the response is truncated.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrrsNextToken :: Lens.Lens' DescribeVoicesResponse (Core.Maybe Types.NextToken)
dvrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dvrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of voices with their properties.
--
-- /Note:/ Consider using 'voices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrrsVoices :: Lens.Lens' DescribeVoicesResponse (Core.Maybe [Types.Voice])
dvrrsVoices = Lens.field @"voices"
{-# INLINEABLE dvrrsVoices #-}
{-# DEPRECATED voices "Use generic-lens or generic-optics with 'voices' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrrsResponseStatus :: Lens.Lens' DescribeVoicesResponse Core.Int
dvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
