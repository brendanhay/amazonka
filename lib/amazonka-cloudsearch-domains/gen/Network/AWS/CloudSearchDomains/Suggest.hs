{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Suggest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves autocomplete suggestions for a partial query string. You can use suggestions enable you to display likely matches before users finish typing. In Amazon CloudSearch, suggestions are based on the contents of a particular text field. When you request suggestions, Amazon CloudSearch finds all of the documents whose values in the suggester field start with the specified query string. The beginning of the field must match the query string to be considered a match. 
--
-- For more information about configuring suggesters and retrieving suggestions, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html Getting Suggestions> in the /Amazon CloudSearch Developer Guide/ . 
-- The endpoint for submitting @Suggest@ requests is domain-specific. You submit suggest requests to a domain's search endpoint. To get the search endpoint for your domain, use the Amazon CloudSearch configuration service @DescribeDomains@ action. A domain's endpoints are also displayed on the domain dashboard in the Amazon CloudSearch console. 
module Network.AWS.CloudSearchDomains.Suggest
    (
    -- * Creating a request
      Suggest (..)
    , mkSuggest
    -- ** Request lenses
    , sQuery
    , sSuggester
    , sSize

    -- * Destructuring the response
    , SuggestResponse (..)
    , mkSuggestResponse
    -- ** Response lenses
    , srrsStatus
    , srrsSuggest
    , srrsResponseStatus
    ) where

import qualified Network.AWS.CloudSearchDomains.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @Suggest@ request.
--
-- /See:/ 'mkSuggest' smart constructor.
data Suggest = Suggest'
  { query :: Types.Query
    -- ^ Specifies the string for which you want to get suggestions.
  , suggester :: Types.Suggester
    -- ^ Specifies the name of the suggester to use to find suggested matches.
  , size :: Core.Maybe Core.Integer
    -- ^ Specifies the maximum number of suggestions to return. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Suggest' value with any optional fields omitted.
mkSuggest
    :: Types.Query -- ^ 'query'
    -> Types.Suggester -- ^ 'suggester'
    -> Suggest
mkSuggest query suggester
  = Suggest'{query, suggester, size = Core.Nothing}

-- | Specifies the string for which you want to get suggestions.
--
-- /Note:/ Consider using 'query' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sQuery :: Lens.Lens' Suggest Types.Query
sQuery = Lens.field @"query"
{-# INLINEABLE sQuery #-}
{-# DEPRECATED query "Use generic-lens or generic-optics with 'query' instead"  #-}

-- | Specifies the name of the suggester to use to find suggested matches.
--
-- /Note:/ Consider using 'suggester' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSuggester :: Lens.Lens' Suggest Types.Suggester
sSuggester = Lens.field @"suggester"
{-# INLINEABLE sSuggester #-}
{-# DEPRECATED suggester "Use generic-lens or generic-optics with 'suggester' instead"  #-}

-- | Specifies the maximum number of suggestions to return. 
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSize :: Lens.Lens' Suggest (Core.Maybe Core.Integer)
sSize = Lens.field @"size"
{-# INLINEABLE sSize #-}
{-# DEPRECATED size "Use generic-lens or generic-optics with 'size' instead"  #-}

instance Core.ToQuery Suggest where
        toQuery Suggest{..}
          = Core.toQueryPair "q" query Core.<>
              Core.toQueryPair "suggester" suggester
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "size") size
              Core.<> Core.toQueryPair "format=sdk&pretty=true" ("" :: Core.Text)

instance Core.ToHeaders Suggest where
        toHeaders Suggest{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest Suggest where
        type Rs Suggest = SuggestResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/2013-01-01/suggest",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SuggestResponse' Core.<$>
                   (x Core..:? "status") Core.<*> x Core..:? "suggest" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a @Suggest@ request.
--
-- /See:/ 'mkSuggestResponse' smart constructor.
data SuggestResponse = SuggestResponse'
  { status :: Core.Maybe Types.SuggestStatus
    -- ^ The status of a @SuggestRequest@ . Contains the resource ID (@rid@ ) and how long it took to process the request (@timems@ ).
  , suggest :: Core.Maybe Types.SuggestModel
    -- ^ Container for the matching search suggestion information.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SuggestResponse' value with any optional fields omitted.
mkSuggestResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SuggestResponse
mkSuggestResponse responseStatus
  = SuggestResponse'{status = Core.Nothing, suggest = Core.Nothing,
                     responseStatus}

-- | The status of a @SuggestRequest@ . Contains the resource ID (@rid@ ) and how long it took to process the request (@timems@ ).
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsStatus :: Lens.Lens' SuggestResponse (Core.Maybe Types.SuggestStatus)
srrsStatus = Lens.field @"status"
{-# INLINEABLE srrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Container for the matching search suggestion information.
--
-- /Note:/ Consider using 'suggest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsSuggest :: Lens.Lens' SuggestResponse (Core.Maybe Types.SuggestModel)
srrsSuggest = Lens.field @"suggest"
{-# INLINEABLE srrsSuggest #-}
{-# DEPRECATED suggest "Use generic-lens or generic-optics with 'suggest' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsResponseStatus :: Lens.Lens' SuggestResponse Core.Int
srrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
