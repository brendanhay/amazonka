{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.GetDomainSuggestions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The GetDomainSuggestions operation returns a list of suggested domain names.
module Network.AWS.Route53Domains.GetDomainSuggestions
    (
    -- * Creating a request
      GetDomainSuggestions (..)
    , mkGetDomainSuggestions
    -- ** Request lenses
    , gdsDomainName
    , gdsSuggestionCount
    , gdsOnlyAvailable

    -- * Destructuring the response
    , GetDomainSuggestionsResponse (..)
    , mkGetDomainSuggestionsResponse
    -- ** Response lenses
    , gdsrrsSuggestionsList
    , gdsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | /See:/ 'mkGetDomainSuggestions' smart constructor.
data GetDomainSuggestions = GetDomainSuggestions'
  { domainName :: Types.DomainName
    -- ^ A domain name that you want to use as the basis for a list of possible domain names. The top-level domain (TLD), such as .com, must be a TLD that Route 53 supports. For a list of supported TLDs, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
--
-- The domain name can contain only the following characters:
--
--     * Letters a through z. Domain names are not case sensitive.
--
--
--     * Numbers 0 through 9.
--
--
--     * Hyphen (-). You can't specify a hyphen at the beginning or end of a label. 
--
--
--     * Period (.) to separate the labels in the name, such as the @.@ in @example.com@ .
--
--
-- Internationalized domain names are not supported for some top-level domains. To determine whether the TLD that you want to use supports internationalized domain names, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> . 
  , suggestionCount :: Core.Int
    -- ^ The number of suggested domain names that you want Route 53 to return. Specify a value between 1 and 50.
  , onlyAvailable :: Core.Bool
    -- ^ If @OnlyAvailable@ is @true@ , Route 53 returns only domain names that are available. If @OnlyAvailable@ is @false@ , Route 53 returns domain names without checking whether they're available to be registered. To determine whether the domain is available, you can call @checkDomainAvailability@ for each suggestion.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDomainSuggestions' value with any optional fields omitted.
mkGetDomainSuggestions
    :: Types.DomainName -- ^ 'domainName'
    -> Core.Int -- ^ 'suggestionCount'
    -> Core.Bool -- ^ 'onlyAvailable'
    -> GetDomainSuggestions
mkGetDomainSuggestions domainName suggestionCount onlyAvailable
  = GetDomainSuggestions'{domainName, suggestionCount, onlyAvailable}

-- | A domain name that you want to use as the basis for a list of possible domain names. The top-level domain (TLD), such as .com, must be a TLD that Route 53 supports. For a list of supported TLDs, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
--
-- The domain name can contain only the following characters:
--
--     * Letters a through z. Domain names are not case sensitive.
--
--
--     * Numbers 0 through 9.
--
--
--     * Hyphen (-). You can't specify a hyphen at the beginning or end of a label. 
--
--
--     * Period (.) to separate the labels in the name, such as the @.@ in @example.com@ .
--
--
-- Internationalized domain names are not supported for some top-level domains. To determine whether the TLD that you want to use supports internationalized domain names, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> . 
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsDomainName :: Lens.Lens' GetDomainSuggestions Types.DomainName
gdsDomainName = Lens.field @"domainName"
{-# INLINEABLE gdsDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | The number of suggested domain names that you want Route 53 to return. Specify a value between 1 and 50.
--
-- /Note:/ Consider using 'suggestionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsSuggestionCount :: Lens.Lens' GetDomainSuggestions Core.Int
gdsSuggestionCount = Lens.field @"suggestionCount"
{-# INLINEABLE gdsSuggestionCount #-}
{-# DEPRECATED suggestionCount "Use generic-lens or generic-optics with 'suggestionCount' instead"  #-}

-- | If @OnlyAvailable@ is @true@ , Route 53 returns only domain names that are available. If @OnlyAvailable@ is @false@ , Route 53 returns domain names without checking whether they're available to be registered. To determine whether the domain is available, you can call @checkDomainAvailability@ for each suggestion.
--
-- /Note:/ Consider using 'onlyAvailable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsOnlyAvailable :: Lens.Lens' GetDomainSuggestions Core.Bool
gdsOnlyAvailable = Lens.field @"onlyAvailable"
{-# INLINEABLE gdsOnlyAvailable #-}
{-# DEPRECATED onlyAvailable "Use generic-lens or generic-optics with 'onlyAvailable' instead"  #-}

instance Core.ToQuery GetDomainSuggestions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDomainSuggestions where
        toHeaders GetDomainSuggestions{..}
          = Core.pure
              ("X-Amz-Target", "Route53Domains_v20140515.GetDomainSuggestions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDomainSuggestions where
        toJSON GetDomainSuggestions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DomainName" Core..= domainName),
                  Core.Just ("SuggestionCount" Core..= suggestionCount),
                  Core.Just ("OnlyAvailable" Core..= onlyAvailable)])

instance Core.AWSRequest GetDomainSuggestions where
        type Rs GetDomainSuggestions = GetDomainSuggestionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDomainSuggestionsResponse' Core.<$>
                   (x Core..:? "SuggestionsList") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDomainSuggestionsResponse' smart constructor.
data GetDomainSuggestionsResponse = GetDomainSuggestionsResponse'
  { suggestionsList :: Core.Maybe [Types.DomainSuggestion]
    -- ^ A list of possible domain names. If you specified @true@ for @OnlyAvailable@ in the request, the list contains only domains that are available for registration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDomainSuggestionsResponse' value with any optional fields omitted.
mkGetDomainSuggestionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDomainSuggestionsResponse
mkGetDomainSuggestionsResponse responseStatus
  = GetDomainSuggestionsResponse'{suggestionsList = Core.Nothing,
                                  responseStatus}

-- | A list of possible domain names. If you specified @true@ for @OnlyAvailable@ in the request, the list contains only domains that are available for registration.
--
-- /Note:/ Consider using 'suggestionsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsSuggestionsList :: Lens.Lens' GetDomainSuggestionsResponse (Core.Maybe [Types.DomainSuggestion])
gdsrrsSuggestionsList = Lens.field @"suggestionsList"
{-# INLINEABLE gdsrrsSuggestionsList #-}
{-# DEPRECATED suggestionsList "Use generic-lens or generic-optics with 'suggestionsList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsResponseStatus :: Lens.Lens' GetDomainSuggestionsResponse Core.Int
gdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
