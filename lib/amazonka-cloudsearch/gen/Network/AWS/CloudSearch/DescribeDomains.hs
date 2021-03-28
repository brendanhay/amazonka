{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeDomains
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the search domains owned by this account. Can be limited to specific domains. Shows all domains by default. To get the number of searchable documents in a domain, use the console or submit a @matchall@ request to your domain's search endpoint: @q=matchall&amp;q.parser=structured&amp;size=0@ . For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-domain-info.html Getting Information about a Search Domain> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DescribeDomains
    (
    -- * Creating a request
      DescribeDomains (..)
    , mkDescribeDomains
    -- ** Request lenses
    , ddDomainNames

    -- * Destructuring the response
    , DescribeDomainsResponse (..)
    , mkDescribeDomainsResponse
    -- ** Response lenses
    , ddrfrsDomainStatusList
    , ddrfrsResponseStatus
    ) where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DescribeDomains' @ operation. By default shows the status of all domains. To restrict the response to particular domains, specify the names of the domains you want to describe.
--
-- /See:/ 'mkDescribeDomains' smart constructor.
newtype DescribeDomains = DescribeDomains'
  { domainNames :: Core.Maybe [Types.DomainName]
    -- ^ The names of the domains you want to include in the response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDomains' value with any optional fields omitted.
mkDescribeDomains
    :: DescribeDomains
mkDescribeDomains = DescribeDomains'{domainNames = Core.Nothing}

-- | The names of the domains you want to include in the response.
--
-- /Note:/ Consider using 'domainNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDomainNames :: Lens.Lens' DescribeDomains (Core.Maybe [Types.DomainName])
ddDomainNames = Lens.field @"domainNames"
{-# INLINEABLE ddDomainNames #-}
{-# DEPRECATED domainNames "Use generic-lens or generic-optics with 'domainNames' instead"  #-}

instance Core.ToQuery DescribeDomains where
        toQuery DescribeDomains{..}
          = Core.toQueryPair "Action" ("DescribeDomains" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2013-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "DomainNames"
                (Core.maybe Core.mempty (Core.toQueryList "member") domainNames)

instance Core.ToHeaders DescribeDomains where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeDomains where
        type Rs DescribeDomains = DescribeDomainsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeDomainsResult"
              (\ s h x ->
                 DescribeDomainsResponse' Core.<$>
                   (x Core..@ "DomainStatusList" Core..@! Core.mempty Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @DescribeDomains@ request. Contains the status of the domains specified in the request or all domains owned by the account.
--
-- /See:/ 'mkDescribeDomainsResponse' smart constructor.
data DescribeDomainsResponse = DescribeDomainsResponse'
  { domainStatusList :: [Types.DomainStatus]
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDomainsResponse' value with any optional fields omitted.
mkDescribeDomainsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDomainsResponse
mkDescribeDomainsResponse responseStatus
  = DescribeDomainsResponse'{domainStatusList = Core.mempty,
                             responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainStatusList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrfrsDomainStatusList :: Lens.Lens' DescribeDomainsResponse [Types.DomainStatus]
ddrfrsDomainStatusList = Lens.field @"domainStatusList"
{-# INLINEABLE ddrfrsDomainStatusList #-}
{-# DEPRECATED domainStatusList "Use generic-lens or generic-optics with 'domainStatusList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrfrsResponseStatus :: Lens.Lens' DescribeDomainsResponse Core.Int
ddrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
