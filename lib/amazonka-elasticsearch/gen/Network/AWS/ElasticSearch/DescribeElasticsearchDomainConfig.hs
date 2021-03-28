{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DescribeElasticsearchDomainConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides cluster configuration information about the specified Elasticsearch domain, such as the state, creation date, update version, and update date for cluster options.
module Network.AWS.ElasticSearch.DescribeElasticsearchDomainConfig
    (
    -- * Creating a request
      DescribeElasticsearchDomainConfig (..)
    , mkDescribeElasticsearchDomainConfig
    -- ** Request lenses
    , dedcDomainName

    -- * Destructuring the response
    , DescribeElasticsearchDomainConfigResponse (..)
    , mkDescribeElasticsearchDomainConfigResponse
    -- ** Response lenses
    , dedcrrsDomainConfig
    , dedcrrsResponseStatus
    ) where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @DescribeElasticsearchDomainConfig@ operation. Specifies the domain name for which you want configuration information.
--
-- /See:/ 'mkDescribeElasticsearchDomainConfig' smart constructor.
newtype DescribeElasticsearchDomainConfig = DescribeElasticsearchDomainConfig'
  { domainName :: Types.DomainName
    -- ^ The Elasticsearch domain that you want to get information about.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeElasticsearchDomainConfig' value with any optional fields omitted.
mkDescribeElasticsearchDomainConfig
    :: Types.DomainName -- ^ 'domainName'
    -> DescribeElasticsearchDomainConfig
mkDescribeElasticsearchDomainConfig domainName
  = DescribeElasticsearchDomainConfig'{domainName}

-- | The Elasticsearch domain that you want to get information about.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedcDomainName :: Lens.Lens' DescribeElasticsearchDomainConfig Types.DomainName
dedcDomainName = Lens.field @"domainName"
{-# INLINEABLE dedcDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.ToQuery DescribeElasticsearchDomainConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeElasticsearchDomainConfig where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeElasticsearchDomainConfig where
        type Rs DescribeElasticsearchDomainConfig =
             DescribeElasticsearchDomainConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2015-01-01/es/domain/" Core.<> Core.toText domainName Core.<>
                             "/config",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeElasticsearchDomainConfigResponse' Core.<$>
                   (x Core..: "DomainConfig") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @DescribeElasticsearchDomainConfig@ request. Contains the configuration information of the requested domain.
--
-- /See:/ 'mkDescribeElasticsearchDomainConfigResponse' smart constructor.
data DescribeElasticsearchDomainConfigResponse = DescribeElasticsearchDomainConfigResponse'
  { domainConfig :: Types.ElasticsearchDomainConfig
    -- ^ The configuration information of the domain requested in the @DescribeElasticsearchDomainConfig@ request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeElasticsearchDomainConfigResponse' value with any optional fields omitted.
mkDescribeElasticsearchDomainConfigResponse
    :: Types.ElasticsearchDomainConfig -- ^ 'domainConfig'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeElasticsearchDomainConfigResponse
mkDescribeElasticsearchDomainConfigResponse domainConfig
  responseStatus
  = DescribeElasticsearchDomainConfigResponse'{domainConfig,
                                               responseStatus}

-- | The configuration information of the domain requested in the @DescribeElasticsearchDomainConfig@ request.
--
-- /Note:/ Consider using 'domainConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedcrrsDomainConfig :: Lens.Lens' DescribeElasticsearchDomainConfigResponse Types.ElasticsearchDomainConfig
dedcrrsDomainConfig = Lens.field @"domainConfig"
{-# INLINEABLE dedcrrsDomainConfig #-}
{-# DEPRECATED domainConfig "Use generic-lens or generic-optics with 'domainConfig' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedcrrsResponseStatus :: Lens.Lens' DescribeElasticsearchDomainConfigResponse Core.Int
dedcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dedcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
