{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DescribeElasticsearchInstanceTypeLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe Elasticsearch Limits for a given InstanceType and ElasticsearchVersion. When modifying existing Domain, specify the @'DomainName' @ to know what Limits are supported for modifying. 
module Network.AWS.ElasticSearch.DescribeElasticsearchInstanceTypeLimits
    (
    -- * Creating a request
      DescribeElasticsearchInstanceTypeLimits (..)
    , mkDescribeElasticsearchInstanceTypeLimits
    -- ** Request lenses
    , deitlInstanceType
    , deitlElasticsearchVersion
    , deitlDomainName

    -- * Destructuring the response
    , DescribeElasticsearchInstanceTypeLimitsResponse (..)
    , mkDescribeElasticsearchInstanceTypeLimitsResponse
    -- ** Response lenses
    , deitlrrsLimitsByRole
    , deitlrrsResponseStatus
    ) where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to @'DescribeElasticsearchInstanceTypeLimits' @ operation. 
--
-- /See:/ 'mkDescribeElasticsearchInstanceTypeLimits' smart constructor.
data DescribeElasticsearchInstanceTypeLimits = DescribeElasticsearchInstanceTypeLimits'
  { instanceType :: Types.ESPartitionInstanceType
    -- ^ The instance type for an Elasticsearch cluster for which Elasticsearch @'Limits' @ are needed. 
  , elasticsearchVersion :: Types.ElasticsearchVersion
    -- ^ Version of Elasticsearch for which @'Limits' @ are needed. 
  , domainName :: Core.Maybe Types.DomainName
    -- ^ DomainName represents the name of the Domain that we are trying to modify. This should be present only if we are querying for Elasticsearch @'Limits' @ for existing domain. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeElasticsearchInstanceTypeLimits' value with any optional fields omitted.
mkDescribeElasticsearchInstanceTypeLimits
    :: Types.ESPartitionInstanceType -- ^ 'instanceType'
    -> Types.ElasticsearchVersion -- ^ 'elasticsearchVersion'
    -> DescribeElasticsearchInstanceTypeLimits
mkDescribeElasticsearchInstanceTypeLimits instanceType
  elasticsearchVersion
  = DescribeElasticsearchInstanceTypeLimits'{instanceType,
                                             elasticsearchVersion, domainName = Core.Nothing}

-- | The instance type for an Elasticsearch cluster for which Elasticsearch @'Limits' @ are needed. 
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deitlInstanceType :: Lens.Lens' DescribeElasticsearchInstanceTypeLimits Types.ESPartitionInstanceType
deitlInstanceType = Lens.field @"instanceType"
{-# INLINEABLE deitlInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | Version of Elasticsearch for which @'Limits' @ are needed. 
--
-- /Note:/ Consider using 'elasticsearchVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deitlElasticsearchVersion :: Lens.Lens' DescribeElasticsearchInstanceTypeLimits Types.ElasticsearchVersion
deitlElasticsearchVersion = Lens.field @"elasticsearchVersion"
{-# INLINEABLE deitlElasticsearchVersion #-}
{-# DEPRECATED elasticsearchVersion "Use generic-lens or generic-optics with 'elasticsearchVersion' instead"  #-}

-- | DomainName represents the name of the Domain that we are trying to modify. This should be present only if we are querying for Elasticsearch @'Limits' @ for existing domain. 
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deitlDomainName :: Lens.Lens' DescribeElasticsearchInstanceTypeLimits (Core.Maybe Types.DomainName)
deitlDomainName = Lens.field @"domainName"
{-# INLINEABLE deitlDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.ToQuery DescribeElasticsearchInstanceTypeLimits where
        toQuery DescribeElasticsearchInstanceTypeLimits{..}
          = Core.maybe Core.mempty (Core.toQueryPair "domainName") domainName

instance Core.ToHeaders DescribeElasticsearchInstanceTypeLimits
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeElasticsearchInstanceTypeLimits
         where
        type Rs DescribeElasticsearchInstanceTypeLimits =
             DescribeElasticsearchInstanceTypeLimitsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2015-01-01/es/instanceTypeLimits/" Core.<>
                             Core.toText elasticsearchVersion
                             Core.<> "/"
                             Core.<> Core.toText instanceType,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeElasticsearchInstanceTypeLimitsResponse' Core.<$>
                   (x Core..:? "LimitsByRole") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Container for the parameters received from @'DescribeElasticsearchInstanceTypeLimits' @ operation. 
--
-- /See:/ 'mkDescribeElasticsearchInstanceTypeLimitsResponse' smart constructor.
data DescribeElasticsearchInstanceTypeLimitsResponse = DescribeElasticsearchInstanceTypeLimitsResponse'
  { limitsByRole :: Core.Maybe (Core.HashMap Types.InstanceRole Types.Limits)
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeElasticsearchInstanceTypeLimitsResponse' value with any optional fields omitted.
mkDescribeElasticsearchInstanceTypeLimitsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeElasticsearchInstanceTypeLimitsResponse
mkDescribeElasticsearchInstanceTypeLimitsResponse responseStatus
  = DescribeElasticsearchInstanceTypeLimitsResponse'{limitsByRole =
                                                       Core.Nothing,
                                                     responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'limitsByRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deitlrrsLimitsByRole :: Lens.Lens' DescribeElasticsearchInstanceTypeLimitsResponse (Core.Maybe (Core.HashMap Types.InstanceRole Types.Limits))
deitlrrsLimitsByRole = Lens.field @"limitsByRole"
{-# INLINEABLE deitlrrsLimitsByRole #-}
{-# DEPRECATED limitsByRole "Use generic-lens or generic-optics with 'limitsByRole' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deitlrrsResponseStatus :: Lens.Lens' DescribeElasticsearchInstanceTypeLimitsResponse Core.Int
deitlrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE deitlrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
