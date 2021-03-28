{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.UpdateElasticsearchDomainConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the cluster configuration of the specified Elasticsearch domain, setting as setting the instance type and the number of instances. 
module Network.AWS.ElasticSearch.UpdateElasticsearchDomainConfig
    (
    -- * Creating a request
      UpdateElasticsearchDomainConfig (..)
    , mkUpdateElasticsearchDomainConfig
    -- ** Request lenses
    , uedcDomainName
    , uedcAccessPolicies
    , uedcAdvancedOptions
    , uedcAdvancedSecurityOptions
    , uedcCognitoOptions
    , uedcDomainEndpointOptions
    , uedcEBSOptions
    , uedcElasticsearchClusterConfig
    , uedcLogPublishingOptions
    , uedcSnapshotOptions
    , uedcVPCOptions

    -- * Destructuring the response
    , UpdateElasticsearchDomainConfigResponse (..)
    , mkUpdateElasticsearchDomainConfigResponse
    -- ** Response lenses
    , uedcrrsDomainConfig
    , uedcrrsResponseStatus
    ) where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'UpdateElasticsearchDomain' @ operation. Specifies the type and number of instances in the domain cluster.
--
-- /See:/ 'mkUpdateElasticsearchDomainConfig' smart constructor.
data UpdateElasticsearchDomainConfig = UpdateElasticsearchDomainConfig'
  { domainName :: Types.DomainName
    -- ^ The name of the Elasticsearch domain that you are updating. 
  , accessPolicies :: Core.Maybe Types.PolicyDocument
    -- ^ IAM access policy as a JSON-formatted string.
  , advancedOptions :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Modifies the advanced option to allow references to indices in an HTTP request body. Must be @false@ when configuring access to individual sub-resources. By default, the value is @true@ . See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.
  , advancedSecurityOptions :: Core.Maybe Types.AdvancedSecurityOptionsInput
    -- ^ Specifies advanced security options.
  , cognitoOptions :: Core.Maybe Types.CognitoOptions
    -- ^ Options to specify the Cognito user and identity pools for Kibana authentication. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
  , domainEndpointOptions :: Core.Maybe Types.DomainEndpointOptions
    -- ^ Options to specify configuration that will be applied to the domain endpoint.
  , eBSOptions :: Core.Maybe Types.EBSOptions
    -- ^ Specify the type and size of the EBS volume that you want to use. 
  , elasticsearchClusterConfig :: Core.Maybe Types.ElasticsearchClusterConfig
    -- ^ The type and number of instances to instantiate for the domain cluster.
  , logPublishingOptions :: Core.Maybe (Core.HashMap Types.LogType Types.LogPublishingOption)
    -- ^ Map of @LogType@ and @LogPublishingOption@ , each containing options to publish a given type of Elasticsearch log.
  , snapshotOptions :: Core.Maybe Types.SnapshotOptions
    -- ^ Option to set the time, in UTC format, for the daily automated snapshot. Default value is @0@ hours. 
  , vPCOptions :: Core.Maybe Types.VPCOptions
    -- ^ Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC> in /VPC Endpoints for Amazon Elasticsearch Service Domains/ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateElasticsearchDomainConfig' value with any optional fields omitted.
mkUpdateElasticsearchDomainConfig
    :: Types.DomainName -- ^ 'domainName'
    -> UpdateElasticsearchDomainConfig
mkUpdateElasticsearchDomainConfig domainName
  = UpdateElasticsearchDomainConfig'{domainName,
                                     accessPolicies = Core.Nothing, advancedOptions = Core.Nothing,
                                     advancedSecurityOptions = Core.Nothing,
                                     cognitoOptions = Core.Nothing,
                                     domainEndpointOptions = Core.Nothing,
                                     eBSOptions = Core.Nothing,
                                     elasticsearchClusterConfig = Core.Nothing,
                                     logPublishingOptions = Core.Nothing,
                                     snapshotOptions = Core.Nothing, vPCOptions = Core.Nothing}

-- | The name of the Elasticsearch domain that you are updating. 
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcDomainName :: Lens.Lens' UpdateElasticsearchDomainConfig Types.DomainName
uedcDomainName = Lens.field @"domainName"
{-# INLINEABLE uedcDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | IAM access policy as a JSON-formatted string.
--
-- /Note:/ Consider using 'accessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcAccessPolicies :: Lens.Lens' UpdateElasticsearchDomainConfig (Core.Maybe Types.PolicyDocument)
uedcAccessPolicies = Lens.field @"accessPolicies"
{-# INLINEABLE uedcAccessPolicies #-}
{-# DEPRECATED accessPolicies "Use generic-lens or generic-optics with 'accessPolicies' instead"  #-}

-- | Modifies the advanced option to allow references to indices in an HTTP request body. Must be @false@ when configuring access to individual sub-resources. By default, the value is @true@ . See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.
--
-- /Note:/ Consider using 'advancedOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcAdvancedOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Core.Maybe (Core.HashMap Core.Text Core.Text))
uedcAdvancedOptions = Lens.field @"advancedOptions"
{-# INLINEABLE uedcAdvancedOptions #-}
{-# DEPRECATED advancedOptions "Use generic-lens or generic-optics with 'advancedOptions' instead"  #-}

-- | Specifies advanced security options.
--
-- /Note:/ Consider using 'advancedSecurityOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcAdvancedSecurityOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Core.Maybe Types.AdvancedSecurityOptionsInput)
uedcAdvancedSecurityOptions = Lens.field @"advancedSecurityOptions"
{-# INLINEABLE uedcAdvancedSecurityOptions #-}
{-# DEPRECATED advancedSecurityOptions "Use generic-lens or generic-optics with 'advancedSecurityOptions' instead"  #-}

-- | Options to specify the Cognito user and identity pools for Kibana authentication. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
--
-- /Note:/ Consider using 'cognitoOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcCognitoOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Core.Maybe Types.CognitoOptions)
uedcCognitoOptions = Lens.field @"cognitoOptions"
{-# INLINEABLE uedcCognitoOptions #-}
{-# DEPRECATED cognitoOptions "Use generic-lens or generic-optics with 'cognitoOptions' instead"  #-}

-- | Options to specify configuration that will be applied to the domain endpoint.
--
-- /Note:/ Consider using 'domainEndpointOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcDomainEndpointOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Core.Maybe Types.DomainEndpointOptions)
uedcDomainEndpointOptions = Lens.field @"domainEndpointOptions"
{-# INLINEABLE uedcDomainEndpointOptions #-}
{-# DEPRECATED domainEndpointOptions "Use generic-lens or generic-optics with 'domainEndpointOptions' instead"  #-}

-- | Specify the type and size of the EBS volume that you want to use. 
--
-- /Note:/ Consider using 'eBSOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcEBSOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Core.Maybe Types.EBSOptions)
uedcEBSOptions = Lens.field @"eBSOptions"
{-# INLINEABLE uedcEBSOptions #-}
{-# DEPRECATED eBSOptions "Use generic-lens or generic-optics with 'eBSOptions' instead"  #-}

-- | The type and number of instances to instantiate for the domain cluster.
--
-- /Note:/ Consider using 'elasticsearchClusterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcElasticsearchClusterConfig :: Lens.Lens' UpdateElasticsearchDomainConfig (Core.Maybe Types.ElasticsearchClusterConfig)
uedcElasticsearchClusterConfig = Lens.field @"elasticsearchClusterConfig"
{-# INLINEABLE uedcElasticsearchClusterConfig #-}
{-# DEPRECATED elasticsearchClusterConfig "Use generic-lens or generic-optics with 'elasticsearchClusterConfig' instead"  #-}

-- | Map of @LogType@ and @LogPublishingOption@ , each containing options to publish a given type of Elasticsearch log.
--
-- /Note:/ Consider using 'logPublishingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcLogPublishingOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Core.Maybe (Core.HashMap Types.LogType Types.LogPublishingOption))
uedcLogPublishingOptions = Lens.field @"logPublishingOptions"
{-# INLINEABLE uedcLogPublishingOptions #-}
{-# DEPRECATED logPublishingOptions "Use generic-lens or generic-optics with 'logPublishingOptions' instead"  #-}

-- | Option to set the time, in UTC format, for the daily automated snapshot. Default value is @0@ hours. 
--
-- /Note:/ Consider using 'snapshotOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcSnapshotOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Core.Maybe Types.SnapshotOptions)
uedcSnapshotOptions = Lens.field @"snapshotOptions"
{-# INLINEABLE uedcSnapshotOptions #-}
{-# DEPRECATED snapshotOptions "Use generic-lens or generic-optics with 'snapshotOptions' instead"  #-}

-- | Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC> in /VPC Endpoints for Amazon Elasticsearch Service Domains/ 
--
-- /Note:/ Consider using 'vPCOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcVPCOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Core.Maybe Types.VPCOptions)
uedcVPCOptions = Lens.field @"vPCOptions"
{-# INLINEABLE uedcVPCOptions #-}
{-# DEPRECATED vPCOptions "Use generic-lens or generic-optics with 'vPCOptions' instead"  #-}

instance Core.ToQuery UpdateElasticsearchDomainConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateElasticsearchDomainConfig where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON UpdateElasticsearchDomainConfig where
        toJSON UpdateElasticsearchDomainConfig{..}
          = Core.object
              (Core.catMaybes
                 [("AccessPolicies" Core..=) Core.<$> accessPolicies,
                  ("AdvancedOptions" Core..=) Core.<$> advancedOptions,
                  ("AdvancedSecurityOptions" Core..=) Core.<$>
                    advancedSecurityOptions,
                  ("CognitoOptions" Core..=) Core.<$> cognitoOptions,
                  ("DomainEndpointOptions" Core..=) Core.<$> domainEndpointOptions,
                  ("EBSOptions" Core..=) Core.<$> eBSOptions,
                  ("ElasticsearchClusterConfig" Core..=) Core.<$>
                    elasticsearchClusterConfig,
                  ("LogPublishingOptions" Core..=) Core.<$> logPublishingOptions,
                  ("SnapshotOptions" Core..=) Core.<$> snapshotOptions,
                  ("VPCOptions" Core..=) Core.<$> vPCOptions])

instance Core.AWSRequest UpdateElasticsearchDomainConfig where
        type Rs UpdateElasticsearchDomainConfig =
             UpdateElasticsearchDomainConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/2015-01-01/es/domain/" Core.<> Core.toText domainName Core.<>
                             "/config",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateElasticsearchDomainConfigResponse' Core.<$>
                   (x Core..: "DomainConfig") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of an @UpdateElasticsearchDomain@ request. Contains the status of the Elasticsearch domain being updated.
--
-- /See:/ 'mkUpdateElasticsearchDomainConfigResponse' smart constructor.
data UpdateElasticsearchDomainConfigResponse = UpdateElasticsearchDomainConfigResponse'
  { domainConfig :: Types.ElasticsearchDomainConfig
    -- ^ The status of the updated Elasticsearch domain. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateElasticsearchDomainConfigResponse' value with any optional fields omitted.
mkUpdateElasticsearchDomainConfigResponse
    :: Types.ElasticsearchDomainConfig -- ^ 'domainConfig'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateElasticsearchDomainConfigResponse
mkUpdateElasticsearchDomainConfigResponse domainConfig
  responseStatus
  = UpdateElasticsearchDomainConfigResponse'{domainConfig,
                                             responseStatus}

-- | The status of the updated Elasticsearch domain. 
--
-- /Note:/ Consider using 'domainConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcrrsDomainConfig :: Lens.Lens' UpdateElasticsearchDomainConfigResponse Types.ElasticsearchDomainConfig
uedcrrsDomainConfig = Lens.field @"domainConfig"
{-# INLINEABLE uedcrrsDomainConfig #-}
{-# DEPRECATED domainConfig "Use generic-lens or generic-optics with 'domainConfig' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcrrsResponseStatus :: Lens.Lens' UpdateElasticsearchDomainConfigResponse Core.Int
uedcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uedcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
