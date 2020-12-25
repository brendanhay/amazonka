{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateElasticsearchDomainConfig (..),
    mkUpdateElasticsearchDomainConfig,

    -- ** Request lenses
    uedcDomainName,
    uedcAccessPolicies,
    uedcAdvancedOptions,
    uedcAdvancedSecurityOptions,
    uedcCognitoOptions,
    uedcDomainEndpointOptions,
    uedcEBSOptions,
    uedcElasticsearchClusterConfig,
    uedcLogPublishingOptions,
    uedcSnapshotOptions,
    uedcVPCOptions,

    -- * Destructuring the response
    UpdateElasticsearchDomainConfigResponse (..),
    mkUpdateElasticsearchDomainConfigResponse,

    -- ** Response lenses
    uedcrrsDomainConfig,
    uedcrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'UpdateElasticsearchDomain' @ operation. Specifies the type and number of instances in the domain cluster.
--
-- /See:/ 'mkUpdateElasticsearchDomainConfig' smart constructor.
data UpdateElasticsearchDomainConfig = UpdateElasticsearchDomainConfig'
  { -- | The name of the Elasticsearch domain that you are updating.
    domainName :: Types.DomainName,
    -- | IAM access policy as a JSON-formatted string.
    accessPolicies :: Core.Maybe Types.PolicyDocument,
    -- | Modifies the advanced option to allow references to indices in an HTTP request body. Must be @false@ when configuring access to individual sub-resources. By default, the value is @true@ . See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.
    advancedOptions :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | Specifies advanced security options.
    advancedSecurityOptions :: Core.Maybe Types.AdvancedSecurityOptionsInput,
    -- | Options to specify the Cognito user and identity pools for Kibana authentication. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
    cognitoOptions :: Core.Maybe Types.CognitoOptions,
    -- | Options to specify configuration that will be applied to the domain endpoint.
    domainEndpointOptions :: Core.Maybe Types.DomainEndpointOptions,
    -- | Specify the type and size of the EBS volume that you want to use.
    eBSOptions :: Core.Maybe Types.EBSOptions,
    -- | The type and number of instances to instantiate for the domain cluster.
    elasticsearchClusterConfig :: Core.Maybe Types.ElasticsearchClusterConfig,
    -- | Map of @LogType@ and @LogPublishingOption@ , each containing options to publish a given type of Elasticsearch log.
    logPublishingOptions :: Core.Maybe (Core.HashMap Types.LogType Types.LogPublishingOption),
    -- | Option to set the time, in UTC format, for the daily automated snapshot. Default value is @0@ hours.
    snapshotOptions :: Core.Maybe Types.SnapshotOptions,
    -- | Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC> in /VPC Endpoints for Amazon Elasticsearch Service Domains/
    vPCOptions :: Core.Maybe Types.VPCOptions
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateElasticsearchDomainConfig' value with any optional fields omitted.
mkUpdateElasticsearchDomainConfig ::
  -- | 'domainName'
  Types.DomainName ->
  UpdateElasticsearchDomainConfig
mkUpdateElasticsearchDomainConfig domainName =
  UpdateElasticsearchDomainConfig'
    { domainName,
      accessPolicies = Core.Nothing,
      advancedOptions = Core.Nothing,
      advancedSecurityOptions = Core.Nothing,
      cognitoOptions = Core.Nothing,
      domainEndpointOptions = Core.Nothing,
      eBSOptions = Core.Nothing,
      elasticsearchClusterConfig = Core.Nothing,
      logPublishingOptions = Core.Nothing,
      snapshotOptions = Core.Nothing,
      vPCOptions = Core.Nothing
    }

-- | The name of the Elasticsearch domain that you are updating.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcDomainName :: Lens.Lens' UpdateElasticsearchDomainConfig Types.DomainName
uedcDomainName = Lens.field @"domainName"
{-# DEPRECATED uedcDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | IAM access policy as a JSON-formatted string.
--
-- /Note:/ Consider using 'accessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcAccessPolicies :: Lens.Lens' UpdateElasticsearchDomainConfig (Core.Maybe Types.PolicyDocument)
uedcAccessPolicies = Lens.field @"accessPolicies"
{-# DEPRECATED uedcAccessPolicies "Use generic-lens or generic-optics with 'accessPolicies' instead." #-}

-- | Modifies the advanced option to allow references to indices in an HTTP request body. Must be @false@ when configuring access to individual sub-resources. By default, the value is @true@ . See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.
--
-- /Note:/ Consider using 'advancedOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcAdvancedOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Core.Maybe (Core.HashMap Types.String Types.String))
uedcAdvancedOptions = Lens.field @"advancedOptions"
{-# DEPRECATED uedcAdvancedOptions "Use generic-lens or generic-optics with 'advancedOptions' instead." #-}

-- | Specifies advanced security options.
--
-- /Note:/ Consider using 'advancedSecurityOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcAdvancedSecurityOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Core.Maybe Types.AdvancedSecurityOptionsInput)
uedcAdvancedSecurityOptions = Lens.field @"advancedSecurityOptions"
{-# DEPRECATED uedcAdvancedSecurityOptions "Use generic-lens or generic-optics with 'advancedSecurityOptions' instead." #-}

-- | Options to specify the Cognito user and identity pools for Kibana authentication. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
--
-- /Note:/ Consider using 'cognitoOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcCognitoOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Core.Maybe Types.CognitoOptions)
uedcCognitoOptions = Lens.field @"cognitoOptions"
{-# DEPRECATED uedcCognitoOptions "Use generic-lens or generic-optics with 'cognitoOptions' instead." #-}

-- | Options to specify configuration that will be applied to the domain endpoint.
--
-- /Note:/ Consider using 'domainEndpointOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcDomainEndpointOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Core.Maybe Types.DomainEndpointOptions)
uedcDomainEndpointOptions = Lens.field @"domainEndpointOptions"
{-# DEPRECATED uedcDomainEndpointOptions "Use generic-lens or generic-optics with 'domainEndpointOptions' instead." #-}

-- | Specify the type and size of the EBS volume that you want to use.
--
-- /Note:/ Consider using 'eBSOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcEBSOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Core.Maybe Types.EBSOptions)
uedcEBSOptions = Lens.field @"eBSOptions"
{-# DEPRECATED uedcEBSOptions "Use generic-lens or generic-optics with 'eBSOptions' instead." #-}

-- | The type and number of instances to instantiate for the domain cluster.
--
-- /Note:/ Consider using 'elasticsearchClusterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcElasticsearchClusterConfig :: Lens.Lens' UpdateElasticsearchDomainConfig (Core.Maybe Types.ElasticsearchClusterConfig)
uedcElasticsearchClusterConfig = Lens.field @"elasticsearchClusterConfig"
{-# DEPRECATED uedcElasticsearchClusterConfig "Use generic-lens or generic-optics with 'elasticsearchClusterConfig' instead." #-}

-- | Map of @LogType@ and @LogPublishingOption@ , each containing options to publish a given type of Elasticsearch log.
--
-- /Note:/ Consider using 'logPublishingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcLogPublishingOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Core.Maybe (Core.HashMap Types.LogType Types.LogPublishingOption))
uedcLogPublishingOptions = Lens.field @"logPublishingOptions"
{-# DEPRECATED uedcLogPublishingOptions "Use generic-lens or generic-optics with 'logPublishingOptions' instead." #-}

-- | Option to set the time, in UTC format, for the daily automated snapshot. Default value is @0@ hours.
--
-- /Note:/ Consider using 'snapshotOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcSnapshotOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Core.Maybe Types.SnapshotOptions)
uedcSnapshotOptions = Lens.field @"snapshotOptions"
{-# DEPRECATED uedcSnapshotOptions "Use generic-lens or generic-optics with 'snapshotOptions' instead." #-}

-- | Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC> in /VPC Endpoints for Amazon Elasticsearch Service Domains/
--
-- /Note:/ Consider using 'vPCOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcVPCOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Core.Maybe Types.VPCOptions)
uedcVPCOptions = Lens.field @"vPCOptions"
{-# DEPRECATED uedcVPCOptions "Use generic-lens or generic-optics with 'vPCOptions' instead." #-}

instance Core.FromJSON UpdateElasticsearchDomainConfig where
  toJSON UpdateElasticsearchDomainConfig {..} =
    Core.object
      ( Core.catMaybes
          [ ("AccessPolicies" Core..=) Core.<$> accessPolicies,
            ("AdvancedOptions" Core..=) Core.<$> advancedOptions,
            ("AdvancedSecurityOptions" Core..=)
              Core.<$> advancedSecurityOptions,
            ("CognitoOptions" Core..=) Core.<$> cognitoOptions,
            ("DomainEndpointOptions" Core..=) Core.<$> domainEndpointOptions,
            ("EBSOptions" Core..=) Core.<$> eBSOptions,
            ("ElasticsearchClusterConfig" Core..=)
              Core.<$> elasticsearchClusterConfig,
            ("LogPublishingOptions" Core..=) Core.<$> logPublishingOptions,
            ("SnapshotOptions" Core..=) Core.<$> snapshotOptions,
            ("VPCOptions" Core..=) Core.<$> vPCOptions
          ]
      )

instance Core.AWSRequest UpdateElasticsearchDomainConfig where
  type
    Rs UpdateElasticsearchDomainConfig =
      UpdateElasticsearchDomainConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/2015-01-01/es/domain/" Core.<> (Core.toText domainName)
                Core.<> ("/config")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateElasticsearchDomainConfigResponse'
            Core.<$> (x Core..: "DomainConfig") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of an @UpdateElasticsearchDomain@ request. Contains the status of the Elasticsearch domain being updated.
--
-- /See:/ 'mkUpdateElasticsearchDomainConfigResponse' smart constructor.
data UpdateElasticsearchDomainConfigResponse = UpdateElasticsearchDomainConfigResponse'
  { -- | The status of the updated Elasticsearch domain.
    domainConfig :: Types.ElasticsearchDomainConfig,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateElasticsearchDomainConfigResponse' value with any optional fields omitted.
mkUpdateElasticsearchDomainConfigResponse ::
  -- | 'domainConfig'
  Types.ElasticsearchDomainConfig ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateElasticsearchDomainConfigResponse
mkUpdateElasticsearchDomainConfigResponse
  domainConfig
  responseStatus =
    UpdateElasticsearchDomainConfigResponse'
      { domainConfig,
        responseStatus
      }

-- | The status of the updated Elasticsearch domain.
--
-- /Note:/ Consider using 'domainConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcrrsDomainConfig :: Lens.Lens' UpdateElasticsearchDomainConfigResponse Types.ElasticsearchDomainConfig
uedcrrsDomainConfig = Lens.field @"domainConfig"
{-# DEPRECATED uedcrrsDomainConfig "Use generic-lens or generic-optics with 'domainConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcrrsResponseStatus :: Lens.Lens' UpdateElasticsearchDomainConfigResponse Core.Int
uedcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uedcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
