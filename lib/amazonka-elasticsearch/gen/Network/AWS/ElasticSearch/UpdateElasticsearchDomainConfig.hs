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
    uedcEBSOptions,
    uedcAccessPolicies,
    uedcLogPublishingOptions,
    uedcAdvancedSecurityOptions,
    uedcElasticsearchClusterConfig,
    uedcSnapshotOptions,
    uedcDomainName,
    uedcCognitoOptions,
    uedcVPCOptions,
    uedcDomainEndpointOptions,
    uedcAdvancedOptions,

    -- * Destructuring the response
    UpdateElasticsearchDomainConfigResponse (..),
    mkUpdateElasticsearchDomainConfigResponse,

    -- ** Response lenses
    uedcrsDomainConfig,
    uedcrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'UpdateElasticsearchDomain' @ operation. Specifies the type and number of instances in the domain cluster.
--
-- /See:/ 'mkUpdateElasticsearchDomainConfig' smart constructor.
data UpdateElasticsearchDomainConfig = UpdateElasticsearchDomainConfig'
  { -- | Specify the type and size of the EBS volume that you want to use.
    ebsOptions :: Lude.Maybe EBSOptions,
    -- | IAM access policy as a JSON-formatted string.
    accessPolicies :: Lude.Maybe Lude.Text,
    -- | Map of @LogType@ and @LogPublishingOption@ , each containing options to publish a given type of Elasticsearch log.
    logPublishingOptions :: Lude.Maybe (Lude.HashMap LogType (LogPublishingOption)),
    -- | Specifies advanced security options.
    advancedSecurityOptions :: Lude.Maybe AdvancedSecurityOptionsInput,
    -- | The type and number of instances to instantiate for the domain cluster.
    elasticsearchClusterConfig :: Lude.Maybe ElasticsearchClusterConfig,
    -- | Option to set the time, in UTC format, for the daily automated snapshot. Default value is @0@ hours.
    snapshotOptions :: Lude.Maybe SnapshotOptions,
    -- | The name of the Elasticsearch domain that you are updating.
    domainName :: Lude.Text,
    -- | Options to specify the Cognito user and identity pools for Kibana authentication. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
    cognitoOptions :: Lude.Maybe CognitoOptions,
    -- | Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC> in /VPC Endpoints for Amazon Elasticsearch Service Domains/
    vpcOptions :: Lude.Maybe VPCOptions,
    -- | Options to specify configuration that will be applied to the domain endpoint.
    domainEndpointOptions :: Lude.Maybe DomainEndpointOptions,
    -- | Modifies the advanced option to allow references to indices in an HTTP request body. Must be @false@ when configuring access to individual sub-resources. By default, the value is @true@ . See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.
    advancedOptions :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateElasticsearchDomainConfig' with the minimum fields required to make a request.
--
-- * 'ebsOptions' - Specify the type and size of the EBS volume that you want to use.
-- * 'accessPolicies' - IAM access policy as a JSON-formatted string.
-- * 'logPublishingOptions' - Map of @LogType@ and @LogPublishingOption@ , each containing options to publish a given type of Elasticsearch log.
-- * 'advancedSecurityOptions' - Specifies advanced security options.
-- * 'elasticsearchClusterConfig' - The type and number of instances to instantiate for the domain cluster.
-- * 'snapshotOptions' - Option to set the time, in UTC format, for the daily automated snapshot. Default value is @0@ hours.
-- * 'domainName' - The name of the Elasticsearch domain that you are updating.
-- * 'cognitoOptions' - Options to specify the Cognito user and identity pools for Kibana authentication. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
-- * 'vpcOptions' - Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC> in /VPC Endpoints for Amazon Elasticsearch Service Domains/
-- * 'domainEndpointOptions' - Options to specify configuration that will be applied to the domain endpoint.
-- * 'advancedOptions' - Modifies the advanced option to allow references to indices in an HTTP request body. Must be @false@ when configuring access to individual sub-resources. By default, the value is @true@ . See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.
mkUpdateElasticsearchDomainConfig ::
  -- | 'domainName'
  Lude.Text ->
  UpdateElasticsearchDomainConfig
mkUpdateElasticsearchDomainConfig pDomainName_ =
  UpdateElasticsearchDomainConfig'
    { ebsOptions = Lude.Nothing,
      accessPolicies = Lude.Nothing,
      logPublishingOptions = Lude.Nothing,
      advancedSecurityOptions = Lude.Nothing,
      elasticsearchClusterConfig = Lude.Nothing,
      snapshotOptions = Lude.Nothing,
      domainName = pDomainName_,
      cognitoOptions = Lude.Nothing,
      vpcOptions = Lude.Nothing,
      domainEndpointOptions = Lude.Nothing,
      advancedOptions = Lude.Nothing
    }

-- | Specify the type and size of the EBS volume that you want to use.
--
-- /Note:/ Consider using 'ebsOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcEBSOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Lude.Maybe EBSOptions)
uedcEBSOptions = Lens.lens (ebsOptions :: UpdateElasticsearchDomainConfig -> Lude.Maybe EBSOptions) (\s a -> s {ebsOptions = a} :: UpdateElasticsearchDomainConfig)
{-# DEPRECATED uedcEBSOptions "Use generic-lens or generic-optics with 'ebsOptions' instead." #-}

-- | IAM access policy as a JSON-formatted string.
--
-- /Note:/ Consider using 'accessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcAccessPolicies :: Lens.Lens' UpdateElasticsearchDomainConfig (Lude.Maybe Lude.Text)
uedcAccessPolicies = Lens.lens (accessPolicies :: UpdateElasticsearchDomainConfig -> Lude.Maybe Lude.Text) (\s a -> s {accessPolicies = a} :: UpdateElasticsearchDomainConfig)
{-# DEPRECATED uedcAccessPolicies "Use generic-lens or generic-optics with 'accessPolicies' instead." #-}

-- | Map of @LogType@ and @LogPublishingOption@ , each containing options to publish a given type of Elasticsearch log.
--
-- /Note:/ Consider using 'logPublishingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcLogPublishingOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Lude.Maybe (Lude.HashMap LogType (LogPublishingOption)))
uedcLogPublishingOptions = Lens.lens (logPublishingOptions :: UpdateElasticsearchDomainConfig -> Lude.Maybe (Lude.HashMap LogType (LogPublishingOption))) (\s a -> s {logPublishingOptions = a} :: UpdateElasticsearchDomainConfig)
{-# DEPRECATED uedcLogPublishingOptions "Use generic-lens or generic-optics with 'logPublishingOptions' instead." #-}

-- | Specifies advanced security options.
--
-- /Note:/ Consider using 'advancedSecurityOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcAdvancedSecurityOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Lude.Maybe AdvancedSecurityOptionsInput)
uedcAdvancedSecurityOptions = Lens.lens (advancedSecurityOptions :: UpdateElasticsearchDomainConfig -> Lude.Maybe AdvancedSecurityOptionsInput) (\s a -> s {advancedSecurityOptions = a} :: UpdateElasticsearchDomainConfig)
{-# DEPRECATED uedcAdvancedSecurityOptions "Use generic-lens or generic-optics with 'advancedSecurityOptions' instead." #-}

-- | The type and number of instances to instantiate for the domain cluster.
--
-- /Note:/ Consider using 'elasticsearchClusterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcElasticsearchClusterConfig :: Lens.Lens' UpdateElasticsearchDomainConfig (Lude.Maybe ElasticsearchClusterConfig)
uedcElasticsearchClusterConfig = Lens.lens (elasticsearchClusterConfig :: UpdateElasticsearchDomainConfig -> Lude.Maybe ElasticsearchClusterConfig) (\s a -> s {elasticsearchClusterConfig = a} :: UpdateElasticsearchDomainConfig)
{-# DEPRECATED uedcElasticsearchClusterConfig "Use generic-lens or generic-optics with 'elasticsearchClusterConfig' instead." #-}

-- | Option to set the time, in UTC format, for the daily automated snapshot. Default value is @0@ hours.
--
-- /Note:/ Consider using 'snapshotOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcSnapshotOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Lude.Maybe SnapshotOptions)
uedcSnapshotOptions = Lens.lens (snapshotOptions :: UpdateElasticsearchDomainConfig -> Lude.Maybe SnapshotOptions) (\s a -> s {snapshotOptions = a} :: UpdateElasticsearchDomainConfig)
{-# DEPRECATED uedcSnapshotOptions "Use generic-lens or generic-optics with 'snapshotOptions' instead." #-}

-- | The name of the Elasticsearch domain that you are updating.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcDomainName :: Lens.Lens' UpdateElasticsearchDomainConfig Lude.Text
uedcDomainName = Lens.lens (domainName :: UpdateElasticsearchDomainConfig -> Lude.Text) (\s a -> s {domainName = a} :: UpdateElasticsearchDomainConfig)
{-# DEPRECATED uedcDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Options to specify the Cognito user and identity pools for Kibana authentication. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
--
-- /Note:/ Consider using 'cognitoOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcCognitoOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Lude.Maybe CognitoOptions)
uedcCognitoOptions = Lens.lens (cognitoOptions :: UpdateElasticsearchDomainConfig -> Lude.Maybe CognitoOptions) (\s a -> s {cognitoOptions = a} :: UpdateElasticsearchDomainConfig)
{-# DEPRECATED uedcCognitoOptions "Use generic-lens or generic-optics with 'cognitoOptions' instead." #-}

-- | Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC> in /VPC Endpoints for Amazon Elasticsearch Service Domains/
--
-- /Note:/ Consider using 'vpcOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcVPCOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Lude.Maybe VPCOptions)
uedcVPCOptions = Lens.lens (vpcOptions :: UpdateElasticsearchDomainConfig -> Lude.Maybe VPCOptions) (\s a -> s {vpcOptions = a} :: UpdateElasticsearchDomainConfig)
{-# DEPRECATED uedcVPCOptions "Use generic-lens or generic-optics with 'vpcOptions' instead." #-}

-- | Options to specify configuration that will be applied to the domain endpoint.
--
-- /Note:/ Consider using 'domainEndpointOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcDomainEndpointOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Lude.Maybe DomainEndpointOptions)
uedcDomainEndpointOptions = Lens.lens (domainEndpointOptions :: UpdateElasticsearchDomainConfig -> Lude.Maybe DomainEndpointOptions) (\s a -> s {domainEndpointOptions = a} :: UpdateElasticsearchDomainConfig)
{-# DEPRECATED uedcDomainEndpointOptions "Use generic-lens or generic-optics with 'domainEndpointOptions' instead." #-}

-- | Modifies the advanced option to allow references to indices in an HTTP request body. Must be @false@ when configuring access to individual sub-resources. By default, the value is @true@ . See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.
--
-- /Note:/ Consider using 'advancedOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcAdvancedOptions :: Lens.Lens' UpdateElasticsearchDomainConfig (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
uedcAdvancedOptions = Lens.lens (advancedOptions :: UpdateElasticsearchDomainConfig -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {advancedOptions = a} :: UpdateElasticsearchDomainConfig)
{-# DEPRECATED uedcAdvancedOptions "Use generic-lens or generic-optics with 'advancedOptions' instead." #-}

instance Lude.AWSRequest UpdateElasticsearchDomainConfig where
  type
    Rs UpdateElasticsearchDomainConfig =
      UpdateElasticsearchDomainConfigResponse
  request = Req.postJSON elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateElasticsearchDomainConfigResponse'
            Lude.<$> (x Lude..:> "DomainConfig") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateElasticsearchDomainConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateElasticsearchDomainConfig where
  toJSON UpdateElasticsearchDomainConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EBSOptions" Lude..=) Lude.<$> ebsOptions,
            ("AccessPolicies" Lude..=) Lude.<$> accessPolicies,
            ("LogPublishingOptions" Lude..=) Lude.<$> logPublishingOptions,
            ("AdvancedSecurityOptions" Lude..=)
              Lude.<$> advancedSecurityOptions,
            ("ElasticsearchClusterConfig" Lude..=)
              Lude.<$> elasticsearchClusterConfig,
            ("SnapshotOptions" Lude..=) Lude.<$> snapshotOptions,
            ("CognitoOptions" Lude..=) Lude.<$> cognitoOptions,
            ("VPCOptions" Lude..=) Lude.<$> vpcOptions,
            ("DomainEndpointOptions" Lude..=) Lude.<$> domainEndpointOptions,
            ("AdvancedOptions" Lude..=) Lude.<$> advancedOptions
          ]
      )

instance Lude.ToPath UpdateElasticsearchDomainConfig where
  toPath UpdateElasticsearchDomainConfig' {..} =
    Lude.mconcat
      ["/2015-01-01/es/domain/", Lude.toBS domainName, "/config"]

instance Lude.ToQuery UpdateElasticsearchDomainConfig where
  toQuery = Lude.const Lude.mempty

-- | The result of an @UpdateElasticsearchDomain@ request. Contains the status of the Elasticsearch domain being updated.
--
-- /See:/ 'mkUpdateElasticsearchDomainConfigResponse' smart constructor.
data UpdateElasticsearchDomainConfigResponse = UpdateElasticsearchDomainConfigResponse'
  { -- | The status of the updated Elasticsearch domain.
    domainConfig :: ElasticsearchDomainConfig,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateElasticsearchDomainConfigResponse' with the minimum fields required to make a request.
--
-- * 'domainConfig' - The status of the updated Elasticsearch domain.
-- * 'responseStatus' - The response status code.
mkUpdateElasticsearchDomainConfigResponse ::
  -- | 'domainConfig'
  ElasticsearchDomainConfig ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdateElasticsearchDomainConfigResponse
mkUpdateElasticsearchDomainConfigResponse
  pDomainConfig_
  pResponseStatus_ =
    UpdateElasticsearchDomainConfigResponse'
      { domainConfig =
          pDomainConfig_,
        responseStatus = pResponseStatus_
      }

-- | The status of the updated Elasticsearch domain.
--
-- /Note:/ Consider using 'domainConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcrsDomainConfig :: Lens.Lens' UpdateElasticsearchDomainConfigResponse ElasticsearchDomainConfig
uedcrsDomainConfig = Lens.lens (domainConfig :: UpdateElasticsearchDomainConfigResponse -> ElasticsearchDomainConfig) (\s a -> s {domainConfig = a} :: UpdateElasticsearchDomainConfigResponse)
{-# DEPRECATED uedcrsDomainConfig "Use generic-lens or generic-optics with 'domainConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedcrsResponseStatus :: Lens.Lens' UpdateElasticsearchDomainConfigResponse Lude.Int
uedcrsResponseStatus = Lens.lens (responseStatus :: UpdateElasticsearchDomainConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateElasticsearchDomainConfigResponse)
{-# DEPRECATED uedcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
