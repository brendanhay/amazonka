{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.CreateElasticsearchDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Elasticsearch domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains Creating Elasticsearch Domains> in the /Amazon Elasticsearch Service Developer Guide/ .
module Network.AWS.ElasticSearch.CreateElasticsearchDomain
  ( -- * Creating a request
    CreateElasticsearchDomain (..),
    mkCreateElasticsearchDomain,

    -- ** Request lenses
    cedEBSOptions,
    cedNodeToNodeEncryptionOptions,
    cedAccessPolicies,
    cedLogPublishingOptions,
    cedAdvancedSecurityOptions,
    cedElasticsearchClusterConfig,
    cedSnapshotOptions,
    cedDomainName,
    cedCognitoOptions,
    cedEncryptionAtRestOptions,
    cedVPCOptions,
    cedDomainEndpointOptions,
    cedAdvancedOptions,
    cedElasticsearchVersion,

    -- * Destructuring the response
    CreateElasticsearchDomainResponse (..),
    mkCreateElasticsearchDomainResponse,

    -- ** Response lenses
    cedrsDomainStatus,
    cedrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateElasticsearchDomain' smart constructor.
data CreateElasticsearchDomain = CreateElasticsearchDomain'
  { -- | Options to enable, disable and specify the type and size of EBS storage volumes.
    ebsOptions :: Lude.Maybe EBSOptions,
    -- | Specifies the NodeToNodeEncryptionOptions.
    nodeToNodeEncryptionOptions :: Lude.Maybe NodeToNodeEncryptionOptions,
    -- | IAM access policy as a JSON-formatted string.
    accessPolicies :: Lude.Maybe Lude.Text,
    -- | Map of @LogType@ and @LogPublishingOption@ , each containing options to publish a given type of Elasticsearch log.
    logPublishingOptions :: Lude.Maybe (Lude.HashMap LogType (LogPublishingOption)),
    -- | Specifies advanced security options.
    advancedSecurityOptions :: Lude.Maybe AdvancedSecurityOptionsInput,
    -- | Configuration options for an Elasticsearch domain. Specifies the instance type and number of instances in the domain cluster.
    elasticsearchClusterConfig :: Lude.Maybe ElasticsearchClusterConfig,
    -- | Option to set time, in UTC format, of the daily automated snapshot. Default value is 0 hours.
    snapshotOptions :: Lude.Maybe SnapshotOptions,
    -- | The name of the Elasticsearch domain that you are creating. Domain names are unique across the domains owned by an account within an AWS region. Domain names must start with a lowercase letter and can contain the following characters: a-z (lowercase), 0-9, and - (hyphen).
    domainName :: Lude.Text,
    -- | Options to specify the Cognito user and identity pools for Kibana authentication. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
    cognitoOptions :: Lude.Maybe CognitoOptions,
    -- | Specifies the Encryption At Rest Options.
    encryptionAtRestOptions :: Lude.Maybe EncryptionAtRestOptions,
    -- | Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC> in /VPC Endpoints for Amazon Elasticsearch Service Domains/
    vpcOptions :: Lude.Maybe VPCOptions,
    -- | Options to specify configuration that will be applied to the domain endpoint.
    domainEndpointOptions :: Lude.Maybe DomainEndpointOptions,
    -- | Option to allow references to indices in an HTTP request body. Must be @false@ when configuring access to individual sub-resources. By default, the value is @true@ . See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.
    advancedOptions :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | String of format X.Y to specify version for the Elasticsearch domain eg. "1.5" or "2.3". For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains Creating Elasticsearch Domains> in the /Amazon Elasticsearch Service Developer Guide/ .
    elasticsearchVersion :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateElasticsearchDomain' with the minimum fields required to make a request.
--
-- * 'ebsOptions' - Options to enable, disable and specify the type and size of EBS storage volumes.
-- * 'nodeToNodeEncryptionOptions' - Specifies the NodeToNodeEncryptionOptions.
-- * 'accessPolicies' - IAM access policy as a JSON-formatted string.
-- * 'logPublishingOptions' - Map of @LogType@ and @LogPublishingOption@ , each containing options to publish a given type of Elasticsearch log.
-- * 'advancedSecurityOptions' - Specifies advanced security options.
-- * 'elasticsearchClusterConfig' - Configuration options for an Elasticsearch domain. Specifies the instance type and number of instances in the domain cluster.
-- * 'snapshotOptions' - Option to set time, in UTC format, of the daily automated snapshot. Default value is 0 hours.
-- * 'domainName' - The name of the Elasticsearch domain that you are creating. Domain names are unique across the domains owned by an account within an AWS region. Domain names must start with a lowercase letter and can contain the following characters: a-z (lowercase), 0-9, and - (hyphen).
-- * 'cognitoOptions' - Options to specify the Cognito user and identity pools for Kibana authentication. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
-- * 'encryptionAtRestOptions' - Specifies the Encryption At Rest Options.
-- * 'vpcOptions' - Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC> in /VPC Endpoints for Amazon Elasticsearch Service Domains/
-- * 'domainEndpointOptions' - Options to specify configuration that will be applied to the domain endpoint.
-- * 'advancedOptions' - Option to allow references to indices in an HTTP request body. Must be @false@ when configuring access to individual sub-resources. By default, the value is @true@ . See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.
-- * 'elasticsearchVersion' - String of format X.Y to specify version for the Elasticsearch domain eg. "1.5" or "2.3". For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains Creating Elasticsearch Domains> in the /Amazon Elasticsearch Service Developer Guide/ .
mkCreateElasticsearchDomain ::
  -- | 'domainName'
  Lude.Text ->
  CreateElasticsearchDomain
mkCreateElasticsearchDomain pDomainName_ =
  CreateElasticsearchDomain'
    { ebsOptions = Lude.Nothing,
      nodeToNodeEncryptionOptions = Lude.Nothing,
      accessPolicies = Lude.Nothing,
      logPublishingOptions = Lude.Nothing,
      advancedSecurityOptions = Lude.Nothing,
      elasticsearchClusterConfig = Lude.Nothing,
      snapshotOptions = Lude.Nothing,
      domainName = pDomainName_,
      cognitoOptions = Lude.Nothing,
      encryptionAtRestOptions = Lude.Nothing,
      vpcOptions = Lude.Nothing,
      domainEndpointOptions = Lude.Nothing,
      advancedOptions = Lude.Nothing,
      elasticsearchVersion = Lude.Nothing
    }

-- | Options to enable, disable and specify the type and size of EBS storage volumes.
--
-- /Note:/ Consider using 'ebsOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedEBSOptions :: Lens.Lens' CreateElasticsearchDomain (Lude.Maybe EBSOptions)
cedEBSOptions = Lens.lens (ebsOptions :: CreateElasticsearchDomain -> Lude.Maybe EBSOptions) (\s a -> s {ebsOptions = a} :: CreateElasticsearchDomain)
{-# DEPRECATED cedEBSOptions "Use generic-lens or generic-optics with 'ebsOptions' instead." #-}

-- | Specifies the NodeToNodeEncryptionOptions.
--
-- /Note:/ Consider using 'nodeToNodeEncryptionOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedNodeToNodeEncryptionOptions :: Lens.Lens' CreateElasticsearchDomain (Lude.Maybe NodeToNodeEncryptionOptions)
cedNodeToNodeEncryptionOptions = Lens.lens (nodeToNodeEncryptionOptions :: CreateElasticsearchDomain -> Lude.Maybe NodeToNodeEncryptionOptions) (\s a -> s {nodeToNodeEncryptionOptions = a} :: CreateElasticsearchDomain)
{-# DEPRECATED cedNodeToNodeEncryptionOptions "Use generic-lens or generic-optics with 'nodeToNodeEncryptionOptions' instead." #-}

-- | IAM access policy as a JSON-formatted string.
--
-- /Note:/ Consider using 'accessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedAccessPolicies :: Lens.Lens' CreateElasticsearchDomain (Lude.Maybe Lude.Text)
cedAccessPolicies = Lens.lens (accessPolicies :: CreateElasticsearchDomain -> Lude.Maybe Lude.Text) (\s a -> s {accessPolicies = a} :: CreateElasticsearchDomain)
{-# DEPRECATED cedAccessPolicies "Use generic-lens or generic-optics with 'accessPolicies' instead." #-}

-- | Map of @LogType@ and @LogPublishingOption@ , each containing options to publish a given type of Elasticsearch log.
--
-- /Note:/ Consider using 'logPublishingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedLogPublishingOptions :: Lens.Lens' CreateElasticsearchDomain (Lude.Maybe (Lude.HashMap LogType (LogPublishingOption)))
cedLogPublishingOptions = Lens.lens (logPublishingOptions :: CreateElasticsearchDomain -> Lude.Maybe (Lude.HashMap LogType (LogPublishingOption))) (\s a -> s {logPublishingOptions = a} :: CreateElasticsearchDomain)
{-# DEPRECATED cedLogPublishingOptions "Use generic-lens or generic-optics with 'logPublishingOptions' instead." #-}

-- | Specifies advanced security options.
--
-- /Note:/ Consider using 'advancedSecurityOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedAdvancedSecurityOptions :: Lens.Lens' CreateElasticsearchDomain (Lude.Maybe AdvancedSecurityOptionsInput)
cedAdvancedSecurityOptions = Lens.lens (advancedSecurityOptions :: CreateElasticsearchDomain -> Lude.Maybe AdvancedSecurityOptionsInput) (\s a -> s {advancedSecurityOptions = a} :: CreateElasticsearchDomain)
{-# DEPRECATED cedAdvancedSecurityOptions "Use generic-lens or generic-optics with 'advancedSecurityOptions' instead." #-}

-- | Configuration options for an Elasticsearch domain. Specifies the instance type and number of instances in the domain cluster.
--
-- /Note:/ Consider using 'elasticsearchClusterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedElasticsearchClusterConfig :: Lens.Lens' CreateElasticsearchDomain (Lude.Maybe ElasticsearchClusterConfig)
cedElasticsearchClusterConfig = Lens.lens (elasticsearchClusterConfig :: CreateElasticsearchDomain -> Lude.Maybe ElasticsearchClusterConfig) (\s a -> s {elasticsearchClusterConfig = a} :: CreateElasticsearchDomain)
{-# DEPRECATED cedElasticsearchClusterConfig "Use generic-lens or generic-optics with 'elasticsearchClusterConfig' instead." #-}

-- | Option to set time, in UTC format, of the daily automated snapshot. Default value is 0 hours.
--
-- /Note:/ Consider using 'snapshotOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedSnapshotOptions :: Lens.Lens' CreateElasticsearchDomain (Lude.Maybe SnapshotOptions)
cedSnapshotOptions = Lens.lens (snapshotOptions :: CreateElasticsearchDomain -> Lude.Maybe SnapshotOptions) (\s a -> s {snapshotOptions = a} :: CreateElasticsearchDomain)
{-# DEPRECATED cedSnapshotOptions "Use generic-lens or generic-optics with 'snapshotOptions' instead." #-}

-- | The name of the Elasticsearch domain that you are creating. Domain names are unique across the domains owned by an account within an AWS region. Domain names must start with a lowercase letter and can contain the following characters: a-z (lowercase), 0-9, and - (hyphen).
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedDomainName :: Lens.Lens' CreateElasticsearchDomain Lude.Text
cedDomainName = Lens.lens (domainName :: CreateElasticsearchDomain -> Lude.Text) (\s a -> s {domainName = a} :: CreateElasticsearchDomain)
{-# DEPRECATED cedDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Options to specify the Cognito user and identity pools for Kibana authentication. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
--
-- /Note:/ Consider using 'cognitoOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedCognitoOptions :: Lens.Lens' CreateElasticsearchDomain (Lude.Maybe CognitoOptions)
cedCognitoOptions = Lens.lens (cognitoOptions :: CreateElasticsearchDomain -> Lude.Maybe CognitoOptions) (\s a -> s {cognitoOptions = a} :: CreateElasticsearchDomain)
{-# DEPRECATED cedCognitoOptions "Use generic-lens or generic-optics with 'cognitoOptions' instead." #-}

-- | Specifies the Encryption At Rest Options.
--
-- /Note:/ Consider using 'encryptionAtRestOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedEncryptionAtRestOptions :: Lens.Lens' CreateElasticsearchDomain (Lude.Maybe EncryptionAtRestOptions)
cedEncryptionAtRestOptions = Lens.lens (encryptionAtRestOptions :: CreateElasticsearchDomain -> Lude.Maybe EncryptionAtRestOptions) (\s a -> s {encryptionAtRestOptions = a} :: CreateElasticsearchDomain)
{-# DEPRECATED cedEncryptionAtRestOptions "Use generic-lens or generic-optics with 'encryptionAtRestOptions' instead." #-}

-- | Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC> in /VPC Endpoints for Amazon Elasticsearch Service Domains/
--
-- /Note:/ Consider using 'vpcOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedVPCOptions :: Lens.Lens' CreateElasticsearchDomain (Lude.Maybe VPCOptions)
cedVPCOptions = Lens.lens (vpcOptions :: CreateElasticsearchDomain -> Lude.Maybe VPCOptions) (\s a -> s {vpcOptions = a} :: CreateElasticsearchDomain)
{-# DEPRECATED cedVPCOptions "Use generic-lens or generic-optics with 'vpcOptions' instead." #-}

-- | Options to specify configuration that will be applied to the domain endpoint.
--
-- /Note:/ Consider using 'domainEndpointOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedDomainEndpointOptions :: Lens.Lens' CreateElasticsearchDomain (Lude.Maybe DomainEndpointOptions)
cedDomainEndpointOptions = Lens.lens (domainEndpointOptions :: CreateElasticsearchDomain -> Lude.Maybe DomainEndpointOptions) (\s a -> s {domainEndpointOptions = a} :: CreateElasticsearchDomain)
{-# DEPRECATED cedDomainEndpointOptions "Use generic-lens or generic-optics with 'domainEndpointOptions' instead." #-}

-- | Option to allow references to indices in an HTTP request body. Must be @false@ when configuring access to individual sub-resources. By default, the value is @true@ . See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.
--
-- /Note:/ Consider using 'advancedOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedAdvancedOptions :: Lens.Lens' CreateElasticsearchDomain (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cedAdvancedOptions = Lens.lens (advancedOptions :: CreateElasticsearchDomain -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {advancedOptions = a} :: CreateElasticsearchDomain)
{-# DEPRECATED cedAdvancedOptions "Use generic-lens or generic-optics with 'advancedOptions' instead." #-}

-- | String of format X.Y to specify version for the Elasticsearch domain eg. "1.5" or "2.3". For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains Creating Elasticsearch Domains> in the /Amazon Elasticsearch Service Developer Guide/ .
--
-- /Note:/ Consider using 'elasticsearchVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedElasticsearchVersion :: Lens.Lens' CreateElasticsearchDomain (Lude.Maybe Lude.Text)
cedElasticsearchVersion = Lens.lens (elasticsearchVersion :: CreateElasticsearchDomain -> Lude.Maybe Lude.Text) (\s a -> s {elasticsearchVersion = a} :: CreateElasticsearchDomain)
{-# DEPRECATED cedElasticsearchVersion "Use generic-lens or generic-optics with 'elasticsearchVersion' instead." #-}

instance Lude.AWSRequest CreateElasticsearchDomain where
  type
    Rs CreateElasticsearchDomain =
      CreateElasticsearchDomainResponse
  request = Req.postJSON elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateElasticsearchDomainResponse'
            Lude.<$> (x Lude..?> "DomainStatus") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateElasticsearchDomain where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateElasticsearchDomain where
  toJSON CreateElasticsearchDomain' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EBSOptions" Lude..=) Lude.<$> ebsOptions,
            ("NodeToNodeEncryptionOptions" Lude..=)
              Lude.<$> nodeToNodeEncryptionOptions,
            ("AccessPolicies" Lude..=) Lude.<$> accessPolicies,
            ("LogPublishingOptions" Lude..=) Lude.<$> logPublishingOptions,
            ("AdvancedSecurityOptions" Lude..=)
              Lude.<$> advancedSecurityOptions,
            ("ElasticsearchClusterConfig" Lude..=)
              Lude.<$> elasticsearchClusterConfig,
            ("SnapshotOptions" Lude..=) Lude.<$> snapshotOptions,
            Lude.Just ("DomainName" Lude..= domainName),
            ("CognitoOptions" Lude..=) Lude.<$> cognitoOptions,
            ("EncryptionAtRestOptions" Lude..=)
              Lude.<$> encryptionAtRestOptions,
            ("VPCOptions" Lude..=) Lude.<$> vpcOptions,
            ("DomainEndpointOptions" Lude..=) Lude.<$> domainEndpointOptions,
            ("AdvancedOptions" Lude..=) Lude.<$> advancedOptions,
            ("ElasticsearchVersion" Lude..=) Lude.<$> elasticsearchVersion
          ]
      )

instance Lude.ToPath CreateElasticsearchDomain where
  toPath = Lude.const "/2015-01-01/es/domain"

instance Lude.ToQuery CreateElasticsearchDomain where
  toQuery = Lude.const Lude.mempty

-- | The result of a @CreateElasticsearchDomain@ operation. Contains the status of the newly created Elasticsearch domain.
--
-- /See:/ 'mkCreateElasticsearchDomainResponse' smart constructor.
data CreateElasticsearchDomainResponse = CreateElasticsearchDomainResponse'
  { -- | The status of the newly created Elasticsearch domain.
    domainStatus :: Lude.Maybe ElasticsearchDomainStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateElasticsearchDomainResponse' with the minimum fields required to make a request.
--
-- * 'domainStatus' - The status of the newly created Elasticsearch domain.
-- * 'responseStatus' - The response status code.
mkCreateElasticsearchDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateElasticsearchDomainResponse
mkCreateElasticsearchDomainResponse pResponseStatus_ =
  CreateElasticsearchDomainResponse'
    { domainStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the newly created Elasticsearch domain.
--
-- /Note:/ Consider using 'domainStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedrsDomainStatus :: Lens.Lens' CreateElasticsearchDomainResponse (Lude.Maybe ElasticsearchDomainStatus)
cedrsDomainStatus = Lens.lens (domainStatus :: CreateElasticsearchDomainResponse -> Lude.Maybe ElasticsearchDomainStatus) (\s a -> s {domainStatus = a} :: CreateElasticsearchDomainResponse)
{-# DEPRECATED cedrsDomainStatus "Use generic-lens or generic-optics with 'domainStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedrsResponseStatus :: Lens.Lens' CreateElasticsearchDomainResponse Lude.Int
cedrsResponseStatus = Lens.lens (responseStatus :: CreateElasticsearchDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateElasticsearchDomainResponse)
{-# DEPRECATED cedrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
