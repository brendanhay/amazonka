-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ElasticsearchDomainStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ElasticsearchDomainStatus
  ( ElasticsearchDomainStatus (..),

    -- * Smart constructor
    mkElasticsearchDomainStatus,

    -- * Lenses
    edsEBSOptions,
    edsNodeToNodeEncryptionOptions,
    edsAccessPolicies,
    edsServiceSoftwareOptions,
    edsLogPublishingOptions,
    edsAdvancedSecurityOptions,
    edsCreated,
    edsSnapshotOptions,
    edsCognitoOptions,
    edsEncryptionAtRestOptions,
    edsDeleted,
    edsVPCOptions,
    edsEndpoints,
    edsDomainEndpointOptions,
    edsProcessing,
    edsEndpoint,
    edsUpgradeProcessing,
    edsAdvancedOptions,
    edsElasticsearchVersion,
    edsDomainId,
    edsDomainName,
    edsARN,
    edsElasticsearchClusterConfig,
  )
where

import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptions
import Network.AWS.ElasticSearch.Types.CognitoOptions
import Network.AWS.ElasticSearch.Types.DomainEndpointOptions
import Network.AWS.ElasticSearch.Types.EBSOptions
import Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfig
import Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions
import Network.AWS.ElasticSearch.Types.LogPublishingOption
import Network.AWS.ElasticSearch.Types.LogType
import Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptions
import Network.AWS.ElasticSearch.Types.ServiceSoftwareOptions
import Network.AWS.ElasticSearch.Types.SnapshotOptions
import Network.AWS.ElasticSearch.Types.VPCDerivedInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The current status of an Elasticsearch domain.
--
-- /See:/ 'mkElasticsearchDomainStatus' smart constructor.
data ElasticsearchDomainStatus = ElasticsearchDomainStatus'
  { ebsOptions ::
      Lude.Maybe EBSOptions,
    nodeToNodeEncryptionOptions ::
      Lude.Maybe NodeToNodeEncryptionOptions,
    accessPolicies :: Lude.Maybe Lude.Text,
    serviceSoftwareOptions ::
      Lude.Maybe ServiceSoftwareOptions,
    logPublishingOptions ::
      Lude.Maybe
        ( Lude.HashMap
            LogType
            (LogPublishingOption)
        ),
    advancedSecurityOptions ::
      Lude.Maybe AdvancedSecurityOptions,
    created :: Lude.Maybe Lude.Bool,
    snapshotOptions ::
      Lude.Maybe SnapshotOptions,
    cognitoOptions ::
      Lude.Maybe CognitoOptions,
    encryptionAtRestOptions ::
      Lude.Maybe EncryptionAtRestOptions,
    deleted :: Lude.Maybe Lude.Bool,
    vpcOptions :: Lude.Maybe VPCDerivedInfo,
    endpoints ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    domainEndpointOptions ::
      Lude.Maybe DomainEndpointOptions,
    processing :: Lude.Maybe Lude.Bool,
    endpoint :: Lude.Maybe Lude.Text,
    upgradeProcessing ::
      Lude.Maybe Lude.Bool,
    advancedOptions ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    elasticsearchVersion ::
      Lude.Maybe Lude.Text,
    domainId :: Lude.Text,
    domainName :: Lude.Text,
    arn :: Lude.Text,
    elasticsearchClusterConfig ::
      ElasticsearchClusterConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElasticsearchDomainStatus' with the minimum fields required to make a request.
--
-- * 'accessPolicies' - IAM access policy as a JSON-formatted string.
-- * 'advancedOptions' - Specifies the status of the @AdvancedOptions@
-- * 'advancedSecurityOptions' - The current status of the Elasticsearch domain's advanced security options.
-- * 'arn' - The Amazon resource name (ARN) of an Elasticsearch domain. See <http://docs.aws.amazon.com/IAM/latest/UserGuide/index.html?Using_Identifiers.html Identifiers for IAM Entities> in /Using AWS Identity and Access Management/ for more information.
-- * 'cognitoOptions' - The @CognitoOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
-- * 'created' - The domain creation status. @True@ if the creation of an Elasticsearch domain is complete. @False@ if domain creation is still in progress.
-- * 'deleted' - The domain deletion status. @True@ if a delete request has been received for the domain but resource cleanup is still in progress. @False@ if the domain has not been deleted. Once domain deletion is complete, the status of the domain is no longer returned.
-- * 'domainEndpointOptions' - The current status of the Elasticsearch domain's endpoint options.
-- * 'domainId' - The unique identifier for the specified Elasticsearch domain.
-- * 'domainName' - The name of an Elasticsearch domain. Domain names are unique across the domains owned by an account within an AWS region. Domain names start with a letter or number and can contain the following characters: a-z (lowercase), 0-9, and - (hyphen).
-- * 'ebsOptions' - The @EBSOptions@ for the specified domain. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs Configuring EBS-based Storage> for more information.
-- * 'elasticsearchClusterConfig' - The type and number of instances in the domain cluster.
-- * 'elasticsearchVersion' - Undocumented field.
-- * 'encryptionAtRestOptions' - Specifies the status of the @EncryptionAtRestOptions@ .
-- * 'endpoint' - The Elasticsearch domain endpoint that you use to submit index and search requests.
-- * 'endpoints' - Map containing the Elasticsearch domain endpoints used to submit index and search requests. Example @key, value@ : @'vpc','vpc-endpoint-h2dsd34efgyghrtguk5gt6j2foh4.us-east-1.es.amazonaws.com'@ .
-- * 'logPublishingOptions' - Log publishing options for the given domain.
-- * 'nodeToNodeEncryptionOptions' - Specifies the status of the @NodeToNodeEncryptionOptions@ .
-- * 'processing' - The status of the Elasticsearch domain configuration. @True@ if Amazon Elasticsearch Service is processing configuration changes. @False@ if the configuration is active.
-- * 'serviceSoftwareOptions' - The current status of the Elasticsearch domain's service software.
-- * 'snapshotOptions' - Specifies the status of the @SnapshotOptions@
-- * 'upgradeProcessing' - The status of an Elasticsearch domain version upgrade. @True@ if Amazon Elasticsearch Service is undergoing a version upgrade. @False@ if the configuration is active.
-- * 'vpcOptions' - The @VPCOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
mkElasticsearchDomainStatus ::
  -- | 'domainId'
  Lude.Text ->
  -- | 'domainName'
  Lude.Text ->
  -- | 'arn'
  Lude.Text ->
  -- | 'elasticsearchClusterConfig'
  ElasticsearchClusterConfig ->
  ElasticsearchDomainStatus
mkElasticsearchDomainStatus
  pDomainId_
  pDomainName_
  pARN_
  pElasticsearchClusterConfig_ =
    ElasticsearchDomainStatus'
      { ebsOptions = Lude.Nothing,
        nodeToNodeEncryptionOptions = Lude.Nothing,
        accessPolicies = Lude.Nothing,
        serviceSoftwareOptions = Lude.Nothing,
        logPublishingOptions = Lude.Nothing,
        advancedSecurityOptions = Lude.Nothing,
        created = Lude.Nothing,
        snapshotOptions = Lude.Nothing,
        cognitoOptions = Lude.Nothing,
        encryptionAtRestOptions = Lude.Nothing,
        deleted = Lude.Nothing,
        vpcOptions = Lude.Nothing,
        endpoints = Lude.Nothing,
        domainEndpointOptions = Lude.Nothing,
        processing = Lude.Nothing,
        endpoint = Lude.Nothing,
        upgradeProcessing = Lude.Nothing,
        advancedOptions = Lude.Nothing,
        elasticsearchVersion = Lude.Nothing,
        domainId = pDomainId_,
        domainName = pDomainName_,
        arn = pARN_,
        elasticsearchClusterConfig = pElasticsearchClusterConfig_
      }

-- | The @EBSOptions@ for the specified domain. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs Configuring EBS-based Storage> for more information.
--
-- /Note:/ Consider using 'ebsOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsEBSOptions :: Lens.Lens' ElasticsearchDomainStatus (Lude.Maybe EBSOptions)
edsEBSOptions = Lens.lens (ebsOptions :: ElasticsearchDomainStatus -> Lude.Maybe EBSOptions) (\s a -> s {ebsOptions = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsEBSOptions "Use generic-lens or generic-optics with 'ebsOptions' instead." #-}

-- | Specifies the status of the @NodeToNodeEncryptionOptions@ .
--
-- /Note:/ Consider using 'nodeToNodeEncryptionOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsNodeToNodeEncryptionOptions :: Lens.Lens' ElasticsearchDomainStatus (Lude.Maybe NodeToNodeEncryptionOptions)
edsNodeToNodeEncryptionOptions = Lens.lens (nodeToNodeEncryptionOptions :: ElasticsearchDomainStatus -> Lude.Maybe NodeToNodeEncryptionOptions) (\s a -> s {nodeToNodeEncryptionOptions = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsNodeToNodeEncryptionOptions "Use generic-lens or generic-optics with 'nodeToNodeEncryptionOptions' instead." #-}

-- | IAM access policy as a JSON-formatted string.
--
-- /Note:/ Consider using 'accessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsAccessPolicies :: Lens.Lens' ElasticsearchDomainStatus (Lude.Maybe Lude.Text)
edsAccessPolicies = Lens.lens (accessPolicies :: ElasticsearchDomainStatus -> Lude.Maybe Lude.Text) (\s a -> s {accessPolicies = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsAccessPolicies "Use generic-lens or generic-optics with 'accessPolicies' instead." #-}

-- | The current status of the Elasticsearch domain's service software.
--
-- /Note:/ Consider using 'serviceSoftwareOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsServiceSoftwareOptions :: Lens.Lens' ElasticsearchDomainStatus (Lude.Maybe ServiceSoftwareOptions)
edsServiceSoftwareOptions = Lens.lens (serviceSoftwareOptions :: ElasticsearchDomainStatus -> Lude.Maybe ServiceSoftwareOptions) (\s a -> s {serviceSoftwareOptions = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsServiceSoftwareOptions "Use generic-lens or generic-optics with 'serviceSoftwareOptions' instead." #-}

-- | Log publishing options for the given domain.
--
-- /Note:/ Consider using 'logPublishingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsLogPublishingOptions :: Lens.Lens' ElasticsearchDomainStatus (Lude.Maybe (Lude.HashMap LogType (LogPublishingOption)))
edsLogPublishingOptions = Lens.lens (logPublishingOptions :: ElasticsearchDomainStatus -> Lude.Maybe (Lude.HashMap LogType (LogPublishingOption))) (\s a -> s {logPublishingOptions = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsLogPublishingOptions "Use generic-lens or generic-optics with 'logPublishingOptions' instead." #-}

-- | The current status of the Elasticsearch domain's advanced security options.
--
-- /Note:/ Consider using 'advancedSecurityOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsAdvancedSecurityOptions :: Lens.Lens' ElasticsearchDomainStatus (Lude.Maybe AdvancedSecurityOptions)
edsAdvancedSecurityOptions = Lens.lens (advancedSecurityOptions :: ElasticsearchDomainStatus -> Lude.Maybe AdvancedSecurityOptions) (\s a -> s {advancedSecurityOptions = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsAdvancedSecurityOptions "Use generic-lens or generic-optics with 'advancedSecurityOptions' instead." #-}

-- | The domain creation status. @True@ if the creation of an Elasticsearch domain is complete. @False@ if domain creation is still in progress.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsCreated :: Lens.Lens' ElasticsearchDomainStatus (Lude.Maybe Lude.Bool)
edsCreated = Lens.lens (created :: ElasticsearchDomainStatus -> Lude.Maybe Lude.Bool) (\s a -> s {created = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | Specifies the status of the @SnapshotOptions@
--
-- /Note:/ Consider using 'snapshotOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsSnapshotOptions :: Lens.Lens' ElasticsearchDomainStatus (Lude.Maybe SnapshotOptions)
edsSnapshotOptions = Lens.lens (snapshotOptions :: ElasticsearchDomainStatus -> Lude.Maybe SnapshotOptions) (\s a -> s {snapshotOptions = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsSnapshotOptions "Use generic-lens or generic-optics with 'snapshotOptions' instead." #-}

-- | The @CognitoOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
--
-- /Note:/ Consider using 'cognitoOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsCognitoOptions :: Lens.Lens' ElasticsearchDomainStatus (Lude.Maybe CognitoOptions)
edsCognitoOptions = Lens.lens (cognitoOptions :: ElasticsearchDomainStatus -> Lude.Maybe CognitoOptions) (\s a -> s {cognitoOptions = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsCognitoOptions "Use generic-lens or generic-optics with 'cognitoOptions' instead." #-}

-- | Specifies the status of the @EncryptionAtRestOptions@ .
--
-- /Note:/ Consider using 'encryptionAtRestOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsEncryptionAtRestOptions :: Lens.Lens' ElasticsearchDomainStatus (Lude.Maybe EncryptionAtRestOptions)
edsEncryptionAtRestOptions = Lens.lens (encryptionAtRestOptions :: ElasticsearchDomainStatus -> Lude.Maybe EncryptionAtRestOptions) (\s a -> s {encryptionAtRestOptions = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsEncryptionAtRestOptions "Use generic-lens or generic-optics with 'encryptionAtRestOptions' instead." #-}

-- | The domain deletion status. @True@ if a delete request has been received for the domain but resource cleanup is still in progress. @False@ if the domain has not been deleted. Once domain deletion is complete, the status of the domain is no longer returned.
--
-- /Note:/ Consider using 'deleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsDeleted :: Lens.Lens' ElasticsearchDomainStatus (Lude.Maybe Lude.Bool)
edsDeleted = Lens.lens (deleted :: ElasticsearchDomainStatus -> Lude.Maybe Lude.Bool) (\s a -> s {deleted = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsDeleted "Use generic-lens or generic-optics with 'deleted' instead." #-}

-- | The @VPCOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
--
-- /Note:/ Consider using 'vpcOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsVPCOptions :: Lens.Lens' ElasticsearchDomainStatus (Lude.Maybe VPCDerivedInfo)
edsVPCOptions = Lens.lens (vpcOptions :: ElasticsearchDomainStatus -> Lude.Maybe VPCDerivedInfo) (\s a -> s {vpcOptions = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsVPCOptions "Use generic-lens or generic-optics with 'vpcOptions' instead." #-}

-- | Map containing the Elasticsearch domain endpoints used to submit index and search requests. Example @key, value@ : @'vpc','vpc-endpoint-h2dsd34efgyghrtguk5gt6j2foh4.us-east-1.es.amazonaws.com'@ .
--
-- /Note:/ Consider using 'endpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsEndpoints :: Lens.Lens' ElasticsearchDomainStatus (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
edsEndpoints = Lens.lens (endpoints :: ElasticsearchDomainStatus -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {endpoints = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsEndpoints "Use generic-lens or generic-optics with 'endpoints' instead." #-}

-- | The current status of the Elasticsearch domain's endpoint options.
--
-- /Note:/ Consider using 'domainEndpointOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsDomainEndpointOptions :: Lens.Lens' ElasticsearchDomainStatus (Lude.Maybe DomainEndpointOptions)
edsDomainEndpointOptions = Lens.lens (domainEndpointOptions :: ElasticsearchDomainStatus -> Lude.Maybe DomainEndpointOptions) (\s a -> s {domainEndpointOptions = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsDomainEndpointOptions "Use generic-lens or generic-optics with 'domainEndpointOptions' instead." #-}

-- | The status of the Elasticsearch domain configuration. @True@ if Amazon Elasticsearch Service is processing configuration changes. @False@ if the configuration is active.
--
-- /Note:/ Consider using 'processing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsProcessing :: Lens.Lens' ElasticsearchDomainStatus (Lude.Maybe Lude.Bool)
edsProcessing = Lens.lens (processing :: ElasticsearchDomainStatus -> Lude.Maybe Lude.Bool) (\s a -> s {processing = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsProcessing "Use generic-lens or generic-optics with 'processing' instead." #-}

-- | The Elasticsearch domain endpoint that you use to submit index and search requests.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsEndpoint :: Lens.Lens' ElasticsearchDomainStatus (Lude.Maybe Lude.Text)
edsEndpoint = Lens.lens (endpoint :: ElasticsearchDomainStatus -> Lude.Maybe Lude.Text) (\s a -> s {endpoint = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The status of an Elasticsearch domain version upgrade. @True@ if Amazon Elasticsearch Service is undergoing a version upgrade. @False@ if the configuration is active.
--
-- /Note:/ Consider using 'upgradeProcessing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsUpgradeProcessing :: Lens.Lens' ElasticsearchDomainStatus (Lude.Maybe Lude.Bool)
edsUpgradeProcessing = Lens.lens (upgradeProcessing :: ElasticsearchDomainStatus -> Lude.Maybe Lude.Bool) (\s a -> s {upgradeProcessing = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsUpgradeProcessing "Use generic-lens or generic-optics with 'upgradeProcessing' instead." #-}

-- | Specifies the status of the @AdvancedOptions@
--
-- /Note:/ Consider using 'advancedOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsAdvancedOptions :: Lens.Lens' ElasticsearchDomainStatus (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
edsAdvancedOptions = Lens.lens (advancedOptions :: ElasticsearchDomainStatus -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {advancedOptions = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsAdvancedOptions "Use generic-lens or generic-optics with 'advancedOptions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'elasticsearchVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsElasticsearchVersion :: Lens.Lens' ElasticsearchDomainStatus (Lude.Maybe Lude.Text)
edsElasticsearchVersion = Lens.lens (elasticsearchVersion :: ElasticsearchDomainStatus -> Lude.Maybe Lude.Text) (\s a -> s {elasticsearchVersion = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsElasticsearchVersion "Use generic-lens or generic-optics with 'elasticsearchVersion' instead." #-}

-- | The unique identifier for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsDomainId :: Lens.Lens' ElasticsearchDomainStatus Lude.Text
edsDomainId = Lens.lens (domainId :: ElasticsearchDomainStatus -> Lude.Text) (\s a -> s {domainId = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | The name of an Elasticsearch domain. Domain names are unique across the domains owned by an account within an AWS region. Domain names start with a letter or number and can contain the following characters: a-z (lowercase), 0-9, and - (hyphen).
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsDomainName :: Lens.Lens' ElasticsearchDomainStatus Lude.Text
edsDomainName = Lens.lens (domainName :: ElasticsearchDomainStatus -> Lude.Text) (\s a -> s {domainName = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The Amazon resource name (ARN) of an Elasticsearch domain. See <http://docs.aws.amazon.com/IAM/latest/UserGuide/index.html?Using_Identifiers.html Identifiers for IAM Entities> in /Using AWS Identity and Access Management/ for more information.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsARN :: Lens.Lens' ElasticsearchDomainStatus Lude.Text
edsARN = Lens.lens (arn :: ElasticsearchDomainStatus -> Lude.Text) (\s a -> s {arn = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The type and number of instances in the domain cluster.
--
-- /Note:/ Consider using 'elasticsearchClusterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsElasticsearchClusterConfig :: Lens.Lens' ElasticsearchDomainStatus ElasticsearchClusterConfig
edsElasticsearchClusterConfig = Lens.lens (elasticsearchClusterConfig :: ElasticsearchDomainStatus -> ElasticsearchClusterConfig) (\s a -> s {elasticsearchClusterConfig = a} :: ElasticsearchDomainStatus)
{-# DEPRECATED edsElasticsearchClusterConfig "Use generic-lens or generic-optics with 'elasticsearchClusterConfig' instead." #-}

instance Lude.FromJSON ElasticsearchDomainStatus where
  parseJSON =
    Lude.withObject
      "ElasticsearchDomainStatus"
      ( \x ->
          ElasticsearchDomainStatus'
            Lude.<$> (x Lude..:? "EBSOptions")
            Lude.<*> (x Lude..:? "NodeToNodeEncryptionOptions")
            Lude.<*> (x Lude..:? "AccessPolicies")
            Lude.<*> (x Lude..:? "ServiceSoftwareOptions")
            Lude.<*> (x Lude..:? "LogPublishingOptions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AdvancedSecurityOptions")
            Lude.<*> (x Lude..:? "Created")
            Lude.<*> (x Lude..:? "SnapshotOptions")
            Lude.<*> (x Lude..:? "CognitoOptions")
            Lude.<*> (x Lude..:? "EncryptionAtRestOptions")
            Lude.<*> (x Lude..:? "Deleted")
            Lude.<*> (x Lude..:? "VPCOptions")
            Lude.<*> (x Lude..:? "Endpoints" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DomainEndpointOptions")
            Lude.<*> (x Lude..:? "Processing")
            Lude.<*> (x Lude..:? "Endpoint")
            Lude.<*> (x Lude..:? "UpgradeProcessing")
            Lude.<*> (x Lude..:? "AdvancedOptions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ElasticsearchVersion")
            Lude.<*> (x Lude..: "DomainId")
            Lude.<*> (x Lude..: "DomainName")
            Lude.<*> (x Lude..: "ARN")
            Lude.<*> (x Lude..: "ElasticsearchClusterConfig")
      )
