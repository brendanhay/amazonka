{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ElasticsearchDomainConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ElasticsearchDomainConfig
  ( ElasticsearchDomainConfig (..),

    -- * Smart constructor
    mkElasticsearchDomainConfig,

    -- * Lenses
    edcEBSOptions,
    edcNodeToNodeEncryptionOptions,
    edcAccessPolicies,
    edcLogPublishingOptions,
    edcAdvancedSecurityOptions,
    edcElasticsearchClusterConfig,
    edcSnapshotOptions,
    edcCognitoOptions,
    edcEncryptionAtRestOptions,
    edcVPCOptions,
    edcDomainEndpointOptions,
    edcAdvancedOptions,
    edcElasticsearchVersion,
  )
where

import Network.AWS.ElasticSearch.Types.AccessPoliciesStatus
import Network.AWS.ElasticSearch.Types.AdvancedOptionsStatus
import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsStatus
import Network.AWS.ElasticSearch.Types.CognitoOptionsStatus
import Network.AWS.ElasticSearch.Types.DomainEndpointOptionsStatus
import Network.AWS.ElasticSearch.Types.EBSOptionsStatus
import Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfigStatus
import Network.AWS.ElasticSearch.Types.ElasticsearchVersionStatus
import Network.AWS.ElasticSearch.Types.EncryptionAtRestOptionsStatus
import Network.AWS.ElasticSearch.Types.LogPublishingOptionsStatus
import Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus
import Network.AWS.ElasticSearch.Types.SnapshotOptionsStatus
import Network.AWS.ElasticSearch.Types.VPCDerivedInfoStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The configuration of an Elasticsearch domain.
--
-- /See:/ 'mkElasticsearchDomainConfig' smart constructor.
data ElasticsearchDomainConfig = ElasticsearchDomainConfig'
  { ebsOptions ::
      Lude.Maybe EBSOptionsStatus,
    nodeToNodeEncryptionOptions ::
      Lude.Maybe
        NodeToNodeEncryptionOptionsStatus,
    accessPolicies ::
      Lude.Maybe AccessPoliciesStatus,
    logPublishingOptions ::
      Lude.Maybe LogPublishingOptionsStatus,
    advancedSecurityOptions ::
      Lude.Maybe
        AdvancedSecurityOptionsStatus,
    elasticsearchClusterConfig ::
      Lude.Maybe
        ElasticsearchClusterConfigStatus,
    snapshotOptions ::
      Lude.Maybe SnapshotOptionsStatus,
    cognitoOptions ::
      Lude.Maybe CognitoOptionsStatus,
    encryptionAtRestOptions ::
      Lude.Maybe
        EncryptionAtRestOptionsStatus,
    vpcOptions ::
      Lude.Maybe VPCDerivedInfoStatus,
    domainEndpointOptions ::
      Lude.Maybe DomainEndpointOptionsStatus,
    advancedOptions ::
      Lude.Maybe AdvancedOptionsStatus,
    elasticsearchVersion ::
      Lude.Maybe ElasticsearchVersionStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElasticsearchDomainConfig' with the minimum fields required to make a request.
--
-- * 'accessPolicies' - IAM access policy as a JSON-formatted string.
-- * 'advancedOptions' - Specifies the @AdvancedOptions@ for the domain. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options> for more information.
-- * 'advancedSecurityOptions' - Specifies @AdvancedSecurityOptions@ for the domain.
-- * 'cognitoOptions' - The @CognitoOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
-- * 'domainEndpointOptions' - Specifies the @DomainEndpointOptions@ for the Elasticsearch domain.
-- * 'ebsOptions' - Specifies the @EBSOptions@ for the Elasticsearch domain.
-- * 'elasticsearchClusterConfig' - Specifies the @ElasticsearchClusterConfig@ for the Elasticsearch domain.
-- * 'elasticsearchVersion' - String of format X.Y to specify version for the Elasticsearch domain.
-- * 'encryptionAtRestOptions' - Specifies the @EncryptionAtRestOptions@ for the Elasticsearch domain.
-- * 'logPublishingOptions' - Log publishing options for the given domain.
-- * 'nodeToNodeEncryptionOptions' - Specifies the @NodeToNodeEncryptionOptions@ for the Elasticsearch domain.
-- * 'snapshotOptions' - Specifies the @SnapshotOptions@ for the Elasticsearch domain.
-- * 'vpcOptions' - The @VPCOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
mkElasticsearchDomainConfig ::
  ElasticsearchDomainConfig
mkElasticsearchDomainConfig =
  ElasticsearchDomainConfig'
    { ebsOptions = Lude.Nothing,
      nodeToNodeEncryptionOptions = Lude.Nothing,
      accessPolicies = Lude.Nothing,
      logPublishingOptions = Lude.Nothing,
      advancedSecurityOptions = Lude.Nothing,
      elasticsearchClusterConfig = Lude.Nothing,
      snapshotOptions = Lude.Nothing,
      cognitoOptions = Lude.Nothing,
      encryptionAtRestOptions = Lude.Nothing,
      vpcOptions = Lude.Nothing,
      domainEndpointOptions = Lude.Nothing,
      advancedOptions = Lude.Nothing,
      elasticsearchVersion = Lude.Nothing
    }

-- | Specifies the @EBSOptions@ for the Elasticsearch domain.
--
-- /Note:/ Consider using 'ebsOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcEBSOptions :: Lens.Lens' ElasticsearchDomainConfig (Lude.Maybe EBSOptionsStatus)
edcEBSOptions = Lens.lens (ebsOptions :: ElasticsearchDomainConfig -> Lude.Maybe EBSOptionsStatus) (\s a -> s {ebsOptions = a} :: ElasticsearchDomainConfig)
{-# DEPRECATED edcEBSOptions "Use generic-lens or generic-optics with 'ebsOptions' instead." #-}

-- | Specifies the @NodeToNodeEncryptionOptions@ for the Elasticsearch domain.
--
-- /Note:/ Consider using 'nodeToNodeEncryptionOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcNodeToNodeEncryptionOptions :: Lens.Lens' ElasticsearchDomainConfig (Lude.Maybe NodeToNodeEncryptionOptionsStatus)
edcNodeToNodeEncryptionOptions = Lens.lens (nodeToNodeEncryptionOptions :: ElasticsearchDomainConfig -> Lude.Maybe NodeToNodeEncryptionOptionsStatus) (\s a -> s {nodeToNodeEncryptionOptions = a} :: ElasticsearchDomainConfig)
{-# DEPRECATED edcNodeToNodeEncryptionOptions "Use generic-lens or generic-optics with 'nodeToNodeEncryptionOptions' instead." #-}

-- | IAM access policy as a JSON-formatted string.
--
-- /Note:/ Consider using 'accessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcAccessPolicies :: Lens.Lens' ElasticsearchDomainConfig (Lude.Maybe AccessPoliciesStatus)
edcAccessPolicies = Lens.lens (accessPolicies :: ElasticsearchDomainConfig -> Lude.Maybe AccessPoliciesStatus) (\s a -> s {accessPolicies = a} :: ElasticsearchDomainConfig)
{-# DEPRECATED edcAccessPolicies "Use generic-lens or generic-optics with 'accessPolicies' instead." #-}

-- | Log publishing options for the given domain.
--
-- /Note:/ Consider using 'logPublishingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcLogPublishingOptions :: Lens.Lens' ElasticsearchDomainConfig (Lude.Maybe LogPublishingOptionsStatus)
edcLogPublishingOptions = Lens.lens (logPublishingOptions :: ElasticsearchDomainConfig -> Lude.Maybe LogPublishingOptionsStatus) (\s a -> s {logPublishingOptions = a} :: ElasticsearchDomainConfig)
{-# DEPRECATED edcLogPublishingOptions "Use generic-lens or generic-optics with 'logPublishingOptions' instead." #-}

-- | Specifies @AdvancedSecurityOptions@ for the domain.
--
-- /Note:/ Consider using 'advancedSecurityOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcAdvancedSecurityOptions :: Lens.Lens' ElasticsearchDomainConfig (Lude.Maybe AdvancedSecurityOptionsStatus)
edcAdvancedSecurityOptions = Lens.lens (advancedSecurityOptions :: ElasticsearchDomainConfig -> Lude.Maybe AdvancedSecurityOptionsStatus) (\s a -> s {advancedSecurityOptions = a} :: ElasticsearchDomainConfig)
{-# DEPRECATED edcAdvancedSecurityOptions "Use generic-lens or generic-optics with 'advancedSecurityOptions' instead." #-}

-- | Specifies the @ElasticsearchClusterConfig@ for the Elasticsearch domain.
--
-- /Note:/ Consider using 'elasticsearchClusterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcElasticsearchClusterConfig :: Lens.Lens' ElasticsearchDomainConfig (Lude.Maybe ElasticsearchClusterConfigStatus)
edcElasticsearchClusterConfig = Lens.lens (elasticsearchClusterConfig :: ElasticsearchDomainConfig -> Lude.Maybe ElasticsearchClusterConfigStatus) (\s a -> s {elasticsearchClusterConfig = a} :: ElasticsearchDomainConfig)
{-# DEPRECATED edcElasticsearchClusterConfig "Use generic-lens or generic-optics with 'elasticsearchClusterConfig' instead." #-}

-- | Specifies the @SnapshotOptions@ for the Elasticsearch domain.
--
-- /Note:/ Consider using 'snapshotOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcSnapshotOptions :: Lens.Lens' ElasticsearchDomainConfig (Lude.Maybe SnapshotOptionsStatus)
edcSnapshotOptions = Lens.lens (snapshotOptions :: ElasticsearchDomainConfig -> Lude.Maybe SnapshotOptionsStatus) (\s a -> s {snapshotOptions = a} :: ElasticsearchDomainConfig)
{-# DEPRECATED edcSnapshotOptions "Use generic-lens or generic-optics with 'snapshotOptions' instead." #-}

-- | The @CognitoOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
--
-- /Note:/ Consider using 'cognitoOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcCognitoOptions :: Lens.Lens' ElasticsearchDomainConfig (Lude.Maybe CognitoOptionsStatus)
edcCognitoOptions = Lens.lens (cognitoOptions :: ElasticsearchDomainConfig -> Lude.Maybe CognitoOptionsStatus) (\s a -> s {cognitoOptions = a} :: ElasticsearchDomainConfig)
{-# DEPRECATED edcCognitoOptions "Use generic-lens or generic-optics with 'cognitoOptions' instead." #-}

-- | Specifies the @EncryptionAtRestOptions@ for the Elasticsearch domain.
--
-- /Note:/ Consider using 'encryptionAtRestOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcEncryptionAtRestOptions :: Lens.Lens' ElasticsearchDomainConfig (Lude.Maybe EncryptionAtRestOptionsStatus)
edcEncryptionAtRestOptions = Lens.lens (encryptionAtRestOptions :: ElasticsearchDomainConfig -> Lude.Maybe EncryptionAtRestOptionsStatus) (\s a -> s {encryptionAtRestOptions = a} :: ElasticsearchDomainConfig)
{-# DEPRECATED edcEncryptionAtRestOptions "Use generic-lens or generic-optics with 'encryptionAtRestOptions' instead." #-}

-- | The @VPCOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
--
-- /Note:/ Consider using 'vpcOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcVPCOptions :: Lens.Lens' ElasticsearchDomainConfig (Lude.Maybe VPCDerivedInfoStatus)
edcVPCOptions = Lens.lens (vpcOptions :: ElasticsearchDomainConfig -> Lude.Maybe VPCDerivedInfoStatus) (\s a -> s {vpcOptions = a} :: ElasticsearchDomainConfig)
{-# DEPRECATED edcVPCOptions "Use generic-lens or generic-optics with 'vpcOptions' instead." #-}

-- | Specifies the @DomainEndpointOptions@ for the Elasticsearch domain.
--
-- /Note:/ Consider using 'domainEndpointOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcDomainEndpointOptions :: Lens.Lens' ElasticsearchDomainConfig (Lude.Maybe DomainEndpointOptionsStatus)
edcDomainEndpointOptions = Lens.lens (domainEndpointOptions :: ElasticsearchDomainConfig -> Lude.Maybe DomainEndpointOptionsStatus) (\s a -> s {domainEndpointOptions = a} :: ElasticsearchDomainConfig)
{-# DEPRECATED edcDomainEndpointOptions "Use generic-lens or generic-optics with 'domainEndpointOptions' instead." #-}

-- | Specifies the @AdvancedOptions@ for the domain. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options> for more information.
--
-- /Note:/ Consider using 'advancedOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcAdvancedOptions :: Lens.Lens' ElasticsearchDomainConfig (Lude.Maybe AdvancedOptionsStatus)
edcAdvancedOptions = Lens.lens (advancedOptions :: ElasticsearchDomainConfig -> Lude.Maybe AdvancedOptionsStatus) (\s a -> s {advancedOptions = a} :: ElasticsearchDomainConfig)
{-# DEPRECATED edcAdvancedOptions "Use generic-lens or generic-optics with 'advancedOptions' instead." #-}

-- | String of format X.Y to specify version for the Elasticsearch domain.
--
-- /Note:/ Consider using 'elasticsearchVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcElasticsearchVersion :: Lens.Lens' ElasticsearchDomainConfig (Lude.Maybe ElasticsearchVersionStatus)
edcElasticsearchVersion = Lens.lens (elasticsearchVersion :: ElasticsearchDomainConfig -> Lude.Maybe ElasticsearchVersionStatus) (\s a -> s {elasticsearchVersion = a} :: ElasticsearchDomainConfig)
{-# DEPRECATED edcElasticsearchVersion "Use generic-lens or generic-optics with 'elasticsearchVersion' instead." #-}

instance Lude.FromJSON ElasticsearchDomainConfig where
  parseJSON =
    Lude.withObject
      "ElasticsearchDomainConfig"
      ( \x ->
          ElasticsearchDomainConfig'
            Lude.<$> (x Lude..:? "EBSOptions")
            Lude.<*> (x Lude..:? "NodeToNodeEncryptionOptions")
            Lude.<*> (x Lude..:? "AccessPolicies")
            Lude.<*> (x Lude..:? "LogPublishingOptions")
            Lude.<*> (x Lude..:? "AdvancedSecurityOptions")
            Lude.<*> (x Lude..:? "ElasticsearchClusterConfig")
            Lude.<*> (x Lude..:? "SnapshotOptions")
            Lude.<*> (x Lude..:? "CognitoOptions")
            Lude.<*> (x Lude..:? "EncryptionAtRestOptions")
            Lude.<*> (x Lude..:? "VPCOptions")
            Lude.<*> (x Lude..:? "DomainEndpointOptions")
            Lude.<*> (x Lude..:? "AdvancedOptions")
            Lude.<*> (x Lude..:? "ElasticsearchVersion")
      )
