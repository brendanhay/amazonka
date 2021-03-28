{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ElasticsearchDomainConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.ElasticsearchDomainConfig
  ( ElasticsearchDomainConfig (..)
  -- * Smart constructor
  , mkElasticsearchDomainConfig
  -- * Lenses
  , edcAccessPolicies
  , edcAdvancedOptions
  , edcAdvancedSecurityOptions
  , edcCognitoOptions
  , edcDomainEndpointOptions
  , edcEBSOptions
  , edcElasticsearchClusterConfig
  , edcElasticsearchVersion
  , edcEncryptionAtRestOptions
  , edcLogPublishingOptions
  , edcNodeToNodeEncryptionOptions
  , edcSnapshotOptions
  , edcVPCOptions
  ) where

import qualified Network.AWS.ElasticSearch.Types.AccessPoliciesStatus as Types
import qualified Network.AWS.ElasticSearch.Types.AdvancedOptionsStatus as Types
import qualified Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsStatus as Types
import qualified Network.AWS.ElasticSearch.Types.CognitoOptionsStatus as Types
import qualified Network.AWS.ElasticSearch.Types.DomainEndpointOptionsStatus as Types
import qualified Network.AWS.ElasticSearch.Types.EBSOptionsStatus as Types
import qualified Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfigStatus as Types
import qualified Network.AWS.ElasticSearch.Types.ElasticsearchVersionStatus as Types
import qualified Network.AWS.ElasticSearch.Types.EncryptionAtRestOptionsStatus as Types
import qualified Network.AWS.ElasticSearch.Types.LogPublishingOptionsStatus as Types
import qualified Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus as Types
import qualified Network.AWS.ElasticSearch.Types.SnapshotOptionsStatus as Types
import qualified Network.AWS.ElasticSearch.Types.VPCDerivedInfoStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The configuration of an Elasticsearch domain.
--
-- /See:/ 'mkElasticsearchDomainConfig' smart constructor.
data ElasticsearchDomainConfig = ElasticsearchDomainConfig'
  { accessPolicies :: Core.Maybe Types.AccessPoliciesStatus
    -- ^ IAM access policy as a JSON-formatted string.
  , advancedOptions :: Core.Maybe Types.AdvancedOptionsStatus
    -- ^ Specifies the @AdvancedOptions@ for the domain. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options> for more information.
  , advancedSecurityOptions :: Core.Maybe Types.AdvancedSecurityOptionsStatus
    -- ^ Specifies @AdvancedSecurityOptions@ for the domain. 
  , cognitoOptions :: Core.Maybe Types.CognitoOptionsStatus
    -- ^ The @CognitoOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
  , domainEndpointOptions :: Core.Maybe Types.DomainEndpointOptionsStatus
    -- ^ Specifies the @DomainEndpointOptions@ for the Elasticsearch domain.
  , eBSOptions :: Core.Maybe Types.EBSOptionsStatus
    -- ^ Specifies the @EBSOptions@ for the Elasticsearch domain.
  , elasticsearchClusterConfig :: Core.Maybe Types.ElasticsearchClusterConfigStatus
    -- ^ Specifies the @ElasticsearchClusterConfig@ for the Elasticsearch domain.
  , elasticsearchVersion :: Core.Maybe Types.ElasticsearchVersionStatus
    -- ^ String of format X.Y to specify version for the Elasticsearch domain.
  , encryptionAtRestOptions :: Core.Maybe Types.EncryptionAtRestOptionsStatus
    -- ^ Specifies the @EncryptionAtRestOptions@ for the Elasticsearch domain.
  , logPublishingOptions :: Core.Maybe Types.LogPublishingOptionsStatus
    -- ^ Log publishing options for the given domain.
  , nodeToNodeEncryptionOptions :: Core.Maybe Types.NodeToNodeEncryptionOptionsStatus
    -- ^ Specifies the @NodeToNodeEncryptionOptions@ for the Elasticsearch domain.
  , snapshotOptions :: Core.Maybe Types.SnapshotOptionsStatus
    -- ^ Specifies the @SnapshotOptions@ for the Elasticsearch domain.
  , vPCOptions :: Core.Maybe Types.VPCDerivedInfoStatus
    -- ^ The @VPCOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ElasticsearchDomainConfig' value with any optional fields omitted.
mkElasticsearchDomainConfig
    :: ElasticsearchDomainConfig
mkElasticsearchDomainConfig
  = ElasticsearchDomainConfig'{accessPolicies = Core.Nothing,
                               advancedOptions = Core.Nothing,
                               advancedSecurityOptions = Core.Nothing,
                               cognitoOptions = Core.Nothing,
                               domainEndpointOptions = Core.Nothing, eBSOptions = Core.Nothing,
                               elasticsearchClusterConfig = Core.Nothing,
                               elasticsearchVersion = Core.Nothing,
                               encryptionAtRestOptions = Core.Nothing,
                               logPublishingOptions = Core.Nothing,
                               nodeToNodeEncryptionOptions = Core.Nothing,
                               snapshotOptions = Core.Nothing, vPCOptions = Core.Nothing}

-- | IAM access policy as a JSON-formatted string.
--
-- /Note:/ Consider using 'accessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcAccessPolicies :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe Types.AccessPoliciesStatus)
edcAccessPolicies = Lens.field @"accessPolicies"
{-# INLINEABLE edcAccessPolicies #-}
{-# DEPRECATED accessPolicies "Use generic-lens or generic-optics with 'accessPolicies' instead"  #-}

-- | Specifies the @AdvancedOptions@ for the domain. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options> for more information.
--
-- /Note:/ Consider using 'advancedOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcAdvancedOptions :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe Types.AdvancedOptionsStatus)
edcAdvancedOptions = Lens.field @"advancedOptions"
{-# INLINEABLE edcAdvancedOptions #-}
{-# DEPRECATED advancedOptions "Use generic-lens or generic-optics with 'advancedOptions' instead"  #-}

-- | Specifies @AdvancedSecurityOptions@ for the domain. 
--
-- /Note:/ Consider using 'advancedSecurityOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcAdvancedSecurityOptions :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe Types.AdvancedSecurityOptionsStatus)
edcAdvancedSecurityOptions = Lens.field @"advancedSecurityOptions"
{-# INLINEABLE edcAdvancedSecurityOptions #-}
{-# DEPRECATED advancedSecurityOptions "Use generic-lens or generic-optics with 'advancedSecurityOptions' instead"  #-}

-- | The @CognitoOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
--
-- /Note:/ Consider using 'cognitoOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcCognitoOptions :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe Types.CognitoOptionsStatus)
edcCognitoOptions = Lens.field @"cognitoOptions"
{-# INLINEABLE edcCognitoOptions #-}
{-# DEPRECATED cognitoOptions "Use generic-lens or generic-optics with 'cognitoOptions' instead"  #-}

-- | Specifies the @DomainEndpointOptions@ for the Elasticsearch domain.
--
-- /Note:/ Consider using 'domainEndpointOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcDomainEndpointOptions :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe Types.DomainEndpointOptionsStatus)
edcDomainEndpointOptions = Lens.field @"domainEndpointOptions"
{-# INLINEABLE edcDomainEndpointOptions #-}
{-# DEPRECATED domainEndpointOptions "Use generic-lens or generic-optics with 'domainEndpointOptions' instead"  #-}

-- | Specifies the @EBSOptions@ for the Elasticsearch domain.
--
-- /Note:/ Consider using 'eBSOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcEBSOptions :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe Types.EBSOptionsStatus)
edcEBSOptions = Lens.field @"eBSOptions"
{-# INLINEABLE edcEBSOptions #-}
{-# DEPRECATED eBSOptions "Use generic-lens or generic-optics with 'eBSOptions' instead"  #-}

-- | Specifies the @ElasticsearchClusterConfig@ for the Elasticsearch domain.
--
-- /Note:/ Consider using 'elasticsearchClusterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcElasticsearchClusterConfig :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe Types.ElasticsearchClusterConfigStatus)
edcElasticsearchClusterConfig = Lens.field @"elasticsearchClusterConfig"
{-# INLINEABLE edcElasticsearchClusterConfig #-}
{-# DEPRECATED elasticsearchClusterConfig "Use generic-lens or generic-optics with 'elasticsearchClusterConfig' instead"  #-}

-- | String of format X.Y to specify version for the Elasticsearch domain.
--
-- /Note:/ Consider using 'elasticsearchVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcElasticsearchVersion :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe Types.ElasticsearchVersionStatus)
edcElasticsearchVersion = Lens.field @"elasticsearchVersion"
{-# INLINEABLE edcElasticsearchVersion #-}
{-# DEPRECATED elasticsearchVersion "Use generic-lens or generic-optics with 'elasticsearchVersion' instead"  #-}

-- | Specifies the @EncryptionAtRestOptions@ for the Elasticsearch domain.
--
-- /Note:/ Consider using 'encryptionAtRestOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcEncryptionAtRestOptions :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe Types.EncryptionAtRestOptionsStatus)
edcEncryptionAtRestOptions = Lens.field @"encryptionAtRestOptions"
{-# INLINEABLE edcEncryptionAtRestOptions #-}
{-# DEPRECATED encryptionAtRestOptions "Use generic-lens or generic-optics with 'encryptionAtRestOptions' instead"  #-}

-- | Log publishing options for the given domain.
--
-- /Note:/ Consider using 'logPublishingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcLogPublishingOptions :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe Types.LogPublishingOptionsStatus)
edcLogPublishingOptions = Lens.field @"logPublishingOptions"
{-# INLINEABLE edcLogPublishingOptions #-}
{-# DEPRECATED logPublishingOptions "Use generic-lens or generic-optics with 'logPublishingOptions' instead"  #-}

-- | Specifies the @NodeToNodeEncryptionOptions@ for the Elasticsearch domain.
--
-- /Note:/ Consider using 'nodeToNodeEncryptionOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcNodeToNodeEncryptionOptions :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe Types.NodeToNodeEncryptionOptionsStatus)
edcNodeToNodeEncryptionOptions = Lens.field @"nodeToNodeEncryptionOptions"
{-# INLINEABLE edcNodeToNodeEncryptionOptions #-}
{-# DEPRECATED nodeToNodeEncryptionOptions "Use generic-lens or generic-optics with 'nodeToNodeEncryptionOptions' instead"  #-}

-- | Specifies the @SnapshotOptions@ for the Elasticsearch domain.
--
-- /Note:/ Consider using 'snapshotOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcSnapshotOptions :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe Types.SnapshotOptionsStatus)
edcSnapshotOptions = Lens.field @"snapshotOptions"
{-# INLINEABLE edcSnapshotOptions #-}
{-# DEPRECATED snapshotOptions "Use generic-lens or generic-optics with 'snapshotOptions' instead"  #-}

-- | The @VPCOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
--
-- /Note:/ Consider using 'vPCOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcVPCOptions :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe Types.VPCDerivedInfoStatus)
edcVPCOptions = Lens.field @"vPCOptions"
{-# INLINEABLE edcVPCOptions #-}
{-# DEPRECATED vPCOptions "Use generic-lens or generic-optics with 'vPCOptions' instead"  #-}

instance Core.FromJSON ElasticsearchDomainConfig where
        parseJSON
          = Core.withObject "ElasticsearchDomainConfig" Core.$
              \ x ->
                ElasticsearchDomainConfig' Core.<$>
                  (x Core..:? "AccessPolicies") Core.<*> x Core..:? "AdvancedOptions"
                    Core.<*> x Core..:? "AdvancedSecurityOptions"
                    Core.<*> x Core..:? "CognitoOptions"
                    Core.<*> x Core..:? "DomainEndpointOptions"
                    Core.<*> x Core..:? "EBSOptions"
                    Core.<*> x Core..:? "ElasticsearchClusterConfig"
                    Core.<*> x Core..:? "ElasticsearchVersion"
                    Core.<*> x Core..:? "EncryptionAtRestOptions"
                    Core.<*> x Core..:? "LogPublishingOptions"
                    Core.<*> x Core..:? "NodeToNodeEncryptionOptions"
                    Core.<*> x Core..:? "SnapshotOptions"
                    Core.<*> x Core..:? "VPCOptions"
