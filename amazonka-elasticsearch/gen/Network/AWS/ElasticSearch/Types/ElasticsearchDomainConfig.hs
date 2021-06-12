{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ElasticsearchDomainConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ElasticsearchDomainConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types.AccessPoliciesStatus
import Network.AWS.ElasticSearch.Types.AdvancedOptionsStatus
import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsStatus
import Network.AWS.ElasticSearch.Types.AutoTuneOptionsStatus
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

-- | The configuration of an Elasticsearch domain.
--
-- /See:/ 'newElasticsearchDomainConfig' smart constructor.
data ElasticsearchDomainConfig = ElasticsearchDomainConfig'
  { -- | Specifies the @EBSOptions@ for the Elasticsearch domain.
    eBSOptions :: Core.Maybe EBSOptionsStatus,
    -- | Specifies the @SnapshotOptions@ for the Elasticsearch domain.
    snapshotOptions :: Core.Maybe SnapshotOptionsStatus,
    -- | Specifies the @ElasticsearchClusterConfig@ for the Elasticsearch domain.
    elasticsearchClusterConfig :: Core.Maybe ElasticsearchClusterConfigStatus,
    -- | Specifies the @DomainEndpointOptions@ for the Elasticsearch domain.
    domainEndpointOptions :: Core.Maybe DomainEndpointOptionsStatus,
    -- | The @VPCOptions@ for the specified domain. For more information, see
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains>.
    vPCOptions :: Core.Maybe VPCDerivedInfoStatus,
    -- | Specifies @AutoTuneOptions@ for the domain.
    autoTuneOptions :: Core.Maybe AutoTuneOptionsStatus,
    -- | IAM access policy as a JSON-formatted string.
    accessPolicies :: Core.Maybe AccessPoliciesStatus,
    -- | Specifies the @EncryptionAtRestOptions@ for the Elasticsearch domain.
    encryptionAtRestOptions :: Core.Maybe EncryptionAtRestOptionsStatus,
    -- | The @CognitoOptions@ for the specified domain. For more information, see
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
    cognitoOptions :: Core.Maybe CognitoOptionsStatus,
    -- | Specifies the @NodeToNodeEncryptionOptions@ for the Elasticsearch
    -- domain.
    nodeToNodeEncryptionOptions :: Core.Maybe NodeToNodeEncryptionOptionsStatus,
    -- | String of format X.Y to specify version for the Elasticsearch domain.
    elasticsearchVersion :: Core.Maybe ElasticsearchVersionStatus,
    -- | Specifies the @AdvancedOptions@ for the domain. See
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options>
    -- for more information.
    advancedOptions :: Core.Maybe AdvancedOptionsStatus,
    -- | Specifies @AdvancedSecurityOptions@ for the domain.
    advancedSecurityOptions :: Core.Maybe AdvancedSecurityOptionsStatus,
    -- | Log publishing options for the given domain.
    logPublishingOptions :: Core.Maybe LogPublishingOptionsStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ElasticsearchDomainConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eBSOptions', 'elasticsearchDomainConfig_eBSOptions' - Specifies the @EBSOptions@ for the Elasticsearch domain.
--
-- 'snapshotOptions', 'elasticsearchDomainConfig_snapshotOptions' - Specifies the @SnapshotOptions@ for the Elasticsearch domain.
--
-- 'elasticsearchClusterConfig', 'elasticsearchDomainConfig_elasticsearchClusterConfig' - Specifies the @ElasticsearchClusterConfig@ for the Elasticsearch domain.
--
-- 'domainEndpointOptions', 'elasticsearchDomainConfig_domainEndpointOptions' - Specifies the @DomainEndpointOptions@ for the Elasticsearch domain.
--
-- 'vPCOptions', 'elasticsearchDomainConfig_vPCOptions' - The @VPCOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains>.
--
-- 'autoTuneOptions', 'elasticsearchDomainConfig_autoTuneOptions' - Specifies @AutoTuneOptions@ for the domain.
--
-- 'accessPolicies', 'elasticsearchDomainConfig_accessPolicies' - IAM access policy as a JSON-formatted string.
--
-- 'encryptionAtRestOptions', 'elasticsearchDomainConfig_encryptionAtRestOptions' - Specifies the @EncryptionAtRestOptions@ for the Elasticsearch domain.
--
-- 'cognitoOptions', 'elasticsearchDomainConfig_cognitoOptions' - The @CognitoOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
--
-- 'nodeToNodeEncryptionOptions', 'elasticsearchDomainConfig_nodeToNodeEncryptionOptions' - Specifies the @NodeToNodeEncryptionOptions@ for the Elasticsearch
-- domain.
--
-- 'elasticsearchVersion', 'elasticsearchDomainConfig_elasticsearchVersion' - String of format X.Y to specify version for the Elasticsearch domain.
--
-- 'advancedOptions', 'elasticsearchDomainConfig_advancedOptions' - Specifies the @AdvancedOptions@ for the domain. See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options>
-- for more information.
--
-- 'advancedSecurityOptions', 'elasticsearchDomainConfig_advancedSecurityOptions' - Specifies @AdvancedSecurityOptions@ for the domain.
--
-- 'logPublishingOptions', 'elasticsearchDomainConfig_logPublishingOptions' - Log publishing options for the given domain.
newElasticsearchDomainConfig ::
  ElasticsearchDomainConfig
newElasticsearchDomainConfig =
  ElasticsearchDomainConfig'
    { eBSOptions =
        Core.Nothing,
      snapshotOptions = Core.Nothing,
      elasticsearchClusterConfig = Core.Nothing,
      domainEndpointOptions = Core.Nothing,
      vPCOptions = Core.Nothing,
      autoTuneOptions = Core.Nothing,
      accessPolicies = Core.Nothing,
      encryptionAtRestOptions = Core.Nothing,
      cognitoOptions = Core.Nothing,
      nodeToNodeEncryptionOptions = Core.Nothing,
      elasticsearchVersion = Core.Nothing,
      advancedOptions = Core.Nothing,
      advancedSecurityOptions = Core.Nothing,
      logPublishingOptions = Core.Nothing
    }

-- | Specifies the @EBSOptions@ for the Elasticsearch domain.
elasticsearchDomainConfig_eBSOptions :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe EBSOptionsStatus)
elasticsearchDomainConfig_eBSOptions = Lens.lens (\ElasticsearchDomainConfig' {eBSOptions} -> eBSOptions) (\s@ElasticsearchDomainConfig' {} a -> s {eBSOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies the @SnapshotOptions@ for the Elasticsearch domain.
elasticsearchDomainConfig_snapshotOptions :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe SnapshotOptionsStatus)
elasticsearchDomainConfig_snapshotOptions = Lens.lens (\ElasticsearchDomainConfig' {snapshotOptions} -> snapshotOptions) (\s@ElasticsearchDomainConfig' {} a -> s {snapshotOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies the @ElasticsearchClusterConfig@ for the Elasticsearch domain.
elasticsearchDomainConfig_elasticsearchClusterConfig :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe ElasticsearchClusterConfigStatus)
elasticsearchDomainConfig_elasticsearchClusterConfig = Lens.lens (\ElasticsearchDomainConfig' {elasticsearchClusterConfig} -> elasticsearchClusterConfig) (\s@ElasticsearchDomainConfig' {} a -> s {elasticsearchClusterConfig = a} :: ElasticsearchDomainConfig)

-- | Specifies the @DomainEndpointOptions@ for the Elasticsearch domain.
elasticsearchDomainConfig_domainEndpointOptions :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe DomainEndpointOptionsStatus)
elasticsearchDomainConfig_domainEndpointOptions = Lens.lens (\ElasticsearchDomainConfig' {domainEndpointOptions} -> domainEndpointOptions) (\s@ElasticsearchDomainConfig' {} a -> s {domainEndpointOptions = a} :: ElasticsearchDomainConfig)

-- | The @VPCOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains>.
elasticsearchDomainConfig_vPCOptions :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe VPCDerivedInfoStatus)
elasticsearchDomainConfig_vPCOptions = Lens.lens (\ElasticsearchDomainConfig' {vPCOptions} -> vPCOptions) (\s@ElasticsearchDomainConfig' {} a -> s {vPCOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies @AutoTuneOptions@ for the domain.
elasticsearchDomainConfig_autoTuneOptions :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe AutoTuneOptionsStatus)
elasticsearchDomainConfig_autoTuneOptions = Lens.lens (\ElasticsearchDomainConfig' {autoTuneOptions} -> autoTuneOptions) (\s@ElasticsearchDomainConfig' {} a -> s {autoTuneOptions = a} :: ElasticsearchDomainConfig)

-- | IAM access policy as a JSON-formatted string.
elasticsearchDomainConfig_accessPolicies :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe AccessPoliciesStatus)
elasticsearchDomainConfig_accessPolicies = Lens.lens (\ElasticsearchDomainConfig' {accessPolicies} -> accessPolicies) (\s@ElasticsearchDomainConfig' {} a -> s {accessPolicies = a} :: ElasticsearchDomainConfig)

-- | Specifies the @EncryptionAtRestOptions@ for the Elasticsearch domain.
elasticsearchDomainConfig_encryptionAtRestOptions :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe EncryptionAtRestOptionsStatus)
elasticsearchDomainConfig_encryptionAtRestOptions = Lens.lens (\ElasticsearchDomainConfig' {encryptionAtRestOptions} -> encryptionAtRestOptions) (\s@ElasticsearchDomainConfig' {} a -> s {encryptionAtRestOptions = a} :: ElasticsearchDomainConfig)

-- | The @CognitoOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
elasticsearchDomainConfig_cognitoOptions :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe CognitoOptionsStatus)
elasticsearchDomainConfig_cognitoOptions = Lens.lens (\ElasticsearchDomainConfig' {cognitoOptions} -> cognitoOptions) (\s@ElasticsearchDomainConfig' {} a -> s {cognitoOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies the @NodeToNodeEncryptionOptions@ for the Elasticsearch
-- domain.
elasticsearchDomainConfig_nodeToNodeEncryptionOptions :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe NodeToNodeEncryptionOptionsStatus)
elasticsearchDomainConfig_nodeToNodeEncryptionOptions = Lens.lens (\ElasticsearchDomainConfig' {nodeToNodeEncryptionOptions} -> nodeToNodeEncryptionOptions) (\s@ElasticsearchDomainConfig' {} a -> s {nodeToNodeEncryptionOptions = a} :: ElasticsearchDomainConfig)

-- | String of format X.Y to specify version for the Elasticsearch domain.
elasticsearchDomainConfig_elasticsearchVersion :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe ElasticsearchVersionStatus)
elasticsearchDomainConfig_elasticsearchVersion = Lens.lens (\ElasticsearchDomainConfig' {elasticsearchVersion} -> elasticsearchVersion) (\s@ElasticsearchDomainConfig' {} a -> s {elasticsearchVersion = a} :: ElasticsearchDomainConfig)

-- | Specifies the @AdvancedOptions@ for the domain. See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options>
-- for more information.
elasticsearchDomainConfig_advancedOptions :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe AdvancedOptionsStatus)
elasticsearchDomainConfig_advancedOptions = Lens.lens (\ElasticsearchDomainConfig' {advancedOptions} -> advancedOptions) (\s@ElasticsearchDomainConfig' {} a -> s {advancedOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies @AdvancedSecurityOptions@ for the domain.
elasticsearchDomainConfig_advancedSecurityOptions :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe AdvancedSecurityOptionsStatus)
elasticsearchDomainConfig_advancedSecurityOptions = Lens.lens (\ElasticsearchDomainConfig' {advancedSecurityOptions} -> advancedSecurityOptions) (\s@ElasticsearchDomainConfig' {} a -> s {advancedSecurityOptions = a} :: ElasticsearchDomainConfig)

-- | Log publishing options for the given domain.
elasticsearchDomainConfig_logPublishingOptions :: Lens.Lens' ElasticsearchDomainConfig (Core.Maybe LogPublishingOptionsStatus)
elasticsearchDomainConfig_logPublishingOptions = Lens.lens (\ElasticsearchDomainConfig' {logPublishingOptions} -> logPublishingOptions) (\s@ElasticsearchDomainConfig' {} a -> s {logPublishingOptions = a} :: ElasticsearchDomainConfig)

instance Core.FromJSON ElasticsearchDomainConfig where
  parseJSON =
    Core.withObject
      "ElasticsearchDomainConfig"
      ( \x ->
          ElasticsearchDomainConfig'
            Core.<$> (x Core..:? "EBSOptions")
            Core.<*> (x Core..:? "SnapshotOptions")
            Core.<*> (x Core..:? "ElasticsearchClusterConfig")
            Core.<*> (x Core..:? "DomainEndpointOptions")
            Core.<*> (x Core..:? "VPCOptions")
            Core.<*> (x Core..:? "AutoTuneOptions")
            Core.<*> (x Core..:? "AccessPolicies")
            Core.<*> (x Core..:? "EncryptionAtRestOptions")
            Core.<*> (x Core..:? "CognitoOptions")
            Core.<*> (x Core..:? "NodeToNodeEncryptionOptions")
            Core.<*> (x Core..:? "ElasticsearchVersion")
            Core.<*> (x Core..:? "AdvancedOptions")
            Core.<*> (x Core..:? "AdvancedSecurityOptions")
            Core.<*> (x Core..:? "LogPublishingOptions")
      )

instance Core.Hashable ElasticsearchDomainConfig

instance Core.NFData ElasticsearchDomainConfig
