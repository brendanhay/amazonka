{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Prelude as Prelude

-- | The configuration of an Elasticsearch domain.
--
-- /See:/ 'newElasticsearchDomainConfig' smart constructor.
data ElasticsearchDomainConfig = ElasticsearchDomainConfig'
  { -- | Specifies the @EBSOptions@ for the Elasticsearch domain.
    eBSOptions :: Prelude.Maybe EBSOptionsStatus,
    -- | Specifies the @SnapshotOptions@ for the Elasticsearch domain.
    snapshotOptions :: Prelude.Maybe SnapshotOptionsStatus,
    -- | Specifies the @ElasticsearchClusterConfig@ for the Elasticsearch domain.
    elasticsearchClusterConfig :: Prelude.Maybe ElasticsearchClusterConfigStatus,
    -- | Specifies the @DomainEndpointOptions@ for the Elasticsearch domain.
    domainEndpointOptions :: Prelude.Maybe DomainEndpointOptionsStatus,
    -- | The @VPCOptions@ for the specified domain. For more information, see
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains>.
    vPCOptions :: Prelude.Maybe VPCDerivedInfoStatus,
    -- | Specifies @AutoTuneOptions@ for the domain.
    autoTuneOptions :: Prelude.Maybe AutoTuneOptionsStatus,
    -- | IAM access policy as a JSON-formatted string.
    accessPolicies :: Prelude.Maybe AccessPoliciesStatus,
    -- | Specifies the @EncryptionAtRestOptions@ for the Elasticsearch domain.
    encryptionAtRestOptions :: Prelude.Maybe EncryptionAtRestOptionsStatus,
    -- | The @CognitoOptions@ for the specified domain. For more information, see
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
    cognitoOptions :: Prelude.Maybe CognitoOptionsStatus,
    -- | Specifies the @NodeToNodeEncryptionOptions@ for the Elasticsearch
    -- domain.
    nodeToNodeEncryptionOptions :: Prelude.Maybe NodeToNodeEncryptionOptionsStatus,
    -- | String of format X.Y to specify version for the Elasticsearch domain.
    elasticsearchVersion :: Prelude.Maybe ElasticsearchVersionStatus,
    -- | Specifies the @AdvancedOptions@ for the domain. See
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options>
    -- for more information.
    advancedOptions :: Prelude.Maybe AdvancedOptionsStatus,
    -- | Specifies @AdvancedSecurityOptions@ for the domain.
    advancedSecurityOptions :: Prelude.Maybe AdvancedSecurityOptionsStatus,
    -- | Log publishing options for the given domain.
    logPublishingOptions :: Prelude.Maybe LogPublishingOptionsStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      snapshotOptions = Prelude.Nothing,
      elasticsearchClusterConfig = Prelude.Nothing,
      domainEndpointOptions = Prelude.Nothing,
      vPCOptions = Prelude.Nothing,
      autoTuneOptions = Prelude.Nothing,
      accessPolicies = Prelude.Nothing,
      encryptionAtRestOptions = Prelude.Nothing,
      cognitoOptions = Prelude.Nothing,
      nodeToNodeEncryptionOptions = Prelude.Nothing,
      elasticsearchVersion = Prelude.Nothing,
      advancedOptions = Prelude.Nothing,
      advancedSecurityOptions = Prelude.Nothing,
      logPublishingOptions = Prelude.Nothing
    }

-- | Specifies the @EBSOptions@ for the Elasticsearch domain.
elasticsearchDomainConfig_eBSOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe EBSOptionsStatus)
elasticsearchDomainConfig_eBSOptions = Lens.lens (\ElasticsearchDomainConfig' {eBSOptions} -> eBSOptions) (\s@ElasticsearchDomainConfig' {} a -> s {eBSOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies the @SnapshotOptions@ for the Elasticsearch domain.
elasticsearchDomainConfig_snapshotOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe SnapshotOptionsStatus)
elasticsearchDomainConfig_snapshotOptions = Lens.lens (\ElasticsearchDomainConfig' {snapshotOptions} -> snapshotOptions) (\s@ElasticsearchDomainConfig' {} a -> s {snapshotOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies the @ElasticsearchClusterConfig@ for the Elasticsearch domain.
elasticsearchDomainConfig_elasticsearchClusterConfig :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe ElasticsearchClusterConfigStatus)
elasticsearchDomainConfig_elasticsearchClusterConfig = Lens.lens (\ElasticsearchDomainConfig' {elasticsearchClusterConfig} -> elasticsearchClusterConfig) (\s@ElasticsearchDomainConfig' {} a -> s {elasticsearchClusterConfig = a} :: ElasticsearchDomainConfig)

-- | Specifies the @DomainEndpointOptions@ for the Elasticsearch domain.
elasticsearchDomainConfig_domainEndpointOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe DomainEndpointOptionsStatus)
elasticsearchDomainConfig_domainEndpointOptions = Lens.lens (\ElasticsearchDomainConfig' {domainEndpointOptions} -> domainEndpointOptions) (\s@ElasticsearchDomainConfig' {} a -> s {domainEndpointOptions = a} :: ElasticsearchDomainConfig)

-- | The @VPCOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains>.
elasticsearchDomainConfig_vPCOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe VPCDerivedInfoStatus)
elasticsearchDomainConfig_vPCOptions = Lens.lens (\ElasticsearchDomainConfig' {vPCOptions} -> vPCOptions) (\s@ElasticsearchDomainConfig' {} a -> s {vPCOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies @AutoTuneOptions@ for the domain.
elasticsearchDomainConfig_autoTuneOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe AutoTuneOptionsStatus)
elasticsearchDomainConfig_autoTuneOptions = Lens.lens (\ElasticsearchDomainConfig' {autoTuneOptions} -> autoTuneOptions) (\s@ElasticsearchDomainConfig' {} a -> s {autoTuneOptions = a} :: ElasticsearchDomainConfig)

-- | IAM access policy as a JSON-formatted string.
elasticsearchDomainConfig_accessPolicies :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe AccessPoliciesStatus)
elasticsearchDomainConfig_accessPolicies = Lens.lens (\ElasticsearchDomainConfig' {accessPolicies} -> accessPolicies) (\s@ElasticsearchDomainConfig' {} a -> s {accessPolicies = a} :: ElasticsearchDomainConfig)

-- | Specifies the @EncryptionAtRestOptions@ for the Elasticsearch domain.
elasticsearchDomainConfig_encryptionAtRestOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe EncryptionAtRestOptionsStatus)
elasticsearchDomainConfig_encryptionAtRestOptions = Lens.lens (\ElasticsearchDomainConfig' {encryptionAtRestOptions} -> encryptionAtRestOptions) (\s@ElasticsearchDomainConfig' {} a -> s {encryptionAtRestOptions = a} :: ElasticsearchDomainConfig)

-- | The @CognitoOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
elasticsearchDomainConfig_cognitoOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe CognitoOptionsStatus)
elasticsearchDomainConfig_cognitoOptions = Lens.lens (\ElasticsearchDomainConfig' {cognitoOptions} -> cognitoOptions) (\s@ElasticsearchDomainConfig' {} a -> s {cognitoOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies the @NodeToNodeEncryptionOptions@ for the Elasticsearch
-- domain.
elasticsearchDomainConfig_nodeToNodeEncryptionOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe NodeToNodeEncryptionOptionsStatus)
elasticsearchDomainConfig_nodeToNodeEncryptionOptions = Lens.lens (\ElasticsearchDomainConfig' {nodeToNodeEncryptionOptions} -> nodeToNodeEncryptionOptions) (\s@ElasticsearchDomainConfig' {} a -> s {nodeToNodeEncryptionOptions = a} :: ElasticsearchDomainConfig)

-- | String of format X.Y to specify version for the Elasticsearch domain.
elasticsearchDomainConfig_elasticsearchVersion :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe ElasticsearchVersionStatus)
elasticsearchDomainConfig_elasticsearchVersion = Lens.lens (\ElasticsearchDomainConfig' {elasticsearchVersion} -> elasticsearchVersion) (\s@ElasticsearchDomainConfig' {} a -> s {elasticsearchVersion = a} :: ElasticsearchDomainConfig)

-- | Specifies the @AdvancedOptions@ for the domain. See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options>
-- for more information.
elasticsearchDomainConfig_advancedOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe AdvancedOptionsStatus)
elasticsearchDomainConfig_advancedOptions = Lens.lens (\ElasticsearchDomainConfig' {advancedOptions} -> advancedOptions) (\s@ElasticsearchDomainConfig' {} a -> s {advancedOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies @AdvancedSecurityOptions@ for the domain.
elasticsearchDomainConfig_advancedSecurityOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe AdvancedSecurityOptionsStatus)
elasticsearchDomainConfig_advancedSecurityOptions = Lens.lens (\ElasticsearchDomainConfig' {advancedSecurityOptions} -> advancedSecurityOptions) (\s@ElasticsearchDomainConfig' {} a -> s {advancedSecurityOptions = a} :: ElasticsearchDomainConfig)

-- | Log publishing options for the given domain.
elasticsearchDomainConfig_logPublishingOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe LogPublishingOptionsStatus)
elasticsearchDomainConfig_logPublishingOptions = Lens.lens (\ElasticsearchDomainConfig' {logPublishingOptions} -> logPublishingOptions) (\s@ElasticsearchDomainConfig' {} a -> s {logPublishingOptions = a} :: ElasticsearchDomainConfig)

instance Prelude.FromJSON ElasticsearchDomainConfig where
  parseJSON =
    Prelude.withObject
      "ElasticsearchDomainConfig"
      ( \x ->
          ElasticsearchDomainConfig'
            Prelude.<$> (x Prelude..:? "EBSOptions")
            Prelude.<*> (x Prelude..:? "SnapshotOptions")
            Prelude.<*> (x Prelude..:? "ElasticsearchClusterConfig")
            Prelude.<*> (x Prelude..:? "DomainEndpointOptions")
            Prelude.<*> (x Prelude..:? "VPCOptions")
            Prelude.<*> (x Prelude..:? "AutoTuneOptions")
            Prelude.<*> (x Prelude..:? "AccessPolicies")
            Prelude.<*> (x Prelude..:? "EncryptionAtRestOptions")
            Prelude.<*> (x Prelude..:? "CognitoOptions")
            Prelude.<*> (x Prelude..:? "NodeToNodeEncryptionOptions")
            Prelude.<*> (x Prelude..:? "ElasticsearchVersion")
            Prelude.<*> (x Prelude..:? "AdvancedOptions")
            Prelude.<*> (x Prelude..:? "AdvancedSecurityOptions")
            Prelude.<*> (x Prelude..:? "LogPublishingOptions")
      )

instance Prelude.Hashable ElasticsearchDomainConfig

instance Prelude.NFData ElasticsearchDomainConfig
