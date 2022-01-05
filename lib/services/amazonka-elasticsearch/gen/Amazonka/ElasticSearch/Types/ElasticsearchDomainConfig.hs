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
-- Module      : Amazonka.ElasticSearch.Types.ElasticsearchDomainConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.ElasticsearchDomainConfig where

import qualified Amazonka.Core as Core
import Amazonka.ElasticSearch.Types.AccessPoliciesStatus
import Amazonka.ElasticSearch.Types.AdvancedOptionsStatus
import Amazonka.ElasticSearch.Types.AdvancedSecurityOptionsStatus
import Amazonka.ElasticSearch.Types.AutoTuneOptionsStatus
import Amazonka.ElasticSearch.Types.CognitoOptionsStatus
import Amazonka.ElasticSearch.Types.DomainEndpointOptionsStatus
import Amazonka.ElasticSearch.Types.EBSOptionsStatus
import Amazonka.ElasticSearch.Types.ElasticsearchClusterConfigStatus
import Amazonka.ElasticSearch.Types.ElasticsearchVersionStatus
import Amazonka.ElasticSearch.Types.EncryptionAtRestOptionsStatus
import Amazonka.ElasticSearch.Types.LogPublishingOptionsStatus
import Amazonka.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus
import Amazonka.ElasticSearch.Types.SnapshotOptionsStatus
import Amazonka.ElasticSearch.Types.VPCDerivedInfoStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration of an Elasticsearch domain.
--
-- /See:/ 'newElasticsearchDomainConfig' smart constructor.
data ElasticsearchDomainConfig = ElasticsearchDomainConfig'
  { -- | Specifies the @EBSOptions@ for the Elasticsearch domain.
    eBSOptions :: Prelude.Maybe EBSOptionsStatus,
    -- | Specifies the @NodeToNodeEncryptionOptions@ for the Elasticsearch
    -- domain.
    nodeToNodeEncryptionOptions :: Prelude.Maybe NodeToNodeEncryptionOptionsStatus,
    -- | IAM access policy as a JSON-formatted string.
    accessPolicies :: Prelude.Maybe AccessPoliciesStatus,
    -- | Specifies @AutoTuneOptions@ for the domain.
    autoTuneOptions :: Prelude.Maybe AutoTuneOptionsStatus,
    -- | Log publishing options for the given domain.
    logPublishingOptions :: Prelude.Maybe LogPublishingOptionsStatus,
    -- | Specifies @AdvancedSecurityOptions@ for the domain.
    advancedSecurityOptions :: Prelude.Maybe AdvancedSecurityOptionsStatus,
    -- | Specifies the @ElasticsearchClusterConfig@ for the Elasticsearch domain.
    elasticsearchClusterConfig :: Prelude.Maybe ElasticsearchClusterConfigStatus,
    -- | Specifies the @SnapshotOptions@ for the Elasticsearch domain.
    snapshotOptions :: Prelude.Maybe SnapshotOptionsStatus,
    -- | The @CognitoOptions@ for the specified domain. For more information, see
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
    cognitoOptions :: Prelude.Maybe CognitoOptionsStatus,
    -- | Specifies the @EncryptionAtRestOptions@ for the Elasticsearch domain.
    encryptionAtRestOptions :: Prelude.Maybe EncryptionAtRestOptionsStatus,
    -- | The @VPCOptions@ for the specified domain. For more information, see
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains>.
    vPCOptions :: Prelude.Maybe VPCDerivedInfoStatus,
    -- | Specifies the @DomainEndpointOptions@ for the Elasticsearch domain.
    domainEndpointOptions :: Prelude.Maybe DomainEndpointOptionsStatus,
    -- | Specifies the @AdvancedOptions@ for the domain. See
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options>
    -- for more information.
    advancedOptions :: Prelude.Maybe AdvancedOptionsStatus,
    -- | String of format X.Y to specify version for the Elasticsearch domain.
    elasticsearchVersion :: Prelude.Maybe ElasticsearchVersionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'nodeToNodeEncryptionOptions', 'elasticsearchDomainConfig_nodeToNodeEncryptionOptions' - Specifies the @NodeToNodeEncryptionOptions@ for the Elasticsearch
-- domain.
--
-- 'accessPolicies', 'elasticsearchDomainConfig_accessPolicies' - IAM access policy as a JSON-formatted string.
--
-- 'autoTuneOptions', 'elasticsearchDomainConfig_autoTuneOptions' - Specifies @AutoTuneOptions@ for the domain.
--
-- 'logPublishingOptions', 'elasticsearchDomainConfig_logPublishingOptions' - Log publishing options for the given domain.
--
-- 'advancedSecurityOptions', 'elasticsearchDomainConfig_advancedSecurityOptions' - Specifies @AdvancedSecurityOptions@ for the domain.
--
-- 'elasticsearchClusterConfig', 'elasticsearchDomainConfig_elasticsearchClusterConfig' - Specifies the @ElasticsearchClusterConfig@ for the Elasticsearch domain.
--
-- 'snapshotOptions', 'elasticsearchDomainConfig_snapshotOptions' - Specifies the @SnapshotOptions@ for the Elasticsearch domain.
--
-- 'cognitoOptions', 'elasticsearchDomainConfig_cognitoOptions' - The @CognitoOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
--
-- 'encryptionAtRestOptions', 'elasticsearchDomainConfig_encryptionAtRestOptions' - Specifies the @EncryptionAtRestOptions@ for the Elasticsearch domain.
--
-- 'vPCOptions', 'elasticsearchDomainConfig_vPCOptions' - The @VPCOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains>.
--
-- 'domainEndpointOptions', 'elasticsearchDomainConfig_domainEndpointOptions' - Specifies the @DomainEndpointOptions@ for the Elasticsearch domain.
--
-- 'advancedOptions', 'elasticsearchDomainConfig_advancedOptions' - Specifies the @AdvancedOptions@ for the domain. See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options>
-- for more information.
--
-- 'elasticsearchVersion', 'elasticsearchDomainConfig_elasticsearchVersion' - String of format X.Y to specify version for the Elasticsearch domain.
newElasticsearchDomainConfig ::
  ElasticsearchDomainConfig
newElasticsearchDomainConfig =
  ElasticsearchDomainConfig'
    { eBSOptions =
        Prelude.Nothing,
      nodeToNodeEncryptionOptions = Prelude.Nothing,
      accessPolicies = Prelude.Nothing,
      autoTuneOptions = Prelude.Nothing,
      logPublishingOptions = Prelude.Nothing,
      advancedSecurityOptions = Prelude.Nothing,
      elasticsearchClusterConfig = Prelude.Nothing,
      snapshotOptions = Prelude.Nothing,
      cognitoOptions = Prelude.Nothing,
      encryptionAtRestOptions = Prelude.Nothing,
      vPCOptions = Prelude.Nothing,
      domainEndpointOptions = Prelude.Nothing,
      advancedOptions = Prelude.Nothing,
      elasticsearchVersion = Prelude.Nothing
    }

-- | Specifies the @EBSOptions@ for the Elasticsearch domain.
elasticsearchDomainConfig_eBSOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe EBSOptionsStatus)
elasticsearchDomainConfig_eBSOptions = Lens.lens (\ElasticsearchDomainConfig' {eBSOptions} -> eBSOptions) (\s@ElasticsearchDomainConfig' {} a -> s {eBSOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies the @NodeToNodeEncryptionOptions@ for the Elasticsearch
-- domain.
elasticsearchDomainConfig_nodeToNodeEncryptionOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe NodeToNodeEncryptionOptionsStatus)
elasticsearchDomainConfig_nodeToNodeEncryptionOptions = Lens.lens (\ElasticsearchDomainConfig' {nodeToNodeEncryptionOptions} -> nodeToNodeEncryptionOptions) (\s@ElasticsearchDomainConfig' {} a -> s {nodeToNodeEncryptionOptions = a} :: ElasticsearchDomainConfig)

-- | IAM access policy as a JSON-formatted string.
elasticsearchDomainConfig_accessPolicies :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe AccessPoliciesStatus)
elasticsearchDomainConfig_accessPolicies = Lens.lens (\ElasticsearchDomainConfig' {accessPolicies} -> accessPolicies) (\s@ElasticsearchDomainConfig' {} a -> s {accessPolicies = a} :: ElasticsearchDomainConfig)

-- | Specifies @AutoTuneOptions@ for the domain.
elasticsearchDomainConfig_autoTuneOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe AutoTuneOptionsStatus)
elasticsearchDomainConfig_autoTuneOptions = Lens.lens (\ElasticsearchDomainConfig' {autoTuneOptions} -> autoTuneOptions) (\s@ElasticsearchDomainConfig' {} a -> s {autoTuneOptions = a} :: ElasticsearchDomainConfig)

-- | Log publishing options for the given domain.
elasticsearchDomainConfig_logPublishingOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe LogPublishingOptionsStatus)
elasticsearchDomainConfig_logPublishingOptions = Lens.lens (\ElasticsearchDomainConfig' {logPublishingOptions} -> logPublishingOptions) (\s@ElasticsearchDomainConfig' {} a -> s {logPublishingOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies @AdvancedSecurityOptions@ for the domain.
elasticsearchDomainConfig_advancedSecurityOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe AdvancedSecurityOptionsStatus)
elasticsearchDomainConfig_advancedSecurityOptions = Lens.lens (\ElasticsearchDomainConfig' {advancedSecurityOptions} -> advancedSecurityOptions) (\s@ElasticsearchDomainConfig' {} a -> s {advancedSecurityOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies the @ElasticsearchClusterConfig@ for the Elasticsearch domain.
elasticsearchDomainConfig_elasticsearchClusterConfig :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe ElasticsearchClusterConfigStatus)
elasticsearchDomainConfig_elasticsearchClusterConfig = Lens.lens (\ElasticsearchDomainConfig' {elasticsearchClusterConfig} -> elasticsearchClusterConfig) (\s@ElasticsearchDomainConfig' {} a -> s {elasticsearchClusterConfig = a} :: ElasticsearchDomainConfig)

-- | Specifies the @SnapshotOptions@ for the Elasticsearch domain.
elasticsearchDomainConfig_snapshotOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe SnapshotOptionsStatus)
elasticsearchDomainConfig_snapshotOptions = Lens.lens (\ElasticsearchDomainConfig' {snapshotOptions} -> snapshotOptions) (\s@ElasticsearchDomainConfig' {} a -> s {snapshotOptions = a} :: ElasticsearchDomainConfig)

-- | The @CognitoOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
elasticsearchDomainConfig_cognitoOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe CognitoOptionsStatus)
elasticsearchDomainConfig_cognitoOptions = Lens.lens (\ElasticsearchDomainConfig' {cognitoOptions} -> cognitoOptions) (\s@ElasticsearchDomainConfig' {} a -> s {cognitoOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies the @EncryptionAtRestOptions@ for the Elasticsearch domain.
elasticsearchDomainConfig_encryptionAtRestOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe EncryptionAtRestOptionsStatus)
elasticsearchDomainConfig_encryptionAtRestOptions = Lens.lens (\ElasticsearchDomainConfig' {encryptionAtRestOptions} -> encryptionAtRestOptions) (\s@ElasticsearchDomainConfig' {} a -> s {encryptionAtRestOptions = a} :: ElasticsearchDomainConfig)

-- | The @VPCOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains>.
elasticsearchDomainConfig_vPCOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe VPCDerivedInfoStatus)
elasticsearchDomainConfig_vPCOptions = Lens.lens (\ElasticsearchDomainConfig' {vPCOptions} -> vPCOptions) (\s@ElasticsearchDomainConfig' {} a -> s {vPCOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies the @DomainEndpointOptions@ for the Elasticsearch domain.
elasticsearchDomainConfig_domainEndpointOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe DomainEndpointOptionsStatus)
elasticsearchDomainConfig_domainEndpointOptions = Lens.lens (\ElasticsearchDomainConfig' {domainEndpointOptions} -> domainEndpointOptions) (\s@ElasticsearchDomainConfig' {} a -> s {domainEndpointOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies the @AdvancedOptions@ for the domain. See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options>
-- for more information.
elasticsearchDomainConfig_advancedOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe AdvancedOptionsStatus)
elasticsearchDomainConfig_advancedOptions = Lens.lens (\ElasticsearchDomainConfig' {advancedOptions} -> advancedOptions) (\s@ElasticsearchDomainConfig' {} a -> s {advancedOptions = a} :: ElasticsearchDomainConfig)

-- | String of format X.Y to specify version for the Elasticsearch domain.
elasticsearchDomainConfig_elasticsearchVersion :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe ElasticsearchVersionStatus)
elasticsearchDomainConfig_elasticsearchVersion = Lens.lens (\ElasticsearchDomainConfig' {elasticsearchVersion} -> elasticsearchVersion) (\s@ElasticsearchDomainConfig' {} a -> s {elasticsearchVersion = a} :: ElasticsearchDomainConfig)

instance Core.FromJSON ElasticsearchDomainConfig where
  parseJSON =
    Core.withObject
      "ElasticsearchDomainConfig"
      ( \x ->
          ElasticsearchDomainConfig'
            Prelude.<$> (x Core..:? "EBSOptions")
            Prelude.<*> (x Core..:? "NodeToNodeEncryptionOptions")
            Prelude.<*> (x Core..:? "AccessPolicies")
            Prelude.<*> (x Core..:? "AutoTuneOptions")
            Prelude.<*> (x Core..:? "LogPublishingOptions")
            Prelude.<*> (x Core..:? "AdvancedSecurityOptions")
            Prelude.<*> (x Core..:? "ElasticsearchClusterConfig")
            Prelude.<*> (x Core..:? "SnapshotOptions")
            Prelude.<*> (x Core..:? "CognitoOptions")
            Prelude.<*> (x Core..:? "EncryptionAtRestOptions")
            Prelude.<*> (x Core..:? "VPCOptions")
            Prelude.<*> (x Core..:? "DomainEndpointOptions")
            Prelude.<*> (x Core..:? "AdvancedOptions")
            Prelude.<*> (x Core..:? "ElasticsearchVersion")
      )

instance Prelude.Hashable ElasticsearchDomainConfig where
  hashWithSalt _salt ElasticsearchDomainConfig' {..} =
    _salt `Prelude.hashWithSalt` eBSOptions
      `Prelude.hashWithSalt` nodeToNodeEncryptionOptions
      `Prelude.hashWithSalt` accessPolicies
      `Prelude.hashWithSalt` autoTuneOptions
      `Prelude.hashWithSalt` logPublishingOptions
      `Prelude.hashWithSalt` advancedSecurityOptions
      `Prelude.hashWithSalt` elasticsearchClusterConfig
      `Prelude.hashWithSalt` snapshotOptions
      `Prelude.hashWithSalt` cognitoOptions
      `Prelude.hashWithSalt` encryptionAtRestOptions
      `Prelude.hashWithSalt` vPCOptions
      `Prelude.hashWithSalt` domainEndpointOptions
      `Prelude.hashWithSalt` advancedOptions
      `Prelude.hashWithSalt` elasticsearchVersion

instance Prelude.NFData ElasticsearchDomainConfig where
  rnf ElasticsearchDomainConfig' {..} =
    Prelude.rnf eBSOptions
      `Prelude.seq` Prelude.rnf nodeToNodeEncryptionOptions
      `Prelude.seq` Prelude.rnf accessPolicies
      `Prelude.seq` Prelude.rnf autoTuneOptions
      `Prelude.seq` Prelude.rnf logPublishingOptions
      `Prelude.seq` Prelude.rnf advancedSecurityOptions
      `Prelude.seq` Prelude.rnf elasticsearchClusterConfig
      `Prelude.seq` Prelude.rnf snapshotOptions
      `Prelude.seq` Prelude.rnf cognitoOptions
      `Prelude.seq` Prelude.rnf encryptionAtRestOptions
      `Prelude.seq` Prelude.rnf vPCOptions
      `Prelude.seq` Prelude.rnf domainEndpointOptions
      `Prelude.seq` Prelude.rnf advancedOptions
      `Prelude.seq` Prelude.rnf elasticsearchVersion
