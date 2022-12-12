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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.ElasticsearchDomainConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.AccessPoliciesStatus
import Amazonka.ElasticSearch.Types.AdvancedOptionsStatus
import Amazonka.ElasticSearch.Types.AdvancedSecurityOptionsStatus
import Amazonka.ElasticSearch.Types.AutoTuneOptionsStatus
import Amazonka.ElasticSearch.Types.ChangeProgressDetails
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
import qualified Amazonka.Prelude as Prelude

-- | The configuration of an Elasticsearch domain.
--
-- /See:/ 'newElasticsearchDomainConfig' smart constructor.
data ElasticsearchDomainConfig = ElasticsearchDomainConfig'
  { -- | IAM access policy as a JSON-formatted string.
    accessPolicies :: Prelude.Maybe AccessPoliciesStatus,
    -- | Specifies the @AdvancedOptions@ for the domain. See
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options>
    -- for more information.
    advancedOptions :: Prelude.Maybe AdvancedOptionsStatus,
    -- | Specifies @AdvancedSecurityOptions@ for the domain.
    advancedSecurityOptions :: Prelude.Maybe AdvancedSecurityOptionsStatus,
    -- | Specifies @AutoTuneOptions@ for the domain.
    autoTuneOptions :: Prelude.Maybe AutoTuneOptionsStatus,
    -- | Specifies change details of the domain configuration change.
    changeProgressDetails :: Prelude.Maybe ChangeProgressDetails,
    -- | The @CognitoOptions@ for the specified domain. For more information, see
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
    cognitoOptions :: Prelude.Maybe CognitoOptionsStatus,
    -- | Specifies the @DomainEndpointOptions@ for the Elasticsearch domain.
    domainEndpointOptions :: Prelude.Maybe DomainEndpointOptionsStatus,
    -- | Specifies the @EBSOptions@ for the Elasticsearch domain.
    eBSOptions :: Prelude.Maybe EBSOptionsStatus,
    -- | Specifies the @ElasticsearchClusterConfig@ for the Elasticsearch domain.
    elasticsearchClusterConfig :: Prelude.Maybe ElasticsearchClusterConfigStatus,
    -- | String of format X.Y to specify version for the Elasticsearch domain.
    elasticsearchVersion :: Prelude.Maybe ElasticsearchVersionStatus,
    -- | Specifies the @EncryptionAtRestOptions@ for the Elasticsearch domain.
    encryptionAtRestOptions :: Prelude.Maybe EncryptionAtRestOptionsStatus,
    -- | Log publishing options for the given domain.
    logPublishingOptions :: Prelude.Maybe LogPublishingOptionsStatus,
    -- | Specifies the @NodeToNodeEncryptionOptions@ for the Elasticsearch
    -- domain.
    nodeToNodeEncryptionOptions :: Prelude.Maybe NodeToNodeEncryptionOptionsStatus,
    -- | Specifies the @SnapshotOptions@ for the Elasticsearch domain.
    snapshotOptions :: Prelude.Maybe SnapshotOptionsStatus,
    -- | The @VPCOptions@ for the specified domain. For more information, see
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains>.
    vPCOptions :: Prelude.Maybe VPCDerivedInfoStatus
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
-- 'accessPolicies', 'elasticsearchDomainConfig_accessPolicies' - IAM access policy as a JSON-formatted string.
--
-- 'advancedOptions', 'elasticsearchDomainConfig_advancedOptions' - Specifies the @AdvancedOptions@ for the domain. See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options>
-- for more information.
--
-- 'advancedSecurityOptions', 'elasticsearchDomainConfig_advancedSecurityOptions' - Specifies @AdvancedSecurityOptions@ for the domain.
--
-- 'autoTuneOptions', 'elasticsearchDomainConfig_autoTuneOptions' - Specifies @AutoTuneOptions@ for the domain.
--
-- 'changeProgressDetails', 'elasticsearchDomainConfig_changeProgressDetails' - Specifies change details of the domain configuration change.
--
-- 'cognitoOptions', 'elasticsearchDomainConfig_cognitoOptions' - The @CognitoOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
--
-- 'domainEndpointOptions', 'elasticsearchDomainConfig_domainEndpointOptions' - Specifies the @DomainEndpointOptions@ for the Elasticsearch domain.
--
-- 'eBSOptions', 'elasticsearchDomainConfig_eBSOptions' - Specifies the @EBSOptions@ for the Elasticsearch domain.
--
-- 'elasticsearchClusterConfig', 'elasticsearchDomainConfig_elasticsearchClusterConfig' - Specifies the @ElasticsearchClusterConfig@ for the Elasticsearch domain.
--
-- 'elasticsearchVersion', 'elasticsearchDomainConfig_elasticsearchVersion' - String of format X.Y to specify version for the Elasticsearch domain.
--
-- 'encryptionAtRestOptions', 'elasticsearchDomainConfig_encryptionAtRestOptions' - Specifies the @EncryptionAtRestOptions@ for the Elasticsearch domain.
--
-- 'logPublishingOptions', 'elasticsearchDomainConfig_logPublishingOptions' - Log publishing options for the given domain.
--
-- 'nodeToNodeEncryptionOptions', 'elasticsearchDomainConfig_nodeToNodeEncryptionOptions' - Specifies the @NodeToNodeEncryptionOptions@ for the Elasticsearch
-- domain.
--
-- 'snapshotOptions', 'elasticsearchDomainConfig_snapshotOptions' - Specifies the @SnapshotOptions@ for the Elasticsearch domain.
--
-- 'vPCOptions', 'elasticsearchDomainConfig_vPCOptions' - The @VPCOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains>.
newElasticsearchDomainConfig ::
  ElasticsearchDomainConfig
newElasticsearchDomainConfig =
  ElasticsearchDomainConfig'
    { accessPolicies =
        Prelude.Nothing,
      advancedOptions = Prelude.Nothing,
      advancedSecurityOptions = Prelude.Nothing,
      autoTuneOptions = Prelude.Nothing,
      changeProgressDetails = Prelude.Nothing,
      cognitoOptions = Prelude.Nothing,
      domainEndpointOptions = Prelude.Nothing,
      eBSOptions = Prelude.Nothing,
      elasticsearchClusterConfig = Prelude.Nothing,
      elasticsearchVersion = Prelude.Nothing,
      encryptionAtRestOptions = Prelude.Nothing,
      logPublishingOptions = Prelude.Nothing,
      nodeToNodeEncryptionOptions = Prelude.Nothing,
      snapshotOptions = Prelude.Nothing,
      vPCOptions = Prelude.Nothing
    }

-- | IAM access policy as a JSON-formatted string.
elasticsearchDomainConfig_accessPolicies :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe AccessPoliciesStatus)
elasticsearchDomainConfig_accessPolicies = Lens.lens (\ElasticsearchDomainConfig' {accessPolicies} -> accessPolicies) (\s@ElasticsearchDomainConfig' {} a -> s {accessPolicies = a} :: ElasticsearchDomainConfig)

-- | Specifies the @AdvancedOptions@ for the domain. See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options>
-- for more information.
elasticsearchDomainConfig_advancedOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe AdvancedOptionsStatus)
elasticsearchDomainConfig_advancedOptions = Lens.lens (\ElasticsearchDomainConfig' {advancedOptions} -> advancedOptions) (\s@ElasticsearchDomainConfig' {} a -> s {advancedOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies @AdvancedSecurityOptions@ for the domain.
elasticsearchDomainConfig_advancedSecurityOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe AdvancedSecurityOptionsStatus)
elasticsearchDomainConfig_advancedSecurityOptions = Lens.lens (\ElasticsearchDomainConfig' {advancedSecurityOptions} -> advancedSecurityOptions) (\s@ElasticsearchDomainConfig' {} a -> s {advancedSecurityOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies @AutoTuneOptions@ for the domain.
elasticsearchDomainConfig_autoTuneOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe AutoTuneOptionsStatus)
elasticsearchDomainConfig_autoTuneOptions = Lens.lens (\ElasticsearchDomainConfig' {autoTuneOptions} -> autoTuneOptions) (\s@ElasticsearchDomainConfig' {} a -> s {autoTuneOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies change details of the domain configuration change.
elasticsearchDomainConfig_changeProgressDetails :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe ChangeProgressDetails)
elasticsearchDomainConfig_changeProgressDetails = Lens.lens (\ElasticsearchDomainConfig' {changeProgressDetails} -> changeProgressDetails) (\s@ElasticsearchDomainConfig' {} a -> s {changeProgressDetails = a} :: ElasticsearchDomainConfig)

-- | The @CognitoOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
elasticsearchDomainConfig_cognitoOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe CognitoOptionsStatus)
elasticsearchDomainConfig_cognitoOptions = Lens.lens (\ElasticsearchDomainConfig' {cognitoOptions} -> cognitoOptions) (\s@ElasticsearchDomainConfig' {} a -> s {cognitoOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies the @DomainEndpointOptions@ for the Elasticsearch domain.
elasticsearchDomainConfig_domainEndpointOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe DomainEndpointOptionsStatus)
elasticsearchDomainConfig_domainEndpointOptions = Lens.lens (\ElasticsearchDomainConfig' {domainEndpointOptions} -> domainEndpointOptions) (\s@ElasticsearchDomainConfig' {} a -> s {domainEndpointOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies the @EBSOptions@ for the Elasticsearch domain.
elasticsearchDomainConfig_eBSOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe EBSOptionsStatus)
elasticsearchDomainConfig_eBSOptions = Lens.lens (\ElasticsearchDomainConfig' {eBSOptions} -> eBSOptions) (\s@ElasticsearchDomainConfig' {} a -> s {eBSOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies the @ElasticsearchClusterConfig@ for the Elasticsearch domain.
elasticsearchDomainConfig_elasticsearchClusterConfig :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe ElasticsearchClusterConfigStatus)
elasticsearchDomainConfig_elasticsearchClusterConfig = Lens.lens (\ElasticsearchDomainConfig' {elasticsearchClusterConfig} -> elasticsearchClusterConfig) (\s@ElasticsearchDomainConfig' {} a -> s {elasticsearchClusterConfig = a} :: ElasticsearchDomainConfig)

-- | String of format X.Y to specify version for the Elasticsearch domain.
elasticsearchDomainConfig_elasticsearchVersion :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe ElasticsearchVersionStatus)
elasticsearchDomainConfig_elasticsearchVersion = Lens.lens (\ElasticsearchDomainConfig' {elasticsearchVersion} -> elasticsearchVersion) (\s@ElasticsearchDomainConfig' {} a -> s {elasticsearchVersion = a} :: ElasticsearchDomainConfig)

-- | Specifies the @EncryptionAtRestOptions@ for the Elasticsearch domain.
elasticsearchDomainConfig_encryptionAtRestOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe EncryptionAtRestOptionsStatus)
elasticsearchDomainConfig_encryptionAtRestOptions = Lens.lens (\ElasticsearchDomainConfig' {encryptionAtRestOptions} -> encryptionAtRestOptions) (\s@ElasticsearchDomainConfig' {} a -> s {encryptionAtRestOptions = a} :: ElasticsearchDomainConfig)

-- | Log publishing options for the given domain.
elasticsearchDomainConfig_logPublishingOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe LogPublishingOptionsStatus)
elasticsearchDomainConfig_logPublishingOptions = Lens.lens (\ElasticsearchDomainConfig' {logPublishingOptions} -> logPublishingOptions) (\s@ElasticsearchDomainConfig' {} a -> s {logPublishingOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies the @NodeToNodeEncryptionOptions@ for the Elasticsearch
-- domain.
elasticsearchDomainConfig_nodeToNodeEncryptionOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe NodeToNodeEncryptionOptionsStatus)
elasticsearchDomainConfig_nodeToNodeEncryptionOptions = Lens.lens (\ElasticsearchDomainConfig' {nodeToNodeEncryptionOptions} -> nodeToNodeEncryptionOptions) (\s@ElasticsearchDomainConfig' {} a -> s {nodeToNodeEncryptionOptions = a} :: ElasticsearchDomainConfig)

-- | Specifies the @SnapshotOptions@ for the Elasticsearch domain.
elasticsearchDomainConfig_snapshotOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe SnapshotOptionsStatus)
elasticsearchDomainConfig_snapshotOptions = Lens.lens (\ElasticsearchDomainConfig' {snapshotOptions} -> snapshotOptions) (\s@ElasticsearchDomainConfig' {} a -> s {snapshotOptions = a} :: ElasticsearchDomainConfig)

-- | The @VPCOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains>.
elasticsearchDomainConfig_vPCOptions :: Lens.Lens' ElasticsearchDomainConfig (Prelude.Maybe VPCDerivedInfoStatus)
elasticsearchDomainConfig_vPCOptions = Lens.lens (\ElasticsearchDomainConfig' {vPCOptions} -> vPCOptions) (\s@ElasticsearchDomainConfig' {} a -> s {vPCOptions = a} :: ElasticsearchDomainConfig)

instance Data.FromJSON ElasticsearchDomainConfig where
  parseJSON =
    Data.withObject
      "ElasticsearchDomainConfig"
      ( \x ->
          ElasticsearchDomainConfig'
            Prelude.<$> (x Data..:? "AccessPolicies")
            Prelude.<*> (x Data..:? "AdvancedOptions")
            Prelude.<*> (x Data..:? "AdvancedSecurityOptions")
            Prelude.<*> (x Data..:? "AutoTuneOptions")
            Prelude.<*> (x Data..:? "ChangeProgressDetails")
            Prelude.<*> (x Data..:? "CognitoOptions")
            Prelude.<*> (x Data..:? "DomainEndpointOptions")
            Prelude.<*> (x Data..:? "EBSOptions")
            Prelude.<*> (x Data..:? "ElasticsearchClusterConfig")
            Prelude.<*> (x Data..:? "ElasticsearchVersion")
            Prelude.<*> (x Data..:? "EncryptionAtRestOptions")
            Prelude.<*> (x Data..:? "LogPublishingOptions")
            Prelude.<*> (x Data..:? "NodeToNodeEncryptionOptions")
            Prelude.<*> (x Data..:? "SnapshotOptions")
            Prelude.<*> (x Data..:? "VPCOptions")
      )

instance Prelude.Hashable ElasticsearchDomainConfig where
  hashWithSalt _salt ElasticsearchDomainConfig' {..} =
    _salt `Prelude.hashWithSalt` accessPolicies
      `Prelude.hashWithSalt` advancedOptions
      `Prelude.hashWithSalt` advancedSecurityOptions
      `Prelude.hashWithSalt` autoTuneOptions
      `Prelude.hashWithSalt` changeProgressDetails
      `Prelude.hashWithSalt` cognitoOptions
      `Prelude.hashWithSalt` domainEndpointOptions
      `Prelude.hashWithSalt` eBSOptions
      `Prelude.hashWithSalt` elasticsearchClusterConfig
      `Prelude.hashWithSalt` elasticsearchVersion
      `Prelude.hashWithSalt` encryptionAtRestOptions
      `Prelude.hashWithSalt` logPublishingOptions
      `Prelude.hashWithSalt` nodeToNodeEncryptionOptions
      `Prelude.hashWithSalt` snapshotOptions
      `Prelude.hashWithSalt` vPCOptions

instance Prelude.NFData ElasticsearchDomainConfig where
  rnf ElasticsearchDomainConfig' {..} =
    Prelude.rnf accessPolicies
      `Prelude.seq` Prelude.rnf advancedOptions
      `Prelude.seq` Prelude.rnf advancedSecurityOptions
      `Prelude.seq` Prelude.rnf autoTuneOptions
      `Prelude.seq` Prelude.rnf changeProgressDetails
      `Prelude.seq` Prelude.rnf cognitoOptions
      `Prelude.seq` Prelude.rnf domainEndpointOptions
      `Prelude.seq` Prelude.rnf eBSOptions
      `Prelude.seq` Prelude.rnf elasticsearchClusterConfig
      `Prelude.seq` Prelude.rnf elasticsearchVersion
      `Prelude.seq` Prelude.rnf encryptionAtRestOptions
      `Prelude.seq` Prelude.rnf logPublishingOptions
      `Prelude.seq` Prelude.rnf nodeToNodeEncryptionOptions
      `Prelude.seq` Prelude.rnf snapshotOptions
      `Prelude.seq` Prelude.rnf vPCOptions
