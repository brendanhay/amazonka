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
-- Module      : Amazonka.OpenSearch.Types.DomainConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.DomainConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.AccessPoliciesStatus
import Amazonka.OpenSearch.Types.AdvancedOptionsStatus
import Amazonka.OpenSearch.Types.AdvancedSecurityOptionsStatus
import Amazonka.OpenSearch.Types.AutoTuneOptionsStatus
import Amazonka.OpenSearch.Types.ChangeProgressDetails
import Amazonka.OpenSearch.Types.ClusterConfigStatus
import Amazonka.OpenSearch.Types.CognitoOptionsStatus
import Amazonka.OpenSearch.Types.DomainEndpointOptionsStatus
import Amazonka.OpenSearch.Types.EBSOptionsStatus
import Amazonka.OpenSearch.Types.EncryptionAtRestOptionsStatus
import Amazonka.OpenSearch.Types.LogPublishingOptionsStatus
import Amazonka.OpenSearch.Types.NodeToNodeEncryptionOptionsStatus
import Amazonka.OpenSearch.Types.SnapshotOptionsStatus
import Amazonka.OpenSearch.Types.VPCDerivedInfoStatus
import Amazonka.OpenSearch.Types.VersionStatus
import qualified Amazonka.Prelude as Prelude

-- | Container for the configuration of an OpenSearch Service domain.
--
-- /See:/ 'newDomainConfig' smart constructor.
data DomainConfig = DomainConfig'
  { -- | Specifies the access policies for the domain.
    accessPolicies :: Prelude.Maybe AccessPoliciesStatus,
    -- | Key-value pairs to specify advanced configuration options. For more
    -- information, see
    -- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced options>.
    advancedOptions :: Prelude.Maybe AdvancedOptionsStatus,
    -- | Container for fine-grained access control settings for the domain.
    advancedSecurityOptions :: Prelude.Maybe AdvancedSecurityOptionsStatus,
    -- | Container for Auto-Tune settings for the domain.
    autoTuneOptions :: Prelude.Maybe AutoTuneOptionsStatus,
    -- | Container for information about the progress of an existing
    -- configuration change.
    changeProgressDetails :: Prelude.Maybe ChangeProgressDetails,
    -- | Container for the cluster configuration of a the domain.
    clusterConfig :: Prelude.Maybe ClusterConfigStatus,
    -- | Container for Amazon Cognito options for the domain.
    cognitoOptions :: Prelude.Maybe CognitoOptionsStatus,
    -- | Additional options for the domain endpoint, such as whether to require
    -- HTTPS for all traffic.
    domainEndpointOptions :: Prelude.Maybe DomainEndpointOptionsStatus,
    -- | Container for EBS options configured for an OpenSearch Service domain.
    eBSOptions :: Prelude.Maybe EBSOptionsStatus,
    -- | Key-value pairs to enable encryption at rest.
    encryptionAtRestOptions :: Prelude.Maybe EncryptionAtRestOptionsStatus,
    -- | The OpenSearch or Elasticsearch version that the domain is running.
    engineVersion :: Prelude.Maybe VersionStatus,
    -- | Key-value pairs to configure slow log publishing.
    logPublishingOptions :: Prelude.Maybe LogPublishingOptionsStatus,
    -- | Whether node-to-node encryption is enabled or disabled.
    nodeToNodeEncryptionOptions :: Prelude.Maybe NodeToNodeEncryptionOptionsStatus,
    -- | DEPRECATED. Container for parameters required to configure automated
    -- snapshots of domain indexes.
    snapshotOptions :: Prelude.Maybe SnapshotOptionsStatus,
    -- | The current VPC options for the domain and the status of any updates to
    -- their configuration.
    vPCOptions :: Prelude.Maybe VPCDerivedInfoStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessPolicies', 'domainConfig_accessPolicies' - Specifies the access policies for the domain.
--
-- 'advancedOptions', 'domainConfig_advancedOptions' - Key-value pairs to specify advanced configuration options. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced options>.
--
-- 'advancedSecurityOptions', 'domainConfig_advancedSecurityOptions' - Container for fine-grained access control settings for the domain.
--
-- 'autoTuneOptions', 'domainConfig_autoTuneOptions' - Container for Auto-Tune settings for the domain.
--
-- 'changeProgressDetails', 'domainConfig_changeProgressDetails' - Container for information about the progress of an existing
-- configuration change.
--
-- 'clusterConfig', 'domainConfig_clusterConfig' - Container for the cluster configuration of a the domain.
--
-- 'cognitoOptions', 'domainConfig_cognitoOptions' - Container for Amazon Cognito options for the domain.
--
-- 'domainEndpointOptions', 'domainConfig_domainEndpointOptions' - Additional options for the domain endpoint, such as whether to require
-- HTTPS for all traffic.
--
-- 'eBSOptions', 'domainConfig_eBSOptions' - Container for EBS options configured for an OpenSearch Service domain.
--
-- 'encryptionAtRestOptions', 'domainConfig_encryptionAtRestOptions' - Key-value pairs to enable encryption at rest.
--
-- 'engineVersion', 'domainConfig_engineVersion' - The OpenSearch or Elasticsearch version that the domain is running.
--
-- 'logPublishingOptions', 'domainConfig_logPublishingOptions' - Key-value pairs to configure slow log publishing.
--
-- 'nodeToNodeEncryptionOptions', 'domainConfig_nodeToNodeEncryptionOptions' - Whether node-to-node encryption is enabled or disabled.
--
-- 'snapshotOptions', 'domainConfig_snapshotOptions' - DEPRECATED. Container for parameters required to configure automated
-- snapshots of domain indexes.
--
-- 'vPCOptions', 'domainConfig_vPCOptions' - The current VPC options for the domain and the status of any updates to
-- their configuration.
newDomainConfig ::
  DomainConfig
newDomainConfig =
  DomainConfig'
    { accessPolicies = Prelude.Nothing,
      advancedOptions = Prelude.Nothing,
      advancedSecurityOptions = Prelude.Nothing,
      autoTuneOptions = Prelude.Nothing,
      changeProgressDetails = Prelude.Nothing,
      clusterConfig = Prelude.Nothing,
      cognitoOptions = Prelude.Nothing,
      domainEndpointOptions = Prelude.Nothing,
      eBSOptions = Prelude.Nothing,
      encryptionAtRestOptions = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      logPublishingOptions = Prelude.Nothing,
      nodeToNodeEncryptionOptions = Prelude.Nothing,
      snapshotOptions = Prelude.Nothing,
      vPCOptions = Prelude.Nothing
    }

-- | Specifies the access policies for the domain.
domainConfig_accessPolicies :: Lens.Lens' DomainConfig (Prelude.Maybe AccessPoliciesStatus)
domainConfig_accessPolicies = Lens.lens (\DomainConfig' {accessPolicies} -> accessPolicies) (\s@DomainConfig' {} a -> s {accessPolicies = a} :: DomainConfig)

-- | Key-value pairs to specify advanced configuration options. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced options>.
domainConfig_advancedOptions :: Lens.Lens' DomainConfig (Prelude.Maybe AdvancedOptionsStatus)
domainConfig_advancedOptions = Lens.lens (\DomainConfig' {advancedOptions} -> advancedOptions) (\s@DomainConfig' {} a -> s {advancedOptions = a} :: DomainConfig)

-- | Container for fine-grained access control settings for the domain.
domainConfig_advancedSecurityOptions :: Lens.Lens' DomainConfig (Prelude.Maybe AdvancedSecurityOptionsStatus)
domainConfig_advancedSecurityOptions = Lens.lens (\DomainConfig' {advancedSecurityOptions} -> advancedSecurityOptions) (\s@DomainConfig' {} a -> s {advancedSecurityOptions = a} :: DomainConfig)

-- | Container for Auto-Tune settings for the domain.
domainConfig_autoTuneOptions :: Lens.Lens' DomainConfig (Prelude.Maybe AutoTuneOptionsStatus)
domainConfig_autoTuneOptions = Lens.lens (\DomainConfig' {autoTuneOptions} -> autoTuneOptions) (\s@DomainConfig' {} a -> s {autoTuneOptions = a} :: DomainConfig)

-- | Container for information about the progress of an existing
-- configuration change.
domainConfig_changeProgressDetails :: Lens.Lens' DomainConfig (Prelude.Maybe ChangeProgressDetails)
domainConfig_changeProgressDetails = Lens.lens (\DomainConfig' {changeProgressDetails} -> changeProgressDetails) (\s@DomainConfig' {} a -> s {changeProgressDetails = a} :: DomainConfig)

-- | Container for the cluster configuration of a the domain.
domainConfig_clusterConfig :: Lens.Lens' DomainConfig (Prelude.Maybe ClusterConfigStatus)
domainConfig_clusterConfig = Lens.lens (\DomainConfig' {clusterConfig} -> clusterConfig) (\s@DomainConfig' {} a -> s {clusterConfig = a} :: DomainConfig)

-- | Container for Amazon Cognito options for the domain.
domainConfig_cognitoOptions :: Lens.Lens' DomainConfig (Prelude.Maybe CognitoOptionsStatus)
domainConfig_cognitoOptions = Lens.lens (\DomainConfig' {cognitoOptions} -> cognitoOptions) (\s@DomainConfig' {} a -> s {cognitoOptions = a} :: DomainConfig)

-- | Additional options for the domain endpoint, such as whether to require
-- HTTPS for all traffic.
domainConfig_domainEndpointOptions :: Lens.Lens' DomainConfig (Prelude.Maybe DomainEndpointOptionsStatus)
domainConfig_domainEndpointOptions = Lens.lens (\DomainConfig' {domainEndpointOptions} -> domainEndpointOptions) (\s@DomainConfig' {} a -> s {domainEndpointOptions = a} :: DomainConfig)

-- | Container for EBS options configured for an OpenSearch Service domain.
domainConfig_eBSOptions :: Lens.Lens' DomainConfig (Prelude.Maybe EBSOptionsStatus)
domainConfig_eBSOptions = Lens.lens (\DomainConfig' {eBSOptions} -> eBSOptions) (\s@DomainConfig' {} a -> s {eBSOptions = a} :: DomainConfig)

-- | Key-value pairs to enable encryption at rest.
domainConfig_encryptionAtRestOptions :: Lens.Lens' DomainConfig (Prelude.Maybe EncryptionAtRestOptionsStatus)
domainConfig_encryptionAtRestOptions = Lens.lens (\DomainConfig' {encryptionAtRestOptions} -> encryptionAtRestOptions) (\s@DomainConfig' {} a -> s {encryptionAtRestOptions = a} :: DomainConfig)

-- | The OpenSearch or Elasticsearch version that the domain is running.
domainConfig_engineVersion :: Lens.Lens' DomainConfig (Prelude.Maybe VersionStatus)
domainConfig_engineVersion = Lens.lens (\DomainConfig' {engineVersion} -> engineVersion) (\s@DomainConfig' {} a -> s {engineVersion = a} :: DomainConfig)

-- | Key-value pairs to configure slow log publishing.
domainConfig_logPublishingOptions :: Lens.Lens' DomainConfig (Prelude.Maybe LogPublishingOptionsStatus)
domainConfig_logPublishingOptions = Lens.lens (\DomainConfig' {logPublishingOptions} -> logPublishingOptions) (\s@DomainConfig' {} a -> s {logPublishingOptions = a} :: DomainConfig)

-- | Whether node-to-node encryption is enabled or disabled.
domainConfig_nodeToNodeEncryptionOptions :: Lens.Lens' DomainConfig (Prelude.Maybe NodeToNodeEncryptionOptionsStatus)
domainConfig_nodeToNodeEncryptionOptions = Lens.lens (\DomainConfig' {nodeToNodeEncryptionOptions} -> nodeToNodeEncryptionOptions) (\s@DomainConfig' {} a -> s {nodeToNodeEncryptionOptions = a} :: DomainConfig)

-- | DEPRECATED. Container for parameters required to configure automated
-- snapshots of domain indexes.
domainConfig_snapshotOptions :: Lens.Lens' DomainConfig (Prelude.Maybe SnapshotOptionsStatus)
domainConfig_snapshotOptions = Lens.lens (\DomainConfig' {snapshotOptions} -> snapshotOptions) (\s@DomainConfig' {} a -> s {snapshotOptions = a} :: DomainConfig)

-- | The current VPC options for the domain and the status of any updates to
-- their configuration.
domainConfig_vPCOptions :: Lens.Lens' DomainConfig (Prelude.Maybe VPCDerivedInfoStatus)
domainConfig_vPCOptions = Lens.lens (\DomainConfig' {vPCOptions} -> vPCOptions) (\s@DomainConfig' {} a -> s {vPCOptions = a} :: DomainConfig)

instance Data.FromJSON DomainConfig where
  parseJSON =
    Data.withObject
      "DomainConfig"
      ( \x ->
          DomainConfig'
            Prelude.<$> (x Data..:? "AccessPolicies")
            Prelude.<*> (x Data..:? "AdvancedOptions")
            Prelude.<*> (x Data..:? "AdvancedSecurityOptions")
            Prelude.<*> (x Data..:? "AutoTuneOptions")
            Prelude.<*> (x Data..:? "ChangeProgressDetails")
            Prelude.<*> (x Data..:? "ClusterConfig")
            Prelude.<*> (x Data..:? "CognitoOptions")
            Prelude.<*> (x Data..:? "DomainEndpointOptions")
            Prelude.<*> (x Data..:? "EBSOptions")
            Prelude.<*> (x Data..:? "EncryptionAtRestOptions")
            Prelude.<*> (x Data..:? "EngineVersion")
            Prelude.<*> (x Data..:? "LogPublishingOptions")
            Prelude.<*> (x Data..:? "NodeToNodeEncryptionOptions")
            Prelude.<*> (x Data..:? "SnapshotOptions")
            Prelude.<*> (x Data..:? "VPCOptions")
      )

instance Prelude.Hashable DomainConfig where
  hashWithSalt _salt DomainConfig' {..} =
    _salt
      `Prelude.hashWithSalt` accessPolicies
      `Prelude.hashWithSalt` advancedOptions
      `Prelude.hashWithSalt` advancedSecurityOptions
      `Prelude.hashWithSalt` autoTuneOptions
      `Prelude.hashWithSalt` changeProgressDetails
      `Prelude.hashWithSalt` clusterConfig
      `Prelude.hashWithSalt` cognitoOptions
      `Prelude.hashWithSalt` domainEndpointOptions
      `Prelude.hashWithSalt` eBSOptions
      `Prelude.hashWithSalt` encryptionAtRestOptions
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` logPublishingOptions
      `Prelude.hashWithSalt` nodeToNodeEncryptionOptions
      `Prelude.hashWithSalt` snapshotOptions
      `Prelude.hashWithSalt` vPCOptions

instance Prelude.NFData DomainConfig where
  rnf DomainConfig' {..} =
    Prelude.rnf accessPolicies
      `Prelude.seq` Prelude.rnf advancedOptions
      `Prelude.seq` Prelude.rnf advancedSecurityOptions
      `Prelude.seq` Prelude.rnf autoTuneOptions
      `Prelude.seq` Prelude.rnf changeProgressDetails
      `Prelude.seq` Prelude.rnf clusterConfig
      `Prelude.seq` Prelude.rnf cognitoOptions
      `Prelude.seq` Prelude.rnf domainEndpointOptions
      `Prelude.seq` Prelude.rnf eBSOptions
      `Prelude.seq` Prelude.rnf encryptionAtRestOptions
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf logPublishingOptions
      `Prelude.seq` Prelude.rnf nodeToNodeEncryptionOptions
      `Prelude.seq` Prelude.rnf snapshotOptions
      `Prelude.seq` Prelude.rnf vPCOptions
