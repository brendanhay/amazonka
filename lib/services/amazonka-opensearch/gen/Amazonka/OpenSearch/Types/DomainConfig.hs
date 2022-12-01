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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.DomainConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
  { -- | Whether node-to-node encryption is enabled or disabled.
    nodeToNodeEncryptionOptions :: Prelude.Maybe NodeToNodeEncryptionOptionsStatus,
    -- | Container for the cluster configuration of a the domain.
    clusterConfig :: Prelude.Maybe ClusterConfigStatus,
    -- | Key-value pairs to specify advanced configuration options. For more
    -- information, see
    -- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced options>.
    advancedOptions :: Prelude.Maybe AdvancedOptionsStatus,
    -- | Container for information about the progress of an existing
    -- configuration change.
    changeProgressDetails :: Prelude.Maybe ChangeProgressDetails,
    -- | Container for fine-grained access control settings for the domain.
    advancedSecurityOptions :: Prelude.Maybe AdvancedSecurityOptionsStatus,
    -- | Container for Amazon Cognito options for the domain.
    cognitoOptions :: Prelude.Maybe CognitoOptionsStatus,
    -- | Key-value pairs to enable encryption at rest.
    encryptionAtRestOptions :: Prelude.Maybe EncryptionAtRestOptionsStatus,
    -- | Container for EBS options configured for an OpenSearch Service domain.
    eBSOptions :: Prelude.Maybe EBSOptionsStatus,
    -- | Specifies the access policies for the domain.
    accessPolicies :: Prelude.Maybe AccessPoliciesStatus,
    -- | The current VPC options for the domain and the status of any updates to
    -- their configuration.
    vPCOptions :: Prelude.Maybe VPCDerivedInfoStatus,
    -- | Container for Auto-Tune settings for the domain.
    autoTuneOptions :: Prelude.Maybe AutoTuneOptionsStatus,
    -- | Additional options for the domain endpoint, such as whether to require
    -- HTTPS for all traffic.
    domainEndpointOptions :: Prelude.Maybe DomainEndpointOptionsStatus,
    -- | DEPRECATED. Container for parameters required to configure automated
    -- snapshots of domain indexes.
    snapshotOptions :: Prelude.Maybe SnapshotOptionsStatus,
    -- | Key-value pairs to configure slow log publishing.
    logPublishingOptions :: Prelude.Maybe LogPublishingOptionsStatus,
    -- | The OpenSearch or Elasticsearch version that the domain is running.
    engineVersion :: Prelude.Maybe VersionStatus
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
-- 'nodeToNodeEncryptionOptions', 'domainConfig_nodeToNodeEncryptionOptions' - Whether node-to-node encryption is enabled or disabled.
--
-- 'clusterConfig', 'domainConfig_clusterConfig' - Container for the cluster configuration of a the domain.
--
-- 'advancedOptions', 'domainConfig_advancedOptions' - Key-value pairs to specify advanced configuration options. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced options>.
--
-- 'changeProgressDetails', 'domainConfig_changeProgressDetails' - Container for information about the progress of an existing
-- configuration change.
--
-- 'advancedSecurityOptions', 'domainConfig_advancedSecurityOptions' - Container for fine-grained access control settings for the domain.
--
-- 'cognitoOptions', 'domainConfig_cognitoOptions' - Container for Amazon Cognito options for the domain.
--
-- 'encryptionAtRestOptions', 'domainConfig_encryptionAtRestOptions' - Key-value pairs to enable encryption at rest.
--
-- 'eBSOptions', 'domainConfig_eBSOptions' - Container for EBS options configured for an OpenSearch Service domain.
--
-- 'accessPolicies', 'domainConfig_accessPolicies' - Specifies the access policies for the domain.
--
-- 'vPCOptions', 'domainConfig_vPCOptions' - The current VPC options for the domain and the status of any updates to
-- their configuration.
--
-- 'autoTuneOptions', 'domainConfig_autoTuneOptions' - Container for Auto-Tune settings for the domain.
--
-- 'domainEndpointOptions', 'domainConfig_domainEndpointOptions' - Additional options for the domain endpoint, such as whether to require
-- HTTPS for all traffic.
--
-- 'snapshotOptions', 'domainConfig_snapshotOptions' - DEPRECATED. Container for parameters required to configure automated
-- snapshots of domain indexes.
--
-- 'logPublishingOptions', 'domainConfig_logPublishingOptions' - Key-value pairs to configure slow log publishing.
--
-- 'engineVersion', 'domainConfig_engineVersion' - The OpenSearch or Elasticsearch version that the domain is running.
newDomainConfig ::
  DomainConfig
newDomainConfig =
  DomainConfig'
    { nodeToNodeEncryptionOptions =
        Prelude.Nothing,
      clusterConfig = Prelude.Nothing,
      advancedOptions = Prelude.Nothing,
      changeProgressDetails = Prelude.Nothing,
      advancedSecurityOptions = Prelude.Nothing,
      cognitoOptions = Prelude.Nothing,
      encryptionAtRestOptions = Prelude.Nothing,
      eBSOptions = Prelude.Nothing,
      accessPolicies = Prelude.Nothing,
      vPCOptions = Prelude.Nothing,
      autoTuneOptions = Prelude.Nothing,
      domainEndpointOptions = Prelude.Nothing,
      snapshotOptions = Prelude.Nothing,
      logPublishingOptions = Prelude.Nothing,
      engineVersion = Prelude.Nothing
    }

-- | Whether node-to-node encryption is enabled or disabled.
domainConfig_nodeToNodeEncryptionOptions :: Lens.Lens' DomainConfig (Prelude.Maybe NodeToNodeEncryptionOptionsStatus)
domainConfig_nodeToNodeEncryptionOptions = Lens.lens (\DomainConfig' {nodeToNodeEncryptionOptions} -> nodeToNodeEncryptionOptions) (\s@DomainConfig' {} a -> s {nodeToNodeEncryptionOptions = a} :: DomainConfig)

-- | Container for the cluster configuration of a the domain.
domainConfig_clusterConfig :: Lens.Lens' DomainConfig (Prelude.Maybe ClusterConfigStatus)
domainConfig_clusterConfig = Lens.lens (\DomainConfig' {clusterConfig} -> clusterConfig) (\s@DomainConfig' {} a -> s {clusterConfig = a} :: DomainConfig)

-- | Key-value pairs to specify advanced configuration options. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced options>.
domainConfig_advancedOptions :: Lens.Lens' DomainConfig (Prelude.Maybe AdvancedOptionsStatus)
domainConfig_advancedOptions = Lens.lens (\DomainConfig' {advancedOptions} -> advancedOptions) (\s@DomainConfig' {} a -> s {advancedOptions = a} :: DomainConfig)

-- | Container for information about the progress of an existing
-- configuration change.
domainConfig_changeProgressDetails :: Lens.Lens' DomainConfig (Prelude.Maybe ChangeProgressDetails)
domainConfig_changeProgressDetails = Lens.lens (\DomainConfig' {changeProgressDetails} -> changeProgressDetails) (\s@DomainConfig' {} a -> s {changeProgressDetails = a} :: DomainConfig)

-- | Container for fine-grained access control settings for the domain.
domainConfig_advancedSecurityOptions :: Lens.Lens' DomainConfig (Prelude.Maybe AdvancedSecurityOptionsStatus)
domainConfig_advancedSecurityOptions = Lens.lens (\DomainConfig' {advancedSecurityOptions} -> advancedSecurityOptions) (\s@DomainConfig' {} a -> s {advancedSecurityOptions = a} :: DomainConfig)

-- | Container for Amazon Cognito options for the domain.
domainConfig_cognitoOptions :: Lens.Lens' DomainConfig (Prelude.Maybe CognitoOptionsStatus)
domainConfig_cognitoOptions = Lens.lens (\DomainConfig' {cognitoOptions} -> cognitoOptions) (\s@DomainConfig' {} a -> s {cognitoOptions = a} :: DomainConfig)

-- | Key-value pairs to enable encryption at rest.
domainConfig_encryptionAtRestOptions :: Lens.Lens' DomainConfig (Prelude.Maybe EncryptionAtRestOptionsStatus)
domainConfig_encryptionAtRestOptions = Lens.lens (\DomainConfig' {encryptionAtRestOptions} -> encryptionAtRestOptions) (\s@DomainConfig' {} a -> s {encryptionAtRestOptions = a} :: DomainConfig)

-- | Container for EBS options configured for an OpenSearch Service domain.
domainConfig_eBSOptions :: Lens.Lens' DomainConfig (Prelude.Maybe EBSOptionsStatus)
domainConfig_eBSOptions = Lens.lens (\DomainConfig' {eBSOptions} -> eBSOptions) (\s@DomainConfig' {} a -> s {eBSOptions = a} :: DomainConfig)

-- | Specifies the access policies for the domain.
domainConfig_accessPolicies :: Lens.Lens' DomainConfig (Prelude.Maybe AccessPoliciesStatus)
domainConfig_accessPolicies = Lens.lens (\DomainConfig' {accessPolicies} -> accessPolicies) (\s@DomainConfig' {} a -> s {accessPolicies = a} :: DomainConfig)

-- | The current VPC options for the domain and the status of any updates to
-- their configuration.
domainConfig_vPCOptions :: Lens.Lens' DomainConfig (Prelude.Maybe VPCDerivedInfoStatus)
domainConfig_vPCOptions = Lens.lens (\DomainConfig' {vPCOptions} -> vPCOptions) (\s@DomainConfig' {} a -> s {vPCOptions = a} :: DomainConfig)

-- | Container for Auto-Tune settings for the domain.
domainConfig_autoTuneOptions :: Lens.Lens' DomainConfig (Prelude.Maybe AutoTuneOptionsStatus)
domainConfig_autoTuneOptions = Lens.lens (\DomainConfig' {autoTuneOptions} -> autoTuneOptions) (\s@DomainConfig' {} a -> s {autoTuneOptions = a} :: DomainConfig)

-- | Additional options for the domain endpoint, such as whether to require
-- HTTPS for all traffic.
domainConfig_domainEndpointOptions :: Lens.Lens' DomainConfig (Prelude.Maybe DomainEndpointOptionsStatus)
domainConfig_domainEndpointOptions = Lens.lens (\DomainConfig' {domainEndpointOptions} -> domainEndpointOptions) (\s@DomainConfig' {} a -> s {domainEndpointOptions = a} :: DomainConfig)

-- | DEPRECATED. Container for parameters required to configure automated
-- snapshots of domain indexes.
domainConfig_snapshotOptions :: Lens.Lens' DomainConfig (Prelude.Maybe SnapshotOptionsStatus)
domainConfig_snapshotOptions = Lens.lens (\DomainConfig' {snapshotOptions} -> snapshotOptions) (\s@DomainConfig' {} a -> s {snapshotOptions = a} :: DomainConfig)

-- | Key-value pairs to configure slow log publishing.
domainConfig_logPublishingOptions :: Lens.Lens' DomainConfig (Prelude.Maybe LogPublishingOptionsStatus)
domainConfig_logPublishingOptions = Lens.lens (\DomainConfig' {logPublishingOptions} -> logPublishingOptions) (\s@DomainConfig' {} a -> s {logPublishingOptions = a} :: DomainConfig)

-- | The OpenSearch or Elasticsearch version that the domain is running.
domainConfig_engineVersion :: Lens.Lens' DomainConfig (Prelude.Maybe VersionStatus)
domainConfig_engineVersion = Lens.lens (\DomainConfig' {engineVersion} -> engineVersion) (\s@DomainConfig' {} a -> s {engineVersion = a} :: DomainConfig)

instance Core.FromJSON DomainConfig where
  parseJSON =
    Core.withObject
      "DomainConfig"
      ( \x ->
          DomainConfig'
            Prelude.<$> (x Core..:? "NodeToNodeEncryptionOptions")
            Prelude.<*> (x Core..:? "ClusterConfig")
            Prelude.<*> (x Core..:? "AdvancedOptions")
            Prelude.<*> (x Core..:? "ChangeProgressDetails")
            Prelude.<*> (x Core..:? "AdvancedSecurityOptions")
            Prelude.<*> (x Core..:? "CognitoOptions")
            Prelude.<*> (x Core..:? "EncryptionAtRestOptions")
            Prelude.<*> (x Core..:? "EBSOptions")
            Prelude.<*> (x Core..:? "AccessPolicies")
            Prelude.<*> (x Core..:? "VPCOptions")
            Prelude.<*> (x Core..:? "AutoTuneOptions")
            Prelude.<*> (x Core..:? "DomainEndpointOptions")
            Prelude.<*> (x Core..:? "SnapshotOptions")
            Prelude.<*> (x Core..:? "LogPublishingOptions")
            Prelude.<*> (x Core..:? "EngineVersion")
      )

instance Prelude.Hashable DomainConfig where
  hashWithSalt _salt DomainConfig' {..} =
    _salt
      `Prelude.hashWithSalt` nodeToNodeEncryptionOptions
      `Prelude.hashWithSalt` clusterConfig
      `Prelude.hashWithSalt` advancedOptions
      `Prelude.hashWithSalt` changeProgressDetails
      `Prelude.hashWithSalt` advancedSecurityOptions
      `Prelude.hashWithSalt` cognitoOptions
      `Prelude.hashWithSalt` encryptionAtRestOptions
      `Prelude.hashWithSalt` eBSOptions
      `Prelude.hashWithSalt` accessPolicies
      `Prelude.hashWithSalt` vPCOptions
      `Prelude.hashWithSalt` autoTuneOptions
      `Prelude.hashWithSalt` domainEndpointOptions
      `Prelude.hashWithSalt` snapshotOptions
      `Prelude.hashWithSalt` logPublishingOptions
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData DomainConfig where
  rnf DomainConfig' {..} =
    Prelude.rnf nodeToNodeEncryptionOptions
      `Prelude.seq` Prelude.rnf clusterConfig
      `Prelude.seq` Prelude.rnf advancedOptions
      `Prelude.seq` Prelude.rnf changeProgressDetails
      `Prelude.seq` Prelude.rnf advancedSecurityOptions
      `Prelude.seq` Prelude.rnf cognitoOptions
      `Prelude.seq` Prelude.rnf encryptionAtRestOptions
      `Prelude.seq` Prelude.rnf eBSOptions
      `Prelude.seq` Prelude.rnf accessPolicies
      `Prelude.seq` Prelude.rnf vPCOptions
      `Prelude.seq` Prelude.rnf autoTuneOptions
      `Prelude.seq` Prelude.rnf domainEndpointOptions
      `Prelude.seq` Prelude.rnf snapshotOptions
      `Prelude.seq` Prelude.rnf logPublishingOptions
      `Prelude.seq` Prelude.rnf engineVersion
