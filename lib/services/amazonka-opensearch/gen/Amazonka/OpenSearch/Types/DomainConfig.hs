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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.DomainConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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

-- | The configuration of a domain.
--
-- /See:/ 'newDomainConfig' smart constructor.
data DomainConfig = DomainConfig'
  { -- | The @NodeToNodeEncryptionOptions@ for the domain.
    nodeToNodeEncryptionOptions :: Prelude.Maybe NodeToNodeEncryptionOptionsStatus,
    -- | The @ClusterConfig@ for the domain.
    clusterConfig :: Prelude.Maybe ClusterConfigStatus,
    -- | The @AdvancedOptions@ for the domain. See
    -- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced options>
    -- for more information.
    advancedOptions :: Prelude.Maybe AdvancedOptionsStatus,
    -- | Specifies change details of the domain configuration change.
    changeProgressDetails :: Prelude.Maybe ChangeProgressDetails,
    -- | Specifies @AdvancedSecurityOptions@ for the domain.
    advancedSecurityOptions :: Prelude.Maybe AdvancedSecurityOptionsStatus,
    -- | The @CognitoOptions@ for the specified domain. For more information, see
    -- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/cognito-auth.html Configuring Amazon Cognito authentication for OpenSearch Dashboards>.
    cognitoOptions :: Prelude.Maybe CognitoOptionsStatus,
    -- | The @EncryptionAtRestOptions@ for the domain.
    encryptionAtRestOptions :: Prelude.Maybe EncryptionAtRestOptionsStatus,
    -- | The @EBSOptions@ for the domain.
    eBSOptions :: Prelude.Maybe EBSOptionsStatus,
    -- | IAM access policy as a JSON-formatted string.
    accessPolicies :: Prelude.Maybe AccessPoliciesStatus,
    -- | The @VPCOptions@ for the specified domain. For more information, see
    -- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>.
    vPCOptions :: Prelude.Maybe VPCDerivedInfoStatus,
    -- | Specifies @AutoTuneOptions@ for the domain.
    autoTuneOptions :: Prelude.Maybe AutoTuneOptionsStatus,
    -- | The @DomainEndpointOptions@ for the domain.
    domainEndpointOptions :: Prelude.Maybe DomainEndpointOptionsStatus,
    -- | The @SnapshotOptions@ for the domain.
    snapshotOptions :: Prelude.Maybe SnapshotOptionsStatus,
    -- | Log publishing options for the given domain.
    logPublishingOptions :: Prelude.Maybe LogPublishingOptionsStatus,
    -- | String of format Elasticsearch_X.Y or OpenSearch_X.Y to specify the
    -- engine version for the OpenSearch or Elasticsearch domain.
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
-- 'nodeToNodeEncryptionOptions', 'domainConfig_nodeToNodeEncryptionOptions' - The @NodeToNodeEncryptionOptions@ for the domain.
--
-- 'clusterConfig', 'domainConfig_clusterConfig' - The @ClusterConfig@ for the domain.
--
-- 'advancedOptions', 'domainConfig_advancedOptions' - The @AdvancedOptions@ for the domain. See
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced options>
-- for more information.
--
-- 'changeProgressDetails', 'domainConfig_changeProgressDetails' - Specifies change details of the domain configuration change.
--
-- 'advancedSecurityOptions', 'domainConfig_advancedSecurityOptions' - Specifies @AdvancedSecurityOptions@ for the domain.
--
-- 'cognitoOptions', 'domainConfig_cognitoOptions' - The @CognitoOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/cognito-auth.html Configuring Amazon Cognito authentication for OpenSearch Dashboards>.
--
-- 'encryptionAtRestOptions', 'domainConfig_encryptionAtRestOptions' - The @EncryptionAtRestOptions@ for the domain.
--
-- 'eBSOptions', 'domainConfig_eBSOptions' - The @EBSOptions@ for the domain.
--
-- 'accessPolicies', 'domainConfig_accessPolicies' - IAM access policy as a JSON-formatted string.
--
-- 'vPCOptions', 'domainConfig_vPCOptions' - The @VPCOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>.
--
-- 'autoTuneOptions', 'domainConfig_autoTuneOptions' - Specifies @AutoTuneOptions@ for the domain.
--
-- 'domainEndpointOptions', 'domainConfig_domainEndpointOptions' - The @DomainEndpointOptions@ for the domain.
--
-- 'snapshotOptions', 'domainConfig_snapshotOptions' - The @SnapshotOptions@ for the domain.
--
-- 'logPublishingOptions', 'domainConfig_logPublishingOptions' - Log publishing options for the given domain.
--
-- 'engineVersion', 'domainConfig_engineVersion' - String of format Elasticsearch_X.Y or OpenSearch_X.Y to specify the
-- engine version for the OpenSearch or Elasticsearch domain.
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

-- | The @NodeToNodeEncryptionOptions@ for the domain.
domainConfig_nodeToNodeEncryptionOptions :: Lens.Lens' DomainConfig (Prelude.Maybe NodeToNodeEncryptionOptionsStatus)
domainConfig_nodeToNodeEncryptionOptions = Lens.lens (\DomainConfig' {nodeToNodeEncryptionOptions} -> nodeToNodeEncryptionOptions) (\s@DomainConfig' {} a -> s {nodeToNodeEncryptionOptions = a} :: DomainConfig)

-- | The @ClusterConfig@ for the domain.
domainConfig_clusterConfig :: Lens.Lens' DomainConfig (Prelude.Maybe ClusterConfigStatus)
domainConfig_clusterConfig = Lens.lens (\DomainConfig' {clusterConfig} -> clusterConfig) (\s@DomainConfig' {} a -> s {clusterConfig = a} :: DomainConfig)

-- | The @AdvancedOptions@ for the domain. See
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html#createdomain-configure-advanced-options Advanced options>
-- for more information.
domainConfig_advancedOptions :: Lens.Lens' DomainConfig (Prelude.Maybe AdvancedOptionsStatus)
domainConfig_advancedOptions = Lens.lens (\DomainConfig' {advancedOptions} -> advancedOptions) (\s@DomainConfig' {} a -> s {advancedOptions = a} :: DomainConfig)

-- | Specifies change details of the domain configuration change.
domainConfig_changeProgressDetails :: Lens.Lens' DomainConfig (Prelude.Maybe ChangeProgressDetails)
domainConfig_changeProgressDetails = Lens.lens (\DomainConfig' {changeProgressDetails} -> changeProgressDetails) (\s@DomainConfig' {} a -> s {changeProgressDetails = a} :: DomainConfig)

-- | Specifies @AdvancedSecurityOptions@ for the domain.
domainConfig_advancedSecurityOptions :: Lens.Lens' DomainConfig (Prelude.Maybe AdvancedSecurityOptionsStatus)
domainConfig_advancedSecurityOptions = Lens.lens (\DomainConfig' {advancedSecurityOptions} -> advancedSecurityOptions) (\s@DomainConfig' {} a -> s {advancedSecurityOptions = a} :: DomainConfig)

-- | The @CognitoOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/cognito-auth.html Configuring Amazon Cognito authentication for OpenSearch Dashboards>.
domainConfig_cognitoOptions :: Lens.Lens' DomainConfig (Prelude.Maybe CognitoOptionsStatus)
domainConfig_cognitoOptions = Lens.lens (\DomainConfig' {cognitoOptions} -> cognitoOptions) (\s@DomainConfig' {} a -> s {cognitoOptions = a} :: DomainConfig)

-- | The @EncryptionAtRestOptions@ for the domain.
domainConfig_encryptionAtRestOptions :: Lens.Lens' DomainConfig (Prelude.Maybe EncryptionAtRestOptionsStatus)
domainConfig_encryptionAtRestOptions = Lens.lens (\DomainConfig' {encryptionAtRestOptions} -> encryptionAtRestOptions) (\s@DomainConfig' {} a -> s {encryptionAtRestOptions = a} :: DomainConfig)

-- | The @EBSOptions@ for the domain.
domainConfig_eBSOptions :: Lens.Lens' DomainConfig (Prelude.Maybe EBSOptionsStatus)
domainConfig_eBSOptions = Lens.lens (\DomainConfig' {eBSOptions} -> eBSOptions) (\s@DomainConfig' {} a -> s {eBSOptions = a} :: DomainConfig)

-- | IAM access policy as a JSON-formatted string.
domainConfig_accessPolicies :: Lens.Lens' DomainConfig (Prelude.Maybe AccessPoliciesStatus)
domainConfig_accessPolicies = Lens.lens (\DomainConfig' {accessPolicies} -> accessPolicies) (\s@DomainConfig' {} a -> s {accessPolicies = a} :: DomainConfig)

-- | The @VPCOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>.
domainConfig_vPCOptions :: Lens.Lens' DomainConfig (Prelude.Maybe VPCDerivedInfoStatus)
domainConfig_vPCOptions = Lens.lens (\DomainConfig' {vPCOptions} -> vPCOptions) (\s@DomainConfig' {} a -> s {vPCOptions = a} :: DomainConfig)

-- | Specifies @AutoTuneOptions@ for the domain.
domainConfig_autoTuneOptions :: Lens.Lens' DomainConfig (Prelude.Maybe AutoTuneOptionsStatus)
domainConfig_autoTuneOptions = Lens.lens (\DomainConfig' {autoTuneOptions} -> autoTuneOptions) (\s@DomainConfig' {} a -> s {autoTuneOptions = a} :: DomainConfig)

-- | The @DomainEndpointOptions@ for the domain.
domainConfig_domainEndpointOptions :: Lens.Lens' DomainConfig (Prelude.Maybe DomainEndpointOptionsStatus)
domainConfig_domainEndpointOptions = Lens.lens (\DomainConfig' {domainEndpointOptions} -> domainEndpointOptions) (\s@DomainConfig' {} a -> s {domainEndpointOptions = a} :: DomainConfig)

-- | The @SnapshotOptions@ for the domain.
domainConfig_snapshotOptions :: Lens.Lens' DomainConfig (Prelude.Maybe SnapshotOptionsStatus)
domainConfig_snapshotOptions = Lens.lens (\DomainConfig' {snapshotOptions} -> snapshotOptions) (\s@DomainConfig' {} a -> s {snapshotOptions = a} :: DomainConfig)

-- | Log publishing options for the given domain.
domainConfig_logPublishingOptions :: Lens.Lens' DomainConfig (Prelude.Maybe LogPublishingOptionsStatus)
domainConfig_logPublishingOptions = Lens.lens (\DomainConfig' {logPublishingOptions} -> logPublishingOptions) (\s@DomainConfig' {} a -> s {logPublishingOptions = a} :: DomainConfig)

-- | String of format Elasticsearch_X.Y or OpenSearch_X.Y to specify the
-- engine version for the OpenSearch or Elasticsearch domain.
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
