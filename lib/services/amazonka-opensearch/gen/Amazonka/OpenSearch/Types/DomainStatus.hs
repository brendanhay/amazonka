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
-- Module      : Amazonka.OpenSearch.Types.DomainStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.DomainStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.AdvancedSecurityOptions
import Amazonka.OpenSearch.Types.AutoTuneOptionsOutput
import Amazonka.OpenSearch.Types.ChangeProgressDetails
import Amazonka.OpenSearch.Types.ClusterConfig
import Amazonka.OpenSearch.Types.CognitoOptions
import Amazonka.OpenSearch.Types.DomainEndpointOptions
import Amazonka.OpenSearch.Types.EBSOptions
import Amazonka.OpenSearch.Types.EncryptionAtRestOptions
import Amazonka.OpenSearch.Types.LogPublishingOption
import Amazonka.OpenSearch.Types.LogType
import Amazonka.OpenSearch.Types.NodeToNodeEncryptionOptions
import Amazonka.OpenSearch.Types.ServiceSoftwareOptions
import Amazonka.OpenSearch.Types.SnapshotOptions
import Amazonka.OpenSearch.Types.VPCDerivedInfo
import qualified Amazonka.Prelude as Prelude

-- | The current status of an OpenSearch Service domain.
--
-- /See:/ 'newDomainStatus' smart constructor.
data DomainStatus = DomainStatus'
  { -- | Identity and Access Management (IAM) policy document specifying the
    -- access policies for the domain.
    accessPolicies :: Prelude.Maybe Prelude.Text,
    -- | Key-value pairs that specify advanced configuration options.
    advancedOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Settings for fine-grained access control.
    advancedSecurityOptions :: Prelude.Maybe AdvancedSecurityOptions,
    -- | Auto-Tune settings for the domain.
    autoTuneOptions :: Prelude.Maybe AutoTuneOptionsOutput,
    -- | Information about a configuration change happening on the domain.
    changeProgressDetails :: Prelude.Maybe ChangeProgressDetails,
    -- | Key-value pairs to configure Amazon Cognito authentication for
    -- OpenSearch Dashboards.
    cognitoOptions :: Prelude.Maybe CognitoOptions,
    -- | Creation status of an OpenSearch Service domain. True if domain creation
    -- is complete. False if domain creation is still in progress.
    created :: Prelude.Maybe Prelude.Bool,
    -- | Deletion status of an OpenSearch Service domain. True if domain deletion
    -- is complete. False if domain deletion is still in progress. Once
    -- deletion is complete, the status of the domain is no longer returned.
    deleted :: Prelude.Maybe Prelude.Bool,
    -- | Additional options for the domain endpoint, such as whether to require
    -- HTTPS for all traffic.
    domainEndpointOptions :: Prelude.Maybe DomainEndpointOptions,
    -- | Container for EBS-based storage settings for the domain.
    eBSOptions :: Prelude.Maybe EBSOptions,
    -- | Encryption at rest settings for the domain.
    encryptionAtRestOptions :: Prelude.Maybe EncryptionAtRestOptions,
    -- | Domain-specific endpoint used to submit index, search, and data upload
    -- requests to the domain.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The key-value pair that exists if the OpenSearch Service domain uses VPC
    -- endpoints.. Example @key, value@:
    -- @\'vpc\',\'vpc-endpoint-h2dsd34efgyghrtguk5gt6j2foh4.us-east-1.es.amazonaws.com\'@.
    endpoints :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Version of OpenSearch or Elasticsearch that the domain is running, in
    -- the format @Elasticsearch_X.Y@ or @OpenSearch_X.Y@.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Log publishing options for the domain.
    logPublishingOptions :: Prelude.Maybe (Prelude.HashMap LogType LogPublishingOption),
    -- | Whether node-to-node encryption is enabled or disabled.
    nodeToNodeEncryptionOptions :: Prelude.Maybe NodeToNodeEncryptionOptions,
    -- | The status of the domain configuration. True if OpenSearch Service is
    -- processing configuration changes. False if the configuration is active.
    processing :: Prelude.Maybe Prelude.Bool,
    -- | The current status of the domain\'s service software.
    serviceSoftwareOptions :: Prelude.Maybe ServiceSoftwareOptions,
    -- | DEPRECATED. Container for parameters required to configure automated
    -- snapshots of domain indexes.
    snapshotOptions :: Prelude.Maybe SnapshotOptions,
    -- | The status of a domain version upgrade to a new version of OpenSearch or
    -- Elasticsearch. True if OpenSearch Service is in the process of a version
    -- upgrade. False if the configuration is active.
    upgradeProcessing :: Prelude.Maybe Prelude.Bool,
    -- | The VPC configuration for the domain.
    vPCOptions :: Prelude.Maybe VPCDerivedInfo,
    -- | Unique identifier for the domain.
    domainId :: Prelude.Text,
    -- | Name of the domain. Domain names are unique across all domains owned by
    -- the same account within an Amazon Web Services Region.
    domainName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the domain. For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM identifiers>
    -- in the /AWS Identity and Access Management User Guide/.
    arn :: Prelude.Text,
    -- | Container for the cluster configuration of the domain.
    clusterConfig :: ClusterConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessPolicies', 'domainStatus_accessPolicies' - Identity and Access Management (IAM) policy document specifying the
-- access policies for the domain.
--
-- 'advancedOptions', 'domainStatus_advancedOptions' - Key-value pairs that specify advanced configuration options.
--
-- 'advancedSecurityOptions', 'domainStatus_advancedSecurityOptions' - Settings for fine-grained access control.
--
-- 'autoTuneOptions', 'domainStatus_autoTuneOptions' - Auto-Tune settings for the domain.
--
-- 'changeProgressDetails', 'domainStatus_changeProgressDetails' - Information about a configuration change happening on the domain.
--
-- 'cognitoOptions', 'domainStatus_cognitoOptions' - Key-value pairs to configure Amazon Cognito authentication for
-- OpenSearch Dashboards.
--
-- 'created', 'domainStatus_created' - Creation status of an OpenSearch Service domain. True if domain creation
-- is complete. False if domain creation is still in progress.
--
-- 'deleted', 'domainStatus_deleted' - Deletion status of an OpenSearch Service domain. True if domain deletion
-- is complete. False if domain deletion is still in progress. Once
-- deletion is complete, the status of the domain is no longer returned.
--
-- 'domainEndpointOptions', 'domainStatus_domainEndpointOptions' - Additional options for the domain endpoint, such as whether to require
-- HTTPS for all traffic.
--
-- 'eBSOptions', 'domainStatus_eBSOptions' - Container for EBS-based storage settings for the domain.
--
-- 'encryptionAtRestOptions', 'domainStatus_encryptionAtRestOptions' - Encryption at rest settings for the domain.
--
-- 'endpoint', 'domainStatus_endpoint' - Domain-specific endpoint used to submit index, search, and data upload
-- requests to the domain.
--
-- 'endpoints', 'domainStatus_endpoints' - The key-value pair that exists if the OpenSearch Service domain uses VPC
-- endpoints.. Example @key, value@:
-- @\'vpc\',\'vpc-endpoint-h2dsd34efgyghrtguk5gt6j2foh4.us-east-1.es.amazonaws.com\'@.
--
-- 'engineVersion', 'domainStatus_engineVersion' - Version of OpenSearch or Elasticsearch that the domain is running, in
-- the format @Elasticsearch_X.Y@ or @OpenSearch_X.Y@.
--
-- 'logPublishingOptions', 'domainStatus_logPublishingOptions' - Log publishing options for the domain.
--
-- 'nodeToNodeEncryptionOptions', 'domainStatus_nodeToNodeEncryptionOptions' - Whether node-to-node encryption is enabled or disabled.
--
-- 'processing', 'domainStatus_processing' - The status of the domain configuration. True if OpenSearch Service is
-- processing configuration changes. False if the configuration is active.
--
-- 'serviceSoftwareOptions', 'domainStatus_serviceSoftwareOptions' - The current status of the domain\'s service software.
--
-- 'snapshotOptions', 'domainStatus_snapshotOptions' - DEPRECATED. Container for parameters required to configure automated
-- snapshots of domain indexes.
--
-- 'upgradeProcessing', 'domainStatus_upgradeProcessing' - The status of a domain version upgrade to a new version of OpenSearch or
-- Elasticsearch. True if OpenSearch Service is in the process of a version
-- upgrade. False if the configuration is active.
--
-- 'vPCOptions', 'domainStatus_vPCOptions' - The VPC configuration for the domain.
--
-- 'domainId', 'domainStatus_domainId' - Unique identifier for the domain.
--
-- 'domainName', 'domainStatus_domainName' - Name of the domain. Domain names are unique across all domains owned by
-- the same account within an Amazon Web Services Region.
--
-- 'arn', 'domainStatus_arn' - The Amazon Resource Name (ARN) of the domain. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM identifiers>
-- in the /AWS Identity and Access Management User Guide/.
--
-- 'clusterConfig', 'domainStatus_clusterConfig' - Container for the cluster configuration of the domain.
newDomainStatus ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'clusterConfig'
  ClusterConfig ->
  DomainStatus
newDomainStatus
  pDomainId_
  pDomainName_
  pARN_
  pClusterConfig_ =
    DomainStatus'
      { accessPolicies = Prelude.Nothing,
        advancedOptions = Prelude.Nothing,
        advancedSecurityOptions = Prelude.Nothing,
        autoTuneOptions = Prelude.Nothing,
        changeProgressDetails = Prelude.Nothing,
        cognitoOptions = Prelude.Nothing,
        created = Prelude.Nothing,
        deleted = Prelude.Nothing,
        domainEndpointOptions = Prelude.Nothing,
        eBSOptions = Prelude.Nothing,
        encryptionAtRestOptions = Prelude.Nothing,
        endpoint = Prelude.Nothing,
        endpoints = Prelude.Nothing,
        engineVersion = Prelude.Nothing,
        logPublishingOptions = Prelude.Nothing,
        nodeToNodeEncryptionOptions = Prelude.Nothing,
        processing = Prelude.Nothing,
        serviceSoftwareOptions = Prelude.Nothing,
        snapshotOptions = Prelude.Nothing,
        upgradeProcessing = Prelude.Nothing,
        vPCOptions = Prelude.Nothing,
        domainId = pDomainId_,
        domainName = pDomainName_,
        arn = pARN_,
        clusterConfig = pClusterConfig_
      }

-- | Identity and Access Management (IAM) policy document specifying the
-- access policies for the domain.
domainStatus_accessPolicies :: Lens.Lens' DomainStatus (Prelude.Maybe Prelude.Text)
domainStatus_accessPolicies = Lens.lens (\DomainStatus' {accessPolicies} -> accessPolicies) (\s@DomainStatus' {} a -> s {accessPolicies = a} :: DomainStatus)

-- | Key-value pairs that specify advanced configuration options.
domainStatus_advancedOptions :: Lens.Lens' DomainStatus (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
domainStatus_advancedOptions = Lens.lens (\DomainStatus' {advancedOptions} -> advancedOptions) (\s@DomainStatus' {} a -> s {advancedOptions = a} :: DomainStatus) Prelude.. Lens.mapping Lens.coerced

-- | Settings for fine-grained access control.
domainStatus_advancedSecurityOptions :: Lens.Lens' DomainStatus (Prelude.Maybe AdvancedSecurityOptions)
domainStatus_advancedSecurityOptions = Lens.lens (\DomainStatus' {advancedSecurityOptions} -> advancedSecurityOptions) (\s@DomainStatus' {} a -> s {advancedSecurityOptions = a} :: DomainStatus)

-- | Auto-Tune settings for the domain.
domainStatus_autoTuneOptions :: Lens.Lens' DomainStatus (Prelude.Maybe AutoTuneOptionsOutput)
domainStatus_autoTuneOptions = Lens.lens (\DomainStatus' {autoTuneOptions} -> autoTuneOptions) (\s@DomainStatus' {} a -> s {autoTuneOptions = a} :: DomainStatus)

-- | Information about a configuration change happening on the domain.
domainStatus_changeProgressDetails :: Lens.Lens' DomainStatus (Prelude.Maybe ChangeProgressDetails)
domainStatus_changeProgressDetails = Lens.lens (\DomainStatus' {changeProgressDetails} -> changeProgressDetails) (\s@DomainStatus' {} a -> s {changeProgressDetails = a} :: DomainStatus)

-- | Key-value pairs to configure Amazon Cognito authentication for
-- OpenSearch Dashboards.
domainStatus_cognitoOptions :: Lens.Lens' DomainStatus (Prelude.Maybe CognitoOptions)
domainStatus_cognitoOptions = Lens.lens (\DomainStatus' {cognitoOptions} -> cognitoOptions) (\s@DomainStatus' {} a -> s {cognitoOptions = a} :: DomainStatus)

-- | Creation status of an OpenSearch Service domain. True if domain creation
-- is complete. False if domain creation is still in progress.
domainStatus_created :: Lens.Lens' DomainStatus (Prelude.Maybe Prelude.Bool)
domainStatus_created = Lens.lens (\DomainStatus' {created} -> created) (\s@DomainStatus' {} a -> s {created = a} :: DomainStatus)

-- | Deletion status of an OpenSearch Service domain. True if domain deletion
-- is complete. False if domain deletion is still in progress. Once
-- deletion is complete, the status of the domain is no longer returned.
domainStatus_deleted :: Lens.Lens' DomainStatus (Prelude.Maybe Prelude.Bool)
domainStatus_deleted = Lens.lens (\DomainStatus' {deleted} -> deleted) (\s@DomainStatus' {} a -> s {deleted = a} :: DomainStatus)

-- | Additional options for the domain endpoint, such as whether to require
-- HTTPS for all traffic.
domainStatus_domainEndpointOptions :: Lens.Lens' DomainStatus (Prelude.Maybe DomainEndpointOptions)
domainStatus_domainEndpointOptions = Lens.lens (\DomainStatus' {domainEndpointOptions} -> domainEndpointOptions) (\s@DomainStatus' {} a -> s {domainEndpointOptions = a} :: DomainStatus)

-- | Container for EBS-based storage settings for the domain.
domainStatus_eBSOptions :: Lens.Lens' DomainStatus (Prelude.Maybe EBSOptions)
domainStatus_eBSOptions = Lens.lens (\DomainStatus' {eBSOptions} -> eBSOptions) (\s@DomainStatus' {} a -> s {eBSOptions = a} :: DomainStatus)

-- | Encryption at rest settings for the domain.
domainStatus_encryptionAtRestOptions :: Lens.Lens' DomainStatus (Prelude.Maybe EncryptionAtRestOptions)
domainStatus_encryptionAtRestOptions = Lens.lens (\DomainStatus' {encryptionAtRestOptions} -> encryptionAtRestOptions) (\s@DomainStatus' {} a -> s {encryptionAtRestOptions = a} :: DomainStatus)

-- | Domain-specific endpoint used to submit index, search, and data upload
-- requests to the domain.
domainStatus_endpoint :: Lens.Lens' DomainStatus (Prelude.Maybe Prelude.Text)
domainStatus_endpoint = Lens.lens (\DomainStatus' {endpoint} -> endpoint) (\s@DomainStatus' {} a -> s {endpoint = a} :: DomainStatus)

-- | The key-value pair that exists if the OpenSearch Service domain uses VPC
-- endpoints.. Example @key, value@:
-- @\'vpc\',\'vpc-endpoint-h2dsd34efgyghrtguk5gt6j2foh4.us-east-1.es.amazonaws.com\'@.
domainStatus_endpoints :: Lens.Lens' DomainStatus (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
domainStatus_endpoints = Lens.lens (\DomainStatus' {endpoints} -> endpoints) (\s@DomainStatus' {} a -> s {endpoints = a} :: DomainStatus) Prelude.. Lens.mapping Lens.coerced

-- | Version of OpenSearch or Elasticsearch that the domain is running, in
-- the format @Elasticsearch_X.Y@ or @OpenSearch_X.Y@.
domainStatus_engineVersion :: Lens.Lens' DomainStatus (Prelude.Maybe Prelude.Text)
domainStatus_engineVersion = Lens.lens (\DomainStatus' {engineVersion} -> engineVersion) (\s@DomainStatus' {} a -> s {engineVersion = a} :: DomainStatus)

-- | Log publishing options for the domain.
domainStatus_logPublishingOptions :: Lens.Lens' DomainStatus (Prelude.Maybe (Prelude.HashMap LogType LogPublishingOption))
domainStatus_logPublishingOptions = Lens.lens (\DomainStatus' {logPublishingOptions} -> logPublishingOptions) (\s@DomainStatus' {} a -> s {logPublishingOptions = a} :: DomainStatus) Prelude.. Lens.mapping Lens.coerced

-- | Whether node-to-node encryption is enabled or disabled.
domainStatus_nodeToNodeEncryptionOptions :: Lens.Lens' DomainStatus (Prelude.Maybe NodeToNodeEncryptionOptions)
domainStatus_nodeToNodeEncryptionOptions = Lens.lens (\DomainStatus' {nodeToNodeEncryptionOptions} -> nodeToNodeEncryptionOptions) (\s@DomainStatus' {} a -> s {nodeToNodeEncryptionOptions = a} :: DomainStatus)

-- | The status of the domain configuration. True if OpenSearch Service is
-- processing configuration changes. False if the configuration is active.
domainStatus_processing :: Lens.Lens' DomainStatus (Prelude.Maybe Prelude.Bool)
domainStatus_processing = Lens.lens (\DomainStatus' {processing} -> processing) (\s@DomainStatus' {} a -> s {processing = a} :: DomainStatus)

-- | The current status of the domain\'s service software.
domainStatus_serviceSoftwareOptions :: Lens.Lens' DomainStatus (Prelude.Maybe ServiceSoftwareOptions)
domainStatus_serviceSoftwareOptions = Lens.lens (\DomainStatus' {serviceSoftwareOptions} -> serviceSoftwareOptions) (\s@DomainStatus' {} a -> s {serviceSoftwareOptions = a} :: DomainStatus)

-- | DEPRECATED. Container for parameters required to configure automated
-- snapshots of domain indexes.
domainStatus_snapshotOptions :: Lens.Lens' DomainStatus (Prelude.Maybe SnapshotOptions)
domainStatus_snapshotOptions = Lens.lens (\DomainStatus' {snapshotOptions} -> snapshotOptions) (\s@DomainStatus' {} a -> s {snapshotOptions = a} :: DomainStatus)

-- | The status of a domain version upgrade to a new version of OpenSearch or
-- Elasticsearch. True if OpenSearch Service is in the process of a version
-- upgrade. False if the configuration is active.
domainStatus_upgradeProcessing :: Lens.Lens' DomainStatus (Prelude.Maybe Prelude.Bool)
domainStatus_upgradeProcessing = Lens.lens (\DomainStatus' {upgradeProcessing} -> upgradeProcessing) (\s@DomainStatus' {} a -> s {upgradeProcessing = a} :: DomainStatus)

-- | The VPC configuration for the domain.
domainStatus_vPCOptions :: Lens.Lens' DomainStatus (Prelude.Maybe VPCDerivedInfo)
domainStatus_vPCOptions = Lens.lens (\DomainStatus' {vPCOptions} -> vPCOptions) (\s@DomainStatus' {} a -> s {vPCOptions = a} :: DomainStatus)

-- | Unique identifier for the domain.
domainStatus_domainId :: Lens.Lens' DomainStatus Prelude.Text
domainStatus_domainId = Lens.lens (\DomainStatus' {domainId} -> domainId) (\s@DomainStatus' {} a -> s {domainId = a} :: DomainStatus)

-- | Name of the domain. Domain names are unique across all domains owned by
-- the same account within an Amazon Web Services Region.
domainStatus_domainName :: Lens.Lens' DomainStatus Prelude.Text
domainStatus_domainName = Lens.lens (\DomainStatus' {domainName} -> domainName) (\s@DomainStatus' {} a -> s {domainName = a} :: DomainStatus)

-- | The Amazon Resource Name (ARN) of the domain. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM identifiers>
-- in the /AWS Identity and Access Management User Guide/.
domainStatus_arn :: Lens.Lens' DomainStatus Prelude.Text
domainStatus_arn = Lens.lens (\DomainStatus' {arn} -> arn) (\s@DomainStatus' {} a -> s {arn = a} :: DomainStatus)

-- | Container for the cluster configuration of the domain.
domainStatus_clusterConfig :: Lens.Lens' DomainStatus ClusterConfig
domainStatus_clusterConfig = Lens.lens (\DomainStatus' {clusterConfig} -> clusterConfig) (\s@DomainStatus' {} a -> s {clusterConfig = a} :: DomainStatus)

instance Data.FromJSON DomainStatus where
  parseJSON =
    Data.withObject
      "DomainStatus"
      ( \x ->
          DomainStatus'
            Prelude.<$> (x Data..:? "AccessPolicies")
            Prelude.<*> ( x Data..:? "AdvancedOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "AdvancedSecurityOptions")
            Prelude.<*> (x Data..:? "AutoTuneOptions")
            Prelude.<*> (x Data..:? "ChangeProgressDetails")
            Prelude.<*> (x Data..:? "CognitoOptions")
            Prelude.<*> (x Data..:? "Created")
            Prelude.<*> (x Data..:? "Deleted")
            Prelude.<*> (x Data..:? "DomainEndpointOptions")
            Prelude.<*> (x Data..:? "EBSOptions")
            Prelude.<*> (x Data..:? "EncryptionAtRestOptions")
            Prelude.<*> (x Data..:? "Endpoint")
            Prelude.<*> (x Data..:? "Endpoints" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "EngineVersion")
            Prelude.<*> ( x Data..:? "LogPublishingOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "NodeToNodeEncryptionOptions")
            Prelude.<*> (x Data..:? "Processing")
            Prelude.<*> (x Data..:? "ServiceSoftwareOptions")
            Prelude.<*> (x Data..:? "SnapshotOptions")
            Prelude.<*> (x Data..:? "UpgradeProcessing")
            Prelude.<*> (x Data..:? "VPCOptions")
            Prelude.<*> (x Data..: "DomainId")
            Prelude.<*> (x Data..: "DomainName")
            Prelude.<*> (x Data..: "ARN")
            Prelude.<*> (x Data..: "ClusterConfig")
      )

instance Prelude.Hashable DomainStatus where
  hashWithSalt _salt DomainStatus' {..} =
    _salt `Prelude.hashWithSalt` accessPolicies
      `Prelude.hashWithSalt` advancedOptions
      `Prelude.hashWithSalt` advancedSecurityOptions
      `Prelude.hashWithSalt` autoTuneOptions
      `Prelude.hashWithSalt` changeProgressDetails
      `Prelude.hashWithSalt` cognitoOptions
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` deleted
      `Prelude.hashWithSalt` domainEndpointOptions
      `Prelude.hashWithSalt` eBSOptions
      `Prelude.hashWithSalt` encryptionAtRestOptions
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` endpoints
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` logPublishingOptions
      `Prelude.hashWithSalt` nodeToNodeEncryptionOptions
      `Prelude.hashWithSalt` processing
      `Prelude.hashWithSalt` serviceSoftwareOptions
      `Prelude.hashWithSalt` snapshotOptions
      `Prelude.hashWithSalt` upgradeProcessing
      `Prelude.hashWithSalt` vPCOptions
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` clusterConfig

instance Prelude.NFData DomainStatus where
  rnf DomainStatus' {..} =
    Prelude.rnf accessPolicies
      `Prelude.seq` Prelude.rnf advancedOptions
      `Prelude.seq` Prelude.rnf advancedSecurityOptions
      `Prelude.seq` Prelude.rnf autoTuneOptions
      `Prelude.seq` Prelude.rnf changeProgressDetails
      `Prelude.seq` Prelude.rnf cognitoOptions
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf deleted
      `Prelude.seq` Prelude.rnf domainEndpointOptions
      `Prelude.seq` Prelude.rnf eBSOptions
      `Prelude.seq` Prelude.rnf encryptionAtRestOptions
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf endpoints
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf logPublishingOptions
      `Prelude.seq` Prelude.rnf
        nodeToNodeEncryptionOptions
      `Prelude.seq` Prelude.rnf processing
      `Prelude.seq` Prelude.rnf serviceSoftwareOptions
      `Prelude.seq` Prelude.rnf snapshotOptions
      `Prelude.seq` Prelude.rnf upgradeProcessing
      `Prelude.seq` Prelude.rnf vPCOptions
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf
        clusterConfig
