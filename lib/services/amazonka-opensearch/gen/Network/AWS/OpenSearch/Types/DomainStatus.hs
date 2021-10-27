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
-- Module      : Network.AWS.OpenSearch.Types.DomainStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpenSearch.Types.DomainStatus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpenSearch.Types.AdvancedSecurityOptions
import Network.AWS.OpenSearch.Types.AutoTuneOptionsOutput
import Network.AWS.OpenSearch.Types.ClusterConfig
import Network.AWS.OpenSearch.Types.CognitoOptions
import Network.AWS.OpenSearch.Types.DomainEndpointOptions
import Network.AWS.OpenSearch.Types.EBSOptions
import Network.AWS.OpenSearch.Types.EncryptionAtRestOptions
import Network.AWS.OpenSearch.Types.LogPublishingOption
import Network.AWS.OpenSearch.Types.LogType
import Network.AWS.OpenSearch.Types.NodeToNodeEncryptionOptions
import Network.AWS.OpenSearch.Types.ServiceSoftwareOptions
import Network.AWS.OpenSearch.Types.SnapshotOptions
import Network.AWS.OpenSearch.Types.VPCDerivedInfo
import qualified Network.AWS.Prelude as Prelude

-- | The current status of a domain.
--
-- /See:/ 'newDomainStatus' smart constructor.
data DomainStatus = DomainStatus'
  { -- | The @EBSOptions@ for the specified domain.
    eBSOptions :: Prelude.Maybe EBSOptions,
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The status of the @NodeToNodeEncryptionOptions@.
    nodeToNodeEncryptionOptions :: Prelude.Maybe NodeToNodeEncryptionOptions,
    -- | IAM access policy as a JSON-formatted string.
    accessPolicies :: Prelude.Maybe Prelude.Text,
    -- | The current status of the domain\'s service software.
    serviceSoftwareOptions :: Prelude.Maybe ServiceSoftwareOptions,
    -- | The current status of the domain\'s Auto-Tune options.
    autoTuneOptions :: Prelude.Maybe AutoTuneOptionsOutput,
    -- | Log publishing options for the given domain.
    logPublishingOptions :: Prelude.Maybe (Prelude.HashMap LogType LogPublishingOption),
    -- | The current status of the domain\'s advanced security options.
    advancedSecurityOptions :: Prelude.Maybe AdvancedSecurityOptions,
    -- | The domain creation status. @True@ if the creation of a domain is
    -- complete. @ False @ if domain creation is still in progress.
    created :: Prelude.Maybe Prelude.Bool,
    -- | The status of the @SnapshotOptions@.
    snapshotOptions :: Prelude.Maybe SnapshotOptions,
    -- | The @CognitoOptions@ for the specified domain. For more information, see
    -- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/cognito-auth.html Configuring Amazon Cognito authentication for OpenSearch Dashboards>.
    cognitoOptions :: Prelude.Maybe CognitoOptions,
    -- | The status of the @EncryptionAtRestOptions@.
    encryptionAtRestOptions :: Prelude.Maybe EncryptionAtRestOptions,
    -- | The domain deletion status. @True@ if a delete request has been received
    -- for the domain but resource cleanup is still in progress. @False@ if the
    -- domain has not been deleted. Once domain deletion is complete, the
    -- status of the domain is no longer returned.
    deleted :: Prelude.Maybe Prelude.Bool,
    -- | The @VPCOptions@ for the specified domain. For more information, see
    -- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>.
    vPCOptions :: Prelude.Maybe VPCDerivedInfo,
    -- | Map containing the domain endpoints used to submit index and search
    -- requests. Example @key, value@:
    -- @\'vpc\',\'vpc-endpoint-h2dsd34efgyghrtguk5gt6j2foh4.us-east-1.es.amazonaws.com\'@.
    endpoints :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The current status of the domain\'s endpoint options.
    domainEndpointOptions :: Prelude.Maybe DomainEndpointOptions,
    -- | The status of the domain configuration. @True@ if Amazon OpenSearch
    -- Service is processing configuration changes. @False@ if the
    -- configuration is active.
    processing :: Prelude.Maybe Prelude.Bool,
    -- | The domain endpoint that you use to submit index and search requests.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The status of a domain version upgrade. @True@ if Amazon OpenSearch
    -- Service is undergoing a version upgrade. @False@ if the configuration is
    -- active.
    upgradeProcessing :: Prelude.Maybe Prelude.Bool,
    -- | The status of the @AdvancedOptions@.
    advancedOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique identifier for the specified domain.
    domainId :: Prelude.Text,
    -- | The name of a domain. Domain names are unique across the domains owned
    -- by an account within an AWS region. Domain names start with a letter or
    -- number and can contain the following characters: a-z (lowercase), 0-9,
    -- and - (hyphen).
    domainName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a domain. See
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM identifiers>
    -- in the /AWS Identity and Access Management User Guide/ for more
    -- information.
    arn :: Prelude.Text,
    -- | The type and number of instances in the domain.
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
-- 'eBSOptions', 'domainStatus_eBSOptions' - The @EBSOptions@ for the specified domain.
--
-- 'engineVersion', 'domainStatus_engineVersion' - Undocumented member.
--
-- 'nodeToNodeEncryptionOptions', 'domainStatus_nodeToNodeEncryptionOptions' - The status of the @NodeToNodeEncryptionOptions@.
--
-- 'accessPolicies', 'domainStatus_accessPolicies' - IAM access policy as a JSON-formatted string.
--
-- 'serviceSoftwareOptions', 'domainStatus_serviceSoftwareOptions' - The current status of the domain\'s service software.
--
-- 'autoTuneOptions', 'domainStatus_autoTuneOptions' - The current status of the domain\'s Auto-Tune options.
--
-- 'logPublishingOptions', 'domainStatus_logPublishingOptions' - Log publishing options for the given domain.
--
-- 'advancedSecurityOptions', 'domainStatus_advancedSecurityOptions' - The current status of the domain\'s advanced security options.
--
-- 'created', 'domainStatus_created' - The domain creation status. @True@ if the creation of a domain is
-- complete. @ False @ if domain creation is still in progress.
--
-- 'snapshotOptions', 'domainStatus_snapshotOptions' - The status of the @SnapshotOptions@.
--
-- 'cognitoOptions', 'domainStatus_cognitoOptions' - The @CognitoOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/cognito-auth.html Configuring Amazon Cognito authentication for OpenSearch Dashboards>.
--
-- 'encryptionAtRestOptions', 'domainStatus_encryptionAtRestOptions' - The status of the @EncryptionAtRestOptions@.
--
-- 'deleted', 'domainStatus_deleted' - The domain deletion status. @True@ if a delete request has been received
-- for the domain but resource cleanup is still in progress. @False@ if the
-- domain has not been deleted. Once domain deletion is complete, the
-- status of the domain is no longer returned.
--
-- 'vPCOptions', 'domainStatus_vPCOptions' - The @VPCOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>.
--
-- 'endpoints', 'domainStatus_endpoints' - Map containing the domain endpoints used to submit index and search
-- requests. Example @key, value@:
-- @\'vpc\',\'vpc-endpoint-h2dsd34efgyghrtguk5gt6j2foh4.us-east-1.es.amazonaws.com\'@.
--
-- 'domainEndpointOptions', 'domainStatus_domainEndpointOptions' - The current status of the domain\'s endpoint options.
--
-- 'processing', 'domainStatus_processing' - The status of the domain configuration. @True@ if Amazon OpenSearch
-- Service is processing configuration changes. @False@ if the
-- configuration is active.
--
-- 'endpoint', 'domainStatus_endpoint' - The domain endpoint that you use to submit index and search requests.
--
-- 'upgradeProcessing', 'domainStatus_upgradeProcessing' - The status of a domain version upgrade. @True@ if Amazon OpenSearch
-- Service is undergoing a version upgrade. @False@ if the configuration is
-- active.
--
-- 'advancedOptions', 'domainStatus_advancedOptions' - The status of the @AdvancedOptions@.
--
-- 'domainId', 'domainStatus_domainId' - The unique identifier for the specified domain.
--
-- 'domainName', 'domainStatus_domainName' - The name of a domain. Domain names are unique across the domains owned
-- by an account within an AWS region. Domain names start with a letter or
-- number and can contain the following characters: a-z (lowercase), 0-9,
-- and - (hyphen).
--
-- 'arn', 'domainStatus_arn' - The Amazon Resource Name (ARN) of a domain. See
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM identifiers>
-- in the /AWS Identity and Access Management User Guide/ for more
-- information.
--
-- 'clusterConfig', 'domainStatus_clusterConfig' - The type and number of instances in the domain.
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
      { eBSOptions = Prelude.Nothing,
        engineVersion = Prelude.Nothing,
        nodeToNodeEncryptionOptions = Prelude.Nothing,
        accessPolicies = Prelude.Nothing,
        serviceSoftwareOptions = Prelude.Nothing,
        autoTuneOptions = Prelude.Nothing,
        logPublishingOptions = Prelude.Nothing,
        advancedSecurityOptions = Prelude.Nothing,
        created = Prelude.Nothing,
        snapshotOptions = Prelude.Nothing,
        cognitoOptions = Prelude.Nothing,
        encryptionAtRestOptions = Prelude.Nothing,
        deleted = Prelude.Nothing,
        vPCOptions = Prelude.Nothing,
        endpoints = Prelude.Nothing,
        domainEndpointOptions = Prelude.Nothing,
        processing = Prelude.Nothing,
        endpoint = Prelude.Nothing,
        upgradeProcessing = Prelude.Nothing,
        advancedOptions = Prelude.Nothing,
        domainId = pDomainId_,
        domainName = pDomainName_,
        arn = pARN_,
        clusterConfig = pClusterConfig_
      }

-- | The @EBSOptions@ for the specified domain.
domainStatus_eBSOptions :: Lens.Lens' DomainStatus (Prelude.Maybe EBSOptions)
domainStatus_eBSOptions = Lens.lens (\DomainStatus' {eBSOptions} -> eBSOptions) (\s@DomainStatus' {} a -> s {eBSOptions = a} :: DomainStatus)

-- | Undocumented member.
domainStatus_engineVersion :: Lens.Lens' DomainStatus (Prelude.Maybe Prelude.Text)
domainStatus_engineVersion = Lens.lens (\DomainStatus' {engineVersion} -> engineVersion) (\s@DomainStatus' {} a -> s {engineVersion = a} :: DomainStatus)

-- | The status of the @NodeToNodeEncryptionOptions@.
domainStatus_nodeToNodeEncryptionOptions :: Lens.Lens' DomainStatus (Prelude.Maybe NodeToNodeEncryptionOptions)
domainStatus_nodeToNodeEncryptionOptions = Lens.lens (\DomainStatus' {nodeToNodeEncryptionOptions} -> nodeToNodeEncryptionOptions) (\s@DomainStatus' {} a -> s {nodeToNodeEncryptionOptions = a} :: DomainStatus)

-- | IAM access policy as a JSON-formatted string.
domainStatus_accessPolicies :: Lens.Lens' DomainStatus (Prelude.Maybe Prelude.Text)
domainStatus_accessPolicies = Lens.lens (\DomainStatus' {accessPolicies} -> accessPolicies) (\s@DomainStatus' {} a -> s {accessPolicies = a} :: DomainStatus)

-- | The current status of the domain\'s service software.
domainStatus_serviceSoftwareOptions :: Lens.Lens' DomainStatus (Prelude.Maybe ServiceSoftwareOptions)
domainStatus_serviceSoftwareOptions = Lens.lens (\DomainStatus' {serviceSoftwareOptions} -> serviceSoftwareOptions) (\s@DomainStatus' {} a -> s {serviceSoftwareOptions = a} :: DomainStatus)

-- | The current status of the domain\'s Auto-Tune options.
domainStatus_autoTuneOptions :: Lens.Lens' DomainStatus (Prelude.Maybe AutoTuneOptionsOutput)
domainStatus_autoTuneOptions = Lens.lens (\DomainStatus' {autoTuneOptions} -> autoTuneOptions) (\s@DomainStatus' {} a -> s {autoTuneOptions = a} :: DomainStatus)

-- | Log publishing options for the given domain.
domainStatus_logPublishingOptions :: Lens.Lens' DomainStatus (Prelude.Maybe (Prelude.HashMap LogType LogPublishingOption))
domainStatus_logPublishingOptions = Lens.lens (\DomainStatus' {logPublishingOptions} -> logPublishingOptions) (\s@DomainStatus' {} a -> s {logPublishingOptions = a} :: DomainStatus) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the domain\'s advanced security options.
domainStatus_advancedSecurityOptions :: Lens.Lens' DomainStatus (Prelude.Maybe AdvancedSecurityOptions)
domainStatus_advancedSecurityOptions = Lens.lens (\DomainStatus' {advancedSecurityOptions} -> advancedSecurityOptions) (\s@DomainStatus' {} a -> s {advancedSecurityOptions = a} :: DomainStatus)

-- | The domain creation status. @True@ if the creation of a domain is
-- complete. @ False @ if domain creation is still in progress.
domainStatus_created :: Lens.Lens' DomainStatus (Prelude.Maybe Prelude.Bool)
domainStatus_created = Lens.lens (\DomainStatus' {created} -> created) (\s@DomainStatus' {} a -> s {created = a} :: DomainStatus)

-- | The status of the @SnapshotOptions@.
domainStatus_snapshotOptions :: Lens.Lens' DomainStatus (Prelude.Maybe SnapshotOptions)
domainStatus_snapshotOptions = Lens.lens (\DomainStatus' {snapshotOptions} -> snapshotOptions) (\s@DomainStatus' {} a -> s {snapshotOptions = a} :: DomainStatus)

-- | The @CognitoOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/cognito-auth.html Configuring Amazon Cognito authentication for OpenSearch Dashboards>.
domainStatus_cognitoOptions :: Lens.Lens' DomainStatus (Prelude.Maybe CognitoOptions)
domainStatus_cognitoOptions = Lens.lens (\DomainStatus' {cognitoOptions} -> cognitoOptions) (\s@DomainStatus' {} a -> s {cognitoOptions = a} :: DomainStatus)

-- | The status of the @EncryptionAtRestOptions@.
domainStatus_encryptionAtRestOptions :: Lens.Lens' DomainStatus (Prelude.Maybe EncryptionAtRestOptions)
domainStatus_encryptionAtRestOptions = Lens.lens (\DomainStatus' {encryptionAtRestOptions} -> encryptionAtRestOptions) (\s@DomainStatus' {} a -> s {encryptionAtRestOptions = a} :: DomainStatus)

-- | The domain deletion status. @True@ if a delete request has been received
-- for the domain but resource cleanup is still in progress. @False@ if the
-- domain has not been deleted. Once domain deletion is complete, the
-- status of the domain is no longer returned.
domainStatus_deleted :: Lens.Lens' DomainStatus (Prelude.Maybe Prelude.Bool)
domainStatus_deleted = Lens.lens (\DomainStatus' {deleted} -> deleted) (\s@DomainStatus' {} a -> s {deleted = a} :: DomainStatus)

-- | The @VPCOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>.
domainStatus_vPCOptions :: Lens.Lens' DomainStatus (Prelude.Maybe VPCDerivedInfo)
domainStatus_vPCOptions = Lens.lens (\DomainStatus' {vPCOptions} -> vPCOptions) (\s@DomainStatus' {} a -> s {vPCOptions = a} :: DomainStatus)

-- | Map containing the domain endpoints used to submit index and search
-- requests. Example @key, value@:
-- @\'vpc\',\'vpc-endpoint-h2dsd34efgyghrtguk5gt6j2foh4.us-east-1.es.amazonaws.com\'@.
domainStatus_endpoints :: Lens.Lens' DomainStatus (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
domainStatus_endpoints = Lens.lens (\DomainStatus' {endpoints} -> endpoints) (\s@DomainStatus' {} a -> s {endpoints = a} :: DomainStatus) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the domain\'s endpoint options.
domainStatus_domainEndpointOptions :: Lens.Lens' DomainStatus (Prelude.Maybe DomainEndpointOptions)
domainStatus_domainEndpointOptions = Lens.lens (\DomainStatus' {domainEndpointOptions} -> domainEndpointOptions) (\s@DomainStatus' {} a -> s {domainEndpointOptions = a} :: DomainStatus)

-- | The status of the domain configuration. @True@ if Amazon OpenSearch
-- Service is processing configuration changes. @False@ if the
-- configuration is active.
domainStatus_processing :: Lens.Lens' DomainStatus (Prelude.Maybe Prelude.Bool)
domainStatus_processing = Lens.lens (\DomainStatus' {processing} -> processing) (\s@DomainStatus' {} a -> s {processing = a} :: DomainStatus)

-- | The domain endpoint that you use to submit index and search requests.
domainStatus_endpoint :: Lens.Lens' DomainStatus (Prelude.Maybe Prelude.Text)
domainStatus_endpoint = Lens.lens (\DomainStatus' {endpoint} -> endpoint) (\s@DomainStatus' {} a -> s {endpoint = a} :: DomainStatus)

-- | The status of a domain version upgrade. @True@ if Amazon OpenSearch
-- Service is undergoing a version upgrade. @False@ if the configuration is
-- active.
domainStatus_upgradeProcessing :: Lens.Lens' DomainStatus (Prelude.Maybe Prelude.Bool)
domainStatus_upgradeProcessing = Lens.lens (\DomainStatus' {upgradeProcessing} -> upgradeProcessing) (\s@DomainStatus' {} a -> s {upgradeProcessing = a} :: DomainStatus)

-- | The status of the @AdvancedOptions@.
domainStatus_advancedOptions :: Lens.Lens' DomainStatus (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
domainStatus_advancedOptions = Lens.lens (\DomainStatus' {advancedOptions} -> advancedOptions) (\s@DomainStatus' {} a -> s {advancedOptions = a} :: DomainStatus) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier for the specified domain.
domainStatus_domainId :: Lens.Lens' DomainStatus Prelude.Text
domainStatus_domainId = Lens.lens (\DomainStatus' {domainId} -> domainId) (\s@DomainStatus' {} a -> s {domainId = a} :: DomainStatus)

-- | The name of a domain. Domain names are unique across the domains owned
-- by an account within an AWS region. Domain names start with a letter or
-- number and can contain the following characters: a-z (lowercase), 0-9,
-- and - (hyphen).
domainStatus_domainName :: Lens.Lens' DomainStatus Prelude.Text
domainStatus_domainName = Lens.lens (\DomainStatus' {domainName} -> domainName) (\s@DomainStatus' {} a -> s {domainName = a} :: DomainStatus)

-- | The Amazon Resource Name (ARN) of a domain. See
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM identifiers>
-- in the /AWS Identity and Access Management User Guide/ for more
-- information.
domainStatus_arn :: Lens.Lens' DomainStatus Prelude.Text
domainStatus_arn = Lens.lens (\DomainStatus' {arn} -> arn) (\s@DomainStatus' {} a -> s {arn = a} :: DomainStatus)

-- | The type and number of instances in the domain.
domainStatus_clusterConfig :: Lens.Lens' DomainStatus ClusterConfig
domainStatus_clusterConfig = Lens.lens (\DomainStatus' {clusterConfig} -> clusterConfig) (\s@DomainStatus' {} a -> s {clusterConfig = a} :: DomainStatus)

instance Core.FromJSON DomainStatus where
  parseJSON =
    Core.withObject
      "DomainStatus"
      ( \x ->
          DomainStatus'
            Prelude.<$> (x Core..:? "EBSOptions")
            Prelude.<*> (x Core..:? "EngineVersion")
            Prelude.<*> (x Core..:? "NodeToNodeEncryptionOptions")
            Prelude.<*> (x Core..:? "AccessPolicies")
            Prelude.<*> (x Core..:? "ServiceSoftwareOptions")
            Prelude.<*> (x Core..:? "AutoTuneOptions")
            Prelude.<*> ( x Core..:? "LogPublishingOptions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "AdvancedSecurityOptions")
            Prelude.<*> (x Core..:? "Created")
            Prelude.<*> (x Core..:? "SnapshotOptions")
            Prelude.<*> (x Core..:? "CognitoOptions")
            Prelude.<*> (x Core..:? "EncryptionAtRestOptions")
            Prelude.<*> (x Core..:? "Deleted")
            Prelude.<*> (x Core..:? "VPCOptions")
            Prelude.<*> (x Core..:? "Endpoints" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "DomainEndpointOptions")
            Prelude.<*> (x Core..:? "Processing")
            Prelude.<*> (x Core..:? "Endpoint")
            Prelude.<*> (x Core..:? "UpgradeProcessing")
            Prelude.<*> ( x Core..:? "AdvancedOptions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "DomainId")
            Prelude.<*> (x Core..: "DomainName")
            Prelude.<*> (x Core..: "ARN")
            Prelude.<*> (x Core..: "ClusterConfig")
      )

instance Prelude.Hashable DomainStatus

instance Prelude.NFData DomainStatus
