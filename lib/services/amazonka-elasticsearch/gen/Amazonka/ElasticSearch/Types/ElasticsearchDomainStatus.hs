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
-- Module      : Amazonka.ElasticSearch.Types.ElasticsearchDomainStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.ElasticsearchDomainStatus where

import qualified Amazonka.Core as Core
import Amazonka.ElasticSearch.Types.AdvancedSecurityOptions
import Amazonka.ElasticSearch.Types.AutoTuneOptionsOutput
import Amazonka.ElasticSearch.Types.CognitoOptions
import Amazonka.ElasticSearch.Types.DomainEndpointOptions
import Amazonka.ElasticSearch.Types.EBSOptions
import Amazonka.ElasticSearch.Types.ElasticsearchClusterConfig
import Amazonka.ElasticSearch.Types.EncryptionAtRestOptions
import Amazonka.ElasticSearch.Types.LogPublishingOption
import Amazonka.ElasticSearch.Types.LogType
import Amazonka.ElasticSearch.Types.NodeToNodeEncryptionOptions
import Amazonka.ElasticSearch.Types.ServiceSoftwareOptions
import Amazonka.ElasticSearch.Types.SnapshotOptions
import Amazonka.ElasticSearch.Types.VPCDerivedInfo
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The current status of an Elasticsearch domain.
--
-- /See:/ 'newElasticsearchDomainStatus' smart constructor.
data ElasticsearchDomainStatus = ElasticsearchDomainStatus'
  { -- | The @EBSOptions@ for the specified domain. See
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs Configuring EBS-based Storage>
    -- for more information.
    eBSOptions :: Prelude.Maybe EBSOptions,
    -- | Specifies the status of the @NodeToNodeEncryptionOptions@.
    nodeToNodeEncryptionOptions :: Prelude.Maybe NodeToNodeEncryptionOptions,
    -- | IAM access policy as a JSON-formatted string.
    accessPolicies :: Prelude.Maybe Prelude.Text,
    -- | The current status of the Elasticsearch domain\'s service software.
    serviceSoftwareOptions :: Prelude.Maybe ServiceSoftwareOptions,
    -- | The current status of the Elasticsearch domain\'s Auto-Tune options.
    autoTuneOptions :: Prelude.Maybe AutoTuneOptionsOutput,
    -- | Log publishing options for the given domain.
    logPublishingOptions :: Prelude.Maybe (Prelude.HashMap LogType LogPublishingOption),
    -- | The current status of the Elasticsearch domain\'s advanced security
    -- options.
    advancedSecurityOptions :: Prelude.Maybe AdvancedSecurityOptions,
    -- | The domain creation status. @True@ if the creation of an Elasticsearch
    -- domain is complete. @False@ if domain creation is still in progress.
    created :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the status of the @SnapshotOptions@
    snapshotOptions :: Prelude.Maybe SnapshotOptions,
    -- | The @CognitoOptions@ for the specified domain. For more information, see
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
    cognitoOptions :: Prelude.Maybe CognitoOptions,
    -- | Specifies the status of the @EncryptionAtRestOptions@.
    encryptionAtRestOptions :: Prelude.Maybe EncryptionAtRestOptions,
    -- | The domain deletion status. @True@ if a delete request has been received
    -- for the domain but resource cleanup is still in progress. @False@ if the
    -- domain has not been deleted. Once domain deletion is complete, the
    -- status of the domain is no longer returned.
    deleted :: Prelude.Maybe Prelude.Bool,
    -- | The @VPCOptions@ for the specified domain. For more information, see
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains>.
    vPCOptions :: Prelude.Maybe VPCDerivedInfo,
    -- | Map containing the Elasticsearch domain endpoints used to submit index
    -- and search requests. Example @key, value@:
    -- @\'vpc\',\'vpc-endpoint-h2dsd34efgyghrtguk5gt6j2foh4.us-east-1.es.amazonaws.com\'@.
    endpoints :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The current status of the Elasticsearch domain\'s endpoint options.
    domainEndpointOptions :: Prelude.Maybe DomainEndpointOptions,
    -- | The status of the Elasticsearch domain configuration. @True@ if Amazon
    -- Elasticsearch Service is processing configuration changes. @False@ if
    -- the configuration is active.
    processing :: Prelude.Maybe Prelude.Bool,
    -- | The Elasticsearch domain endpoint that you use to submit index and
    -- search requests.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The status of an Elasticsearch domain version upgrade. @True@ if Amazon
    -- Elasticsearch Service is undergoing a version upgrade. @False@ if the
    -- configuration is active.
    upgradeProcessing :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the status of the @AdvancedOptions@
    advancedOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    elasticsearchVersion :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the specified Elasticsearch domain.
    domainId :: Prelude.Text,
    -- | The name of an Elasticsearch domain. Domain names are unique across the
    -- domains owned by an account within an AWS region. Domain names start
    -- with a letter or number and can contain the following characters: a-z
    -- (lowercase), 0-9, and - (hyphen).
    domainName :: Prelude.Text,
    -- | The Amazon resource name (ARN) of an Elasticsearch domain. See
    -- <http://docs.aws.amazon.com/IAM/latest/UserGuide/index.html?Using_Identifiers.html Identifiers for IAM Entities>
    -- in /Using AWS Identity and Access Management/ for more information.
    arn :: Prelude.Text,
    -- | The type and number of instances in the domain cluster.
    elasticsearchClusterConfig :: ElasticsearchClusterConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ElasticsearchDomainStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eBSOptions', 'elasticsearchDomainStatus_eBSOptions' - The @EBSOptions@ for the specified domain. See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs Configuring EBS-based Storage>
-- for more information.
--
-- 'nodeToNodeEncryptionOptions', 'elasticsearchDomainStatus_nodeToNodeEncryptionOptions' - Specifies the status of the @NodeToNodeEncryptionOptions@.
--
-- 'accessPolicies', 'elasticsearchDomainStatus_accessPolicies' - IAM access policy as a JSON-formatted string.
--
-- 'serviceSoftwareOptions', 'elasticsearchDomainStatus_serviceSoftwareOptions' - The current status of the Elasticsearch domain\'s service software.
--
-- 'autoTuneOptions', 'elasticsearchDomainStatus_autoTuneOptions' - The current status of the Elasticsearch domain\'s Auto-Tune options.
--
-- 'logPublishingOptions', 'elasticsearchDomainStatus_logPublishingOptions' - Log publishing options for the given domain.
--
-- 'advancedSecurityOptions', 'elasticsearchDomainStatus_advancedSecurityOptions' - The current status of the Elasticsearch domain\'s advanced security
-- options.
--
-- 'created', 'elasticsearchDomainStatus_created' - The domain creation status. @True@ if the creation of an Elasticsearch
-- domain is complete. @False@ if domain creation is still in progress.
--
-- 'snapshotOptions', 'elasticsearchDomainStatus_snapshotOptions' - Specifies the status of the @SnapshotOptions@
--
-- 'cognitoOptions', 'elasticsearchDomainStatus_cognitoOptions' - The @CognitoOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
--
-- 'encryptionAtRestOptions', 'elasticsearchDomainStatus_encryptionAtRestOptions' - Specifies the status of the @EncryptionAtRestOptions@.
--
-- 'deleted', 'elasticsearchDomainStatus_deleted' - The domain deletion status. @True@ if a delete request has been received
-- for the domain but resource cleanup is still in progress. @False@ if the
-- domain has not been deleted. Once domain deletion is complete, the
-- status of the domain is no longer returned.
--
-- 'vPCOptions', 'elasticsearchDomainStatus_vPCOptions' - The @VPCOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains>.
--
-- 'endpoints', 'elasticsearchDomainStatus_endpoints' - Map containing the Elasticsearch domain endpoints used to submit index
-- and search requests. Example @key, value@:
-- @\'vpc\',\'vpc-endpoint-h2dsd34efgyghrtguk5gt6j2foh4.us-east-1.es.amazonaws.com\'@.
--
-- 'domainEndpointOptions', 'elasticsearchDomainStatus_domainEndpointOptions' - The current status of the Elasticsearch domain\'s endpoint options.
--
-- 'processing', 'elasticsearchDomainStatus_processing' - The status of the Elasticsearch domain configuration. @True@ if Amazon
-- Elasticsearch Service is processing configuration changes. @False@ if
-- the configuration is active.
--
-- 'endpoint', 'elasticsearchDomainStatus_endpoint' - The Elasticsearch domain endpoint that you use to submit index and
-- search requests.
--
-- 'upgradeProcessing', 'elasticsearchDomainStatus_upgradeProcessing' - The status of an Elasticsearch domain version upgrade. @True@ if Amazon
-- Elasticsearch Service is undergoing a version upgrade. @False@ if the
-- configuration is active.
--
-- 'advancedOptions', 'elasticsearchDomainStatus_advancedOptions' - Specifies the status of the @AdvancedOptions@
--
-- 'elasticsearchVersion', 'elasticsearchDomainStatus_elasticsearchVersion' - Undocumented member.
--
-- 'domainId', 'elasticsearchDomainStatus_domainId' - The unique identifier for the specified Elasticsearch domain.
--
-- 'domainName', 'elasticsearchDomainStatus_domainName' - The name of an Elasticsearch domain. Domain names are unique across the
-- domains owned by an account within an AWS region. Domain names start
-- with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
--
-- 'arn', 'elasticsearchDomainStatus_arn' - The Amazon resource name (ARN) of an Elasticsearch domain. See
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/index.html?Using_Identifiers.html Identifiers for IAM Entities>
-- in /Using AWS Identity and Access Management/ for more information.
--
-- 'elasticsearchClusterConfig', 'elasticsearchDomainStatus_elasticsearchClusterConfig' - The type and number of instances in the domain cluster.
newElasticsearchDomainStatus ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'elasticsearchClusterConfig'
  ElasticsearchClusterConfig ->
  ElasticsearchDomainStatus
newElasticsearchDomainStatus
  pDomainId_
  pDomainName_
  pARN_
  pElasticsearchClusterConfig_ =
    ElasticsearchDomainStatus'
      { eBSOptions =
          Prelude.Nothing,
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
        elasticsearchVersion = Prelude.Nothing,
        domainId = pDomainId_,
        domainName = pDomainName_,
        arn = pARN_,
        elasticsearchClusterConfig =
          pElasticsearchClusterConfig_
      }

-- | The @EBSOptions@ for the specified domain. See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs Configuring EBS-based Storage>
-- for more information.
elasticsearchDomainStatus_eBSOptions :: Lens.Lens' ElasticsearchDomainStatus (Prelude.Maybe EBSOptions)
elasticsearchDomainStatus_eBSOptions = Lens.lens (\ElasticsearchDomainStatus' {eBSOptions} -> eBSOptions) (\s@ElasticsearchDomainStatus' {} a -> s {eBSOptions = a} :: ElasticsearchDomainStatus)

-- | Specifies the status of the @NodeToNodeEncryptionOptions@.
elasticsearchDomainStatus_nodeToNodeEncryptionOptions :: Lens.Lens' ElasticsearchDomainStatus (Prelude.Maybe NodeToNodeEncryptionOptions)
elasticsearchDomainStatus_nodeToNodeEncryptionOptions = Lens.lens (\ElasticsearchDomainStatus' {nodeToNodeEncryptionOptions} -> nodeToNodeEncryptionOptions) (\s@ElasticsearchDomainStatus' {} a -> s {nodeToNodeEncryptionOptions = a} :: ElasticsearchDomainStatus)

-- | IAM access policy as a JSON-formatted string.
elasticsearchDomainStatus_accessPolicies :: Lens.Lens' ElasticsearchDomainStatus (Prelude.Maybe Prelude.Text)
elasticsearchDomainStatus_accessPolicies = Lens.lens (\ElasticsearchDomainStatus' {accessPolicies} -> accessPolicies) (\s@ElasticsearchDomainStatus' {} a -> s {accessPolicies = a} :: ElasticsearchDomainStatus)

-- | The current status of the Elasticsearch domain\'s service software.
elasticsearchDomainStatus_serviceSoftwareOptions :: Lens.Lens' ElasticsearchDomainStatus (Prelude.Maybe ServiceSoftwareOptions)
elasticsearchDomainStatus_serviceSoftwareOptions = Lens.lens (\ElasticsearchDomainStatus' {serviceSoftwareOptions} -> serviceSoftwareOptions) (\s@ElasticsearchDomainStatus' {} a -> s {serviceSoftwareOptions = a} :: ElasticsearchDomainStatus)

-- | The current status of the Elasticsearch domain\'s Auto-Tune options.
elasticsearchDomainStatus_autoTuneOptions :: Lens.Lens' ElasticsearchDomainStatus (Prelude.Maybe AutoTuneOptionsOutput)
elasticsearchDomainStatus_autoTuneOptions = Lens.lens (\ElasticsearchDomainStatus' {autoTuneOptions} -> autoTuneOptions) (\s@ElasticsearchDomainStatus' {} a -> s {autoTuneOptions = a} :: ElasticsearchDomainStatus)

-- | Log publishing options for the given domain.
elasticsearchDomainStatus_logPublishingOptions :: Lens.Lens' ElasticsearchDomainStatus (Prelude.Maybe (Prelude.HashMap LogType LogPublishingOption))
elasticsearchDomainStatus_logPublishingOptions = Lens.lens (\ElasticsearchDomainStatus' {logPublishingOptions} -> logPublishingOptions) (\s@ElasticsearchDomainStatus' {} a -> s {logPublishingOptions = a} :: ElasticsearchDomainStatus) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the Elasticsearch domain\'s advanced security
-- options.
elasticsearchDomainStatus_advancedSecurityOptions :: Lens.Lens' ElasticsearchDomainStatus (Prelude.Maybe AdvancedSecurityOptions)
elasticsearchDomainStatus_advancedSecurityOptions = Lens.lens (\ElasticsearchDomainStatus' {advancedSecurityOptions} -> advancedSecurityOptions) (\s@ElasticsearchDomainStatus' {} a -> s {advancedSecurityOptions = a} :: ElasticsearchDomainStatus)

-- | The domain creation status. @True@ if the creation of an Elasticsearch
-- domain is complete. @False@ if domain creation is still in progress.
elasticsearchDomainStatus_created :: Lens.Lens' ElasticsearchDomainStatus (Prelude.Maybe Prelude.Bool)
elasticsearchDomainStatus_created = Lens.lens (\ElasticsearchDomainStatus' {created} -> created) (\s@ElasticsearchDomainStatus' {} a -> s {created = a} :: ElasticsearchDomainStatus)

-- | Specifies the status of the @SnapshotOptions@
elasticsearchDomainStatus_snapshotOptions :: Lens.Lens' ElasticsearchDomainStatus (Prelude.Maybe SnapshotOptions)
elasticsearchDomainStatus_snapshotOptions = Lens.lens (\ElasticsearchDomainStatus' {snapshotOptions} -> snapshotOptions) (\s@ElasticsearchDomainStatus' {} a -> s {snapshotOptions = a} :: ElasticsearchDomainStatus)

-- | The @CognitoOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana>.
elasticsearchDomainStatus_cognitoOptions :: Lens.Lens' ElasticsearchDomainStatus (Prelude.Maybe CognitoOptions)
elasticsearchDomainStatus_cognitoOptions = Lens.lens (\ElasticsearchDomainStatus' {cognitoOptions} -> cognitoOptions) (\s@ElasticsearchDomainStatus' {} a -> s {cognitoOptions = a} :: ElasticsearchDomainStatus)

-- | Specifies the status of the @EncryptionAtRestOptions@.
elasticsearchDomainStatus_encryptionAtRestOptions :: Lens.Lens' ElasticsearchDomainStatus (Prelude.Maybe EncryptionAtRestOptions)
elasticsearchDomainStatus_encryptionAtRestOptions = Lens.lens (\ElasticsearchDomainStatus' {encryptionAtRestOptions} -> encryptionAtRestOptions) (\s@ElasticsearchDomainStatus' {} a -> s {encryptionAtRestOptions = a} :: ElasticsearchDomainStatus)

-- | The domain deletion status. @True@ if a delete request has been received
-- for the domain but resource cleanup is still in progress. @False@ if the
-- domain has not been deleted. Once domain deletion is complete, the
-- status of the domain is no longer returned.
elasticsearchDomainStatus_deleted :: Lens.Lens' ElasticsearchDomainStatus (Prelude.Maybe Prelude.Bool)
elasticsearchDomainStatus_deleted = Lens.lens (\ElasticsearchDomainStatus' {deleted} -> deleted) (\s@ElasticsearchDomainStatus' {} a -> s {deleted = a} :: ElasticsearchDomainStatus)

-- | The @VPCOptions@ for the specified domain. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains>.
elasticsearchDomainStatus_vPCOptions :: Lens.Lens' ElasticsearchDomainStatus (Prelude.Maybe VPCDerivedInfo)
elasticsearchDomainStatus_vPCOptions = Lens.lens (\ElasticsearchDomainStatus' {vPCOptions} -> vPCOptions) (\s@ElasticsearchDomainStatus' {} a -> s {vPCOptions = a} :: ElasticsearchDomainStatus)

-- | Map containing the Elasticsearch domain endpoints used to submit index
-- and search requests. Example @key, value@:
-- @\'vpc\',\'vpc-endpoint-h2dsd34efgyghrtguk5gt6j2foh4.us-east-1.es.amazonaws.com\'@.
elasticsearchDomainStatus_endpoints :: Lens.Lens' ElasticsearchDomainStatus (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
elasticsearchDomainStatus_endpoints = Lens.lens (\ElasticsearchDomainStatus' {endpoints} -> endpoints) (\s@ElasticsearchDomainStatus' {} a -> s {endpoints = a} :: ElasticsearchDomainStatus) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the Elasticsearch domain\'s endpoint options.
elasticsearchDomainStatus_domainEndpointOptions :: Lens.Lens' ElasticsearchDomainStatus (Prelude.Maybe DomainEndpointOptions)
elasticsearchDomainStatus_domainEndpointOptions = Lens.lens (\ElasticsearchDomainStatus' {domainEndpointOptions} -> domainEndpointOptions) (\s@ElasticsearchDomainStatus' {} a -> s {domainEndpointOptions = a} :: ElasticsearchDomainStatus)

-- | The status of the Elasticsearch domain configuration. @True@ if Amazon
-- Elasticsearch Service is processing configuration changes. @False@ if
-- the configuration is active.
elasticsearchDomainStatus_processing :: Lens.Lens' ElasticsearchDomainStatus (Prelude.Maybe Prelude.Bool)
elasticsearchDomainStatus_processing = Lens.lens (\ElasticsearchDomainStatus' {processing} -> processing) (\s@ElasticsearchDomainStatus' {} a -> s {processing = a} :: ElasticsearchDomainStatus)

-- | The Elasticsearch domain endpoint that you use to submit index and
-- search requests.
elasticsearchDomainStatus_endpoint :: Lens.Lens' ElasticsearchDomainStatus (Prelude.Maybe Prelude.Text)
elasticsearchDomainStatus_endpoint = Lens.lens (\ElasticsearchDomainStatus' {endpoint} -> endpoint) (\s@ElasticsearchDomainStatus' {} a -> s {endpoint = a} :: ElasticsearchDomainStatus)

-- | The status of an Elasticsearch domain version upgrade. @True@ if Amazon
-- Elasticsearch Service is undergoing a version upgrade. @False@ if the
-- configuration is active.
elasticsearchDomainStatus_upgradeProcessing :: Lens.Lens' ElasticsearchDomainStatus (Prelude.Maybe Prelude.Bool)
elasticsearchDomainStatus_upgradeProcessing = Lens.lens (\ElasticsearchDomainStatus' {upgradeProcessing} -> upgradeProcessing) (\s@ElasticsearchDomainStatus' {} a -> s {upgradeProcessing = a} :: ElasticsearchDomainStatus)

-- | Specifies the status of the @AdvancedOptions@
elasticsearchDomainStatus_advancedOptions :: Lens.Lens' ElasticsearchDomainStatus (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
elasticsearchDomainStatus_advancedOptions = Lens.lens (\ElasticsearchDomainStatus' {advancedOptions} -> advancedOptions) (\s@ElasticsearchDomainStatus' {} a -> s {advancedOptions = a} :: ElasticsearchDomainStatus) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
elasticsearchDomainStatus_elasticsearchVersion :: Lens.Lens' ElasticsearchDomainStatus (Prelude.Maybe Prelude.Text)
elasticsearchDomainStatus_elasticsearchVersion = Lens.lens (\ElasticsearchDomainStatus' {elasticsearchVersion} -> elasticsearchVersion) (\s@ElasticsearchDomainStatus' {} a -> s {elasticsearchVersion = a} :: ElasticsearchDomainStatus)

-- | The unique identifier for the specified Elasticsearch domain.
elasticsearchDomainStatus_domainId :: Lens.Lens' ElasticsearchDomainStatus Prelude.Text
elasticsearchDomainStatus_domainId = Lens.lens (\ElasticsearchDomainStatus' {domainId} -> domainId) (\s@ElasticsearchDomainStatus' {} a -> s {domainId = a} :: ElasticsearchDomainStatus)

-- | The name of an Elasticsearch domain. Domain names are unique across the
-- domains owned by an account within an AWS region. Domain names start
-- with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
elasticsearchDomainStatus_domainName :: Lens.Lens' ElasticsearchDomainStatus Prelude.Text
elasticsearchDomainStatus_domainName = Lens.lens (\ElasticsearchDomainStatus' {domainName} -> domainName) (\s@ElasticsearchDomainStatus' {} a -> s {domainName = a} :: ElasticsearchDomainStatus)

-- | The Amazon resource name (ARN) of an Elasticsearch domain. See
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/index.html?Using_Identifiers.html Identifiers for IAM Entities>
-- in /Using AWS Identity and Access Management/ for more information.
elasticsearchDomainStatus_arn :: Lens.Lens' ElasticsearchDomainStatus Prelude.Text
elasticsearchDomainStatus_arn = Lens.lens (\ElasticsearchDomainStatus' {arn} -> arn) (\s@ElasticsearchDomainStatus' {} a -> s {arn = a} :: ElasticsearchDomainStatus)

-- | The type and number of instances in the domain cluster.
elasticsearchDomainStatus_elasticsearchClusterConfig :: Lens.Lens' ElasticsearchDomainStatus ElasticsearchClusterConfig
elasticsearchDomainStatus_elasticsearchClusterConfig = Lens.lens (\ElasticsearchDomainStatus' {elasticsearchClusterConfig} -> elasticsearchClusterConfig) (\s@ElasticsearchDomainStatus' {} a -> s {elasticsearchClusterConfig = a} :: ElasticsearchDomainStatus)

instance Core.FromJSON ElasticsearchDomainStatus where
  parseJSON =
    Core.withObject
      "ElasticsearchDomainStatus"
      ( \x ->
          ElasticsearchDomainStatus'
            Prelude.<$> (x Core..:? "EBSOptions")
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
            Prelude.<*> (x Core..:? "ElasticsearchVersion")
            Prelude.<*> (x Core..: "DomainId")
            Prelude.<*> (x Core..: "DomainName")
            Prelude.<*> (x Core..: "ARN")
            Prelude.<*> (x Core..: "ElasticsearchClusterConfig")
      )

instance Prelude.Hashable ElasticsearchDomainStatus

instance Prelude.NFData ElasticsearchDomainStatus
