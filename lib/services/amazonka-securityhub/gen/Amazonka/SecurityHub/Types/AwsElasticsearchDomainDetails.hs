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
-- Module      : Amazonka.SecurityHub.Types.AwsElasticsearchDomainDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElasticsearchDomainDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainDomainEndpointOptions
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainElasticsearchClusterConfigDetails
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainEncryptionAtRestOptions
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainLogPublishingOptions
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainNodeToNodeEncryptionOptions
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainServiceSoftwareOptions
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainVPCOptions

-- | Information about an Elasticsearch domain.
--
-- /See:/ 'newAwsElasticsearchDomainDetails' smart constructor.
data AwsElasticsearchDomainDetails = AwsElasticsearchDomainDetails'
  { -- | Details about the configuration for node-to-node encryption.
    nodeToNodeEncryptionOptions :: Prelude.Maybe AwsElasticsearchDomainNodeToNodeEncryptionOptions,
    -- | IAM policy document specifying the access policies for the new
    -- Elasticsearch domain.
    accessPolicies :: Prelude.Maybe Prelude.Text,
    -- | Information about the status of a domain relative to the latest service
    -- software.
    serviceSoftwareOptions :: Prelude.Maybe AwsElasticsearchDomainServiceSoftwareOptions,
    -- | Configures the CloudWatch Logs to publish for the Elasticsearch domain.
    logPublishingOptions :: Prelude.Maybe AwsElasticsearchDomainLogPublishingOptions,
    -- | Information about an OpenSearch cluster configuration.
    elasticsearchClusterConfig :: Prelude.Maybe AwsElasticsearchDomainElasticsearchClusterConfigDetails,
    -- | Name of an Elasticsearch domain.
    --
    -- Domain names are unique across all domains owned by the same account
    -- within an Amazon Web Services Region.
    --
    -- Domain names must start with a lowercase letter and must be between 3
    -- and 28 characters.
    --
    -- Valid characters are a-z (lowercase only), 0-9, and – (hyphen).
    domainName :: Prelude.Maybe Prelude.Text,
    -- | Details about the configuration for encryption at rest.
    encryptionAtRestOptions :: Prelude.Maybe AwsElasticsearchDomainEncryptionAtRestOptions,
    -- | Information that OpenSearch derives based on @VPCOptions@ for the
    -- domain.
    vPCOptions :: Prelude.Maybe AwsElasticsearchDomainVPCOptions,
    -- | Unique identifier for an Elasticsearch domain.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The key-value pair that exists if the Elasticsearch domain uses VPC
    -- endpoints.
    endpoints :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Additional options for the domain endpoint.
    domainEndpointOptions :: Prelude.Maybe AwsElasticsearchDomainDomainEndpointOptions,
    -- | Domain-specific endpoint used to submit index, search, and data upload
    -- requests to an Elasticsearch domain.
    --
    -- The endpoint is a service URL.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | OpenSearch version.
    elasticsearchVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElasticsearchDomainDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeToNodeEncryptionOptions', 'awsElasticsearchDomainDetails_nodeToNodeEncryptionOptions' - Details about the configuration for node-to-node encryption.
--
-- 'accessPolicies', 'awsElasticsearchDomainDetails_accessPolicies' - IAM policy document specifying the access policies for the new
-- Elasticsearch domain.
--
-- 'serviceSoftwareOptions', 'awsElasticsearchDomainDetails_serviceSoftwareOptions' - Information about the status of a domain relative to the latest service
-- software.
--
-- 'logPublishingOptions', 'awsElasticsearchDomainDetails_logPublishingOptions' - Configures the CloudWatch Logs to publish for the Elasticsearch domain.
--
-- 'elasticsearchClusterConfig', 'awsElasticsearchDomainDetails_elasticsearchClusterConfig' - Information about an OpenSearch cluster configuration.
--
-- 'domainName', 'awsElasticsearchDomainDetails_domainName' - Name of an Elasticsearch domain.
--
-- Domain names are unique across all domains owned by the same account
-- within an Amazon Web Services Region.
--
-- Domain names must start with a lowercase letter and must be between 3
-- and 28 characters.
--
-- Valid characters are a-z (lowercase only), 0-9, and – (hyphen).
--
-- 'encryptionAtRestOptions', 'awsElasticsearchDomainDetails_encryptionAtRestOptions' - Details about the configuration for encryption at rest.
--
-- 'vPCOptions', 'awsElasticsearchDomainDetails_vPCOptions' - Information that OpenSearch derives based on @VPCOptions@ for the
-- domain.
--
-- 'domainId', 'awsElasticsearchDomainDetails_domainId' - Unique identifier for an Elasticsearch domain.
--
-- 'endpoints', 'awsElasticsearchDomainDetails_endpoints' - The key-value pair that exists if the Elasticsearch domain uses VPC
-- endpoints.
--
-- 'domainEndpointOptions', 'awsElasticsearchDomainDetails_domainEndpointOptions' - Additional options for the domain endpoint.
--
-- 'endpoint', 'awsElasticsearchDomainDetails_endpoint' - Domain-specific endpoint used to submit index, search, and data upload
-- requests to an Elasticsearch domain.
--
-- The endpoint is a service URL.
--
-- 'elasticsearchVersion', 'awsElasticsearchDomainDetails_elasticsearchVersion' - OpenSearch version.
newAwsElasticsearchDomainDetails ::
  AwsElasticsearchDomainDetails
newAwsElasticsearchDomainDetails =
  AwsElasticsearchDomainDetails'
    { nodeToNodeEncryptionOptions =
        Prelude.Nothing,
      accessPolicies = Prelude.Nothing,
      serviceSoftwareOptions = Prelude.Nothing,
      logPublishingOptions = Prelude.Nothing,
      elasticsearchClusterConfig = Prelude.Nothing,
      domainName = Prelude.Nothing,
      encryptionAtRestOptions = Prelude.Nothing,
      vPCOptions = Prelude.Nothing,
      domainId = Prelude.Nothing,
      endpoints = Prelude.Nothing,
      domainEndpointOptions = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      elasticsearchVersion = Prelude.Nothing
    }

-- | Details about the configuration for node-to-node encryption.
awsElasticsearchDomainDetails_nodeToNodeEncryptionOptions :: Lens.Lens' AwsElasticsearchDomainDetails (Prelude.Maybe AwsElasticsearchDomainNodeToNodeEncryptionOptions)
awsElasticsearchDomainDetails_nodeToNodeEncryptionOptions = Lens.lens (\AwsElasticsearchDomainDetails' {nodeToNodeEncryptionOptions} -> nodeToNodeEncryptionOptions) (\s@AwsElasticsearchDomainDetails' {} a -> s {nodeToNodeEncryptionOptions = a} :: AwsElasticsearchDomainDetails)

-- | IAM policy document specifying the access policies for the new
-- Elasticsearch domain.
awsElasticsearchDomainDetails_accessPolicies :: Lens.Lens' AwsElasticsearchDomainDetails (Prelude.Maybe Prelude.Text)
awsElasticsearchDomainDetails_accessPolicies = Lens.lens (\AwsElasticsearchDomainDetails' {accessPolicies} -> accessPolicies) (\s@AwsElasticsearchDomainDetails' {} a -> s {accessPolicies = a} :: AwsElasticsearchDomainDetails)

-- | Information about the status of a domain relative to the latest service
-- software.
awsElasticsearchDomainDetails_serviceSoftwareOptions :: Lens.Lens' AwsElasticsearchDomainDetails (Prelude.Maybe AwsElasticsearchDomainServiceSoftwareOptions)
awsElasticsearchDomainDetails_serviceSoftwareOptions = Lens.lens (\AwsElasticsearchDomainDetails' {serviceSoftwareOptions} -> serviceSoftwareOptions) (\s@AwsElasticsearchDomainDetails' {} a -> s {serviceSoftwareOptions = a} :: AwsElasticsearchDomainDetails)

-- | Configures the CloudWatch Logs to publish for the Elasticsearch domain.
awsElasticsearchDomainDetails_logPublishingOptions :: Lens.Lens' AwsElasticsearchDomainDetails (Prelude.Maybe AwsElasticsearchDomainLogPublishingOptions)
awsElasticsearchDomainDetails_logPublishingOptions = Lens.lens (\AwsElasticsearchDomainDetails' {logPublishingOptions} -> logPublishingOptions) (\s@AwsElasticsearchDomainDetails' {} a -> s {logPublishingOptions = a} :: AwsElasticsearchDomainDetails)

-- | Information about an OpenSearch cluster configuration.
awsElasticsearchDomainDetails_elasticsearchClusterConfig :: Lens.Lens' AwsElasticsearchDomainDetails (Prelude.Maybe AwsElasticsearchDomainElasticsearchClusterConfigDetails)
awsElasticsearchDomainDetails_elasticsearchClusterConfig = Lens.lens (\AwsElasticsearchDomainDetails' {elasticsearchClusterConfig} -> elasticsearchClusterConfig) (\s@AwsElasticsearchDomainDetails' {} a -> s {elasticsearchClusterConfig = a} :: AwsElasticsearchDomainDetails)

-- | Name of an Elasticsearch domain.
--
-- Domain names are unique across all domains owned by the same account
-- within an Amazon Web Services Region.
--
-- Domain names must start with a lowercase letter and must be between 3
-- and 28 characters.
--
-- Valid characters are a-z (lowercase only), 0-9, and – (hyphen).
awsElasticsearchDomainDetails_domainName :: Lens.Lens' AwsElasticsearchDomainDetails (Prelude.Maybe Prelude.Text)
awsElasticsearchDomainDetails_domainName = Lens.lens (\AwsElasticsearchDomainDetails' {domainName} -> domainName) (\s@AwsElasticsearchDomainDetails' {} a -> s {domainName = a} :: AwsElasticsearchDomainDetails)

-- | Details about the configuration for encryption at rest.
awsElasticsearchDomainDetails_encryptionAtRestOptions :: Lens.Lens' AwsElasticsearchDomainDetails (Prelude.Maybe AwsElasticsearchDomainEncryptionAtRestOptions)
awsElasticsearchDomainDetails_encryptionAtRestOptions = Lens.lens (\AwsElasticsearchDomainDetails' {encryptionAtRestOptions} -> encryptionAtRestOptions) (\s@AwsElasticsearchDomainDetails' {} a -> s {encryptionAtRestOptions = a} :: AwsElasticsearchDomainDetails)

-- | Information that OpenSearch derives based on @VPCOptions@ for the
-- domain.
awsElasticsearchDomainDetails_vPCOptions :: Lens.Lens' AwsElasticsearchDomainDetails (Prelude.Maybe AwsElasticsearchDomainVPCOptions)
awsElasticsearchDomainDetails_vPCOptions = Lens.lens (\AwsElasticsearchDomainDetails' {vPCOptions} -> vPCOptions) (\s@AwsElasticsearchDomainDetails' {} a -> s {vPCOptions = a} :: AwsElasticsearchDomainDetails)

-- | Unique identifier for an Elasticsearch domain.
awsElasticsearchDomainDetails_domainId :: Lens.Lens' AwsElasticsearchDomainDetails (Prelude.Maybe Prelude.Text)
awsElasticsearchDomainDetails_domainId = Lens.lens (\AwsElasticsearchDomainDetails' {domainId} -> domainId) (\s@AwsElasticsearchDomainDetails' {} a -> s {domainId = a} :: AwsElasticsearchDomainDetails)

-- | The key-value pair that exists if the Elasticsearch domain uses VPC
-- endpoints.
awsElasticsearchDomainDetails_endpoints :: Lens.Lens' AwsElasticsearchDomainDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
awsElasticsearchDomainDetails_endpoints = Lens.lens (\AwsElasticsearchDomainDetails' {endpoints} -> endpoints) (\s@AwsElasticsearchDomainDetails' {} a -> s {endpoints = a} :: AwsElasticsearchDomainDetails) Prelude.. Lens.mapping Lens.coerced

-- | Additional options for the domain endpoint.
awsElasticsearchDomainDetails_domainEndpointOptions :: Lens.Lens' AwsElasticsearchDomainDetails (Prelude.Maybe AwsElasticsearchDomainDomainEndpointOptions)
awsElasticsearchDomainDetails_domainEndpointOptions = Lens.lens (\AwsElasticsearchDomainDetails' {domainEndpointOptions} -> domainEndpointOptions) (\s@AwsElasticsearchDomainDetails' {} a -> s {domainEndpointOptions = a} :: AwsElasticsearchDomainDetails)

-- | Domain-specific endpoint used to submit index, search, and data upload
-- requests to an Elasticsearch domain.
--
-- The endpoint is a service URL.
awsElasticsearchDomainDetails_endpoint :: Lens.Lens' AwsElasticsearchDomainDetails (Prelude.Maybe Prelude.Text)
awsElasticsearchDomainDetails_endpoint = Lens.lens (\AwsElasticsearchDomainDetails' {endpoint} -> endpoint) (\s@AwsElasticsearchDomainDetails' {} a -> s {endpoint = a} :: AwsElasticsearchDomainDetails)

-- | OpenSearch version.
awsElasticsearchDomainDetails_elasticsearchVersion :: Lens.Lens' AwsElasticsearchDomainDetails (Prelude.Maybe Prelude.Text)
awsElasticsearchDomainDetails_elasticsearchVersion = Lens.lens (\AwsElasticsearchDomainDetails' {elasticsearchVersion} -> elasticsearchVersion) (\s@AwsElasticsearchDomainDetails' {} a -> s {elasticsearchVersion = a} :: AwsElasticsearchDomainDetails)

instance Core.FromJSON AwsElasticsearchDomainDetails where
  parseJSON =
    Core.withObject
      "AwsElasticsearchDomainDetails"
      ( \x ->
          AwsElasticsearchDomainDetails'
            Prelude.<$> (x Core..:? "NodeToNodeEncryptionOptions")
            Prelude.<*> (x Core..:? "AccessPolicies")
            Prelude.<*> (x Core..:? "ServiceSoftwareOptions")
            Prelude.<*> (x Core..:? "LogPublishingOptions")
            Prelude.<*> (x Core..:? "ElasticsearchClusterConfig")
            Prelude.<*> (x Core..:? "DomainName")
            Prelude.<*> (x Core..:? "EncryptionAtRestOptions")
            Prelude.<*> (x Core..:? "VPCOptions")
            Prelude.<*> (x Core..:? "DomainId")
            Prelude.<*> (x Core..:? "Endpoints" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "DomainEndpointOptions")
            Prelude.<*> (x Core..:? "Endpoint")
            Prelude.<*> (x Core..:? "ElasticsearchVersion")
      )

instance
  Prelude.Hashable
    AwsElasticsearchDomainDetails

instance Prelude.NFData AwsElasticsearchDomainDetails

instance Core.ToJSON AwsElasticsearchDomainDetails where
  toJSON AwsElasticsearchDomainDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NodeToNodeEncryptionOptions" Core..=)
              Prelude.<$> nodeToNodeEncryptionOptions,
            ("AccessPolicies" Core..=)
              Prelude.<$> accessPolicies,
            ("ServiceSoftwareOptions" Core..=)
              Prelude.<$> serviceSoftwareOptions,
            ("LogPublishingOptions" Core..=)
              Prelude.<$> logPublishingOptions,
            ("ElasticsearchClusterConfig" Core..=)
              Prelude.<$> elasticsearchClusterConfig,
            ("DomainName" Core..=) Prelude.<$> domainName,
            ("EncryptionAtRestOptions" Core..=)
              Prelude.<$> encryptionAtRestOptions,
            ("VPCOptions" Core..=) Prelude.<$> vPCOptions,
            ("DomainId" Core..=) Prelude.<$> domainId,
            ("Endpoints" Core..=) Prelude.<$> endpoints,
            ("DomainEndpointOptions" Core..=)
              Prelude.<$> domainEndpointOptions,
            ("Endpoint" Core..=) Prelude.<$> endpoint,
            ("ElasticsearchVersion" Core..=)
              Prelude.<$> elasticsearchVersion
          ]
      )
