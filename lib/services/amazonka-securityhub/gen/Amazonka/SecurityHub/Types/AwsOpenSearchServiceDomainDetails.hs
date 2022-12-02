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
-- Module      : Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainClusterConfigDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainDomainEndpointOptionsDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainLogPublishingOptionsDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainVpcOptionsDetails

-- | Information about an Amazon OpenSearch Service domain.
--
-- /See:/ 'newAwsOpenSearchServiceDomainDetails' smart constructor.
data AwsOpenSearchServiceDomainDetails = AwsOpenSearchServiceDomainDetails'
  { -- | Details about the configuration for node-to-node encryption.
    nodeToNodeEncryptionOptions :: Prelude.Maybe AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails,
    -- | Details about the configuration of an OpenSearch cluster.
    clusterConfig :: Prelude.Maybe AwsOpenSearchServiceDomainClusterConfigDetails,
    -- | Specifies options for fine-grained access control.
    advancedSecurityOptions :: Prelude.Maybe AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails,
    -- | The name of the endpoint.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the OpenSearch Service domain.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Details about the configuration for encryption at rest.
    encryptionAtRestOptions :: Prelude.Maybe AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails,
    -- | The identifier of the domain.
    id :: Prelude.Maybe Prelude.Text,
    -- | IAM policy document that specifies the access policies for the
    -- OpenSearch Service domain.
    accessPolicies :: Prelude.Maybe Prelude.Text,
    -- | Information that OpenSearch Service derives based on @VPCOptions@ for
    -- the domain.
    vpcOptions :: Prelude.Maybe AwsOpenSearchServiceDomainVpcOptionsDetails,
    -- | Additional options for the domain endpoint.
    domainEndpointOptions :: Prelude.Maybe AwsOpenSearchServiceDomainDomainEndpointOptionsDetails,
    -- | The domain endpoints. Used if the OpenSearch domain resides in a VPC.
    --
    -- This is a map of key-value pairs. The key is always @vpc@. The value is
    -- the endpoint.
    domainEndpoints :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The domain endpoint.
    domainEndpoint :: Prelude.Maybe Prelude.Text,
    -- | Information about the status of a domain relative to the latest service
    -- software.
    serviceSoftwareOptions :: Prelude.Maybe AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails,
    -- | Configures the CloudWatch Logs to publish for the OpenSearch domain.
    logPublishingOptions :: Prelude.Maybe AwsOpenSearchServiceDomainLogPublishingOptionsDetails,
    -- | The version of the domain engine.
    engineVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsOpenSearchServiceDomainDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeToNodeEncryptionOptions', 'awsOpenSearchServiceDomainDetails_nodeToNodeEncryptionOptions' - Details about the configuration for node-to-node encryption.
--
-- 'clusterConfig', 'awsOpenSearchServiceDomainDetails_clusterConfig' - Details about the configuration of an OpenSearch cluster.
--
-- 'advancedSecurityOptions', 'awsOpenSearchServiceDomainDetails_advancedSecurityOptions' - Specifies options for fine-grained access control.
--
-- 'domainName', 'awsOpenSearchServiceDomainDetails_domainName' - The name of the endpoint.
--
-- 'arn', 'awsOpenSearchServiceDomainDetails_arn' - The ARN of the OpenSearch Service domain.
--
-- 'encryptionAtRestOptions', 'awsOpenSearchServiceDomainDetails_encryptionAtRestOptions' - Details about the configuration for encryption at rest.
--
-- 'id', 'awsOpenSearchServiceDomainDetails_id' - The identifier of the domain.
--
-- 'accessPolicies', 'awsOpenSearchServiceDomainDetails_accessPolicies' - IAM policy document that specifies the access policies for the
-- OpenSearch Service domain.
--
-- 'vpcOptions', 'awsOpenSearchServiceDomainDetails_vpcOptions' - Information that OpenSearch Service derives based on @VPCOptions@ for
-- the domain.
--
-- 'domainEndpointOptions', 'awsOpenSearchServiceDomainDetails_domainEndpointOptions' - Additional options for the domain endpoint.
--
-- 'domainEndpoints', 'awsOpenSearchServiceDomainDetails_domainEndpoints' - The domain endpoints. Used if the OpenSearch domain resides in a VPC.
--
-- This is a map of key-value pairs. The key is always @vpc@. The value is
-- the endpoint.
--
-- 'domainEndpoint', 'awsOpenSearchServiceDomainDetails_domainEndpoint' - The domain endpoint.
--
-- 'serviceSoftwareOptions', 'awsOpenSearchServiceDomainDetails_serviceSoftwareOptions' - Information about the status of a domain relative to the latest service
-- software.
--
-- 'logPublishingOptions', 'awsOpenSearchServiceDomainDetails_logPublishingOptions' - Configures the CloudWatch Logs to publish for the OpenSearch domain.
--
-- 'engineVersion', 'awsOpenSearchServiceDomainDetails_engineVersion' - The version of the domain engine.
newAwsOpenSearchServiceDomainDetails ::
  AwsOpenSearchServiceDomainDetails
newAwsOpenSearchServiceDomainDetails =
  AwsOpenSearchServiceDomainDetails'
    { nodeToNodeEncryptionOptions =
        Prelude.Nothing,
      clusterConfig = Prelude.Nothing,
      advancedSecurityOptions =
        Prelude.Nothing,
      domainName = Prelude.Nothing,
      arn = Prelude.Nothing,
      encryptionAtRestOptions =
        Prelude.Nothing,
      id = Prelude.Nothing,
      accessPolicies = Prelude.Nothing,
      vpcOptions = Prelude.Nothing,
      domainEndpointOptions = Prelude.Nothing,
      domainEndpoints = Prelude.Nothing,
      domainEndpoint = Prelude.Nothing,
      serviceSoftwareOptions = Prelude.Nothing,
      logPublishingOptions = Prelude.Nothing,
      engineVersion = Prelude.Nothing
    }

-- | Details about the configuration for node-to-node encryption.
awsOpenSearchServiceDomainDetails_nodeToNodeEncryptionOptions :: Lens.Lens' AwsOpenSearchServiceDomainDetails (Prelude.Maybe AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails)
awsOpenSearchServiceDomainDetails_nodeToNodeEncryptionOptions = Lens.lens (\AwsOpenSearchServiceDomainDetails' {nodeToNodeEncryptionOptions} -> nodeToNodeEncryptionOptions) (\s@AwsOpenSearchServiceDomainDetails' {} a -> s {nodeToNodeEncryptionOptions = a} :: AwsOpenSearchServiceDomainDetails)

-- | Details about the configuration of an OpenSearch cluster.
awsOpenSearchServiceDomainDetails_clusterConfig :: Lens.Lens' AwsOpenSearchServiceDomainDetails (Prelude.Maybe AwsOpenSearchServiceDomainClusterConfigDetails)
awsOpenSearchServiceDomainDetails_clusterConfig = Lens.lens (\AwsOpenSearchServiceDomainDetails' {clusterConfig} -> clusterConfig) (\s@AwsOpenSearchServiceDomainDetails' {} a -> s {clusterConfig = a} :: AwsOpenSearchServiceDomainDetails)

-- | Specifies options for fine-grained access control.
awsOpenSearchServiceDomainDetails_advancedSecurityOptions :: Lens.Lens' AwsOpenSearchServiceDomainDetails (Prelude.Maybe AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails)
awsOpenSearchServiceDomainDetails_advancedSecurityOptions = Lens.lens (\AwsOpenSearchServiceDomainDetails' {advancedSecurityOptions} -> advancedSecurityOptions) (\s@AwsOpenSearchServiceDomainDetails' {} a -> s {advancedSecurityOptions = a} :: AwsOpenSearchServiceDomainDetails)

-- | The name of the endpoint.
awsOpenSearchServiceDomainDetails_domainName :: Lens.Lens' AwsOpenSearchServiceDomainDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainDetails_domainName = Lens.lens (\AwsOpenSearchServiceDomainDetails' {domainName} -> domainName) (\s@AwsOpenSearchServiceDomainDetails' {} a -> s {domainName = a} :: AwsOpenSearchServiceDomainDetails)

-- | The ARN of the OpenSearch Service domain.
awsOpenSearchServiceDomainDetails_arn :: Lens.Lens' AwsOpenSearchServiceDomainDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainDetails_arn = Lens.lens (\AwsOpenSearchServiceDomainDetails' {arn} -> arn) (\s@AwsOpenSearchServiceDomainDetails' {} a -> s {arn = a} :: AwsOpenSearchServiceDomainDetails)

-- | Details about the configuration for encryption at rest.
awsOpenSearchServiceDomainDetails_encryptionAtRestOptions :: Lens.Lens' AwsOpenSearchServiceDomainDetails (Prelude.Maybe AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails)
awsOpenSearchServiceDomainDetails_encryptionAtRestOptions = Lens.lens (\AwsOpenSearchServiceDomainDetails' {encryptionAtRestOptions} -> encryptionAtRestOptions) (\s@AwsOpenSearchServiceDomainDetails' {} a -> s {encryptionAtRestOptions = a} :: AwsOpenSearchServiceDomainDetails)

-- | The identifier of the domain.
awsOpenSearchServiceDomainDetails_id :: Lens.Lens' AwsOpenSearchServiceDomainDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainDetails_id = Lens.lens (\AwsOpenSearchServiceDomainDetails' {id} -> id) (\s@AwsOpenSearchServiceDomainDetails' {} a -> s {id = a} :: AwsOpenSearchServiceDomainDetails)

-- | IAM policy document that specifies the access policies for the
-- OpenSearch Service domain.
awsOpenSearchServiceDomainDetails_accessPolicies :: Lens.Lens' AwsOpenSearchServiceDomainDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainDetails_accessPolicies = Lens.lens (\AwsOpenSearchServiceDomainDetails' {accessPolicies} -> accessPolicies) (\s@AwsOpenSearchServiceDomainDetails' {} a -> s {accessPolicies = a} :: AwsOpenSearchServiceDomainDetails)

-- | Information that OpenSearch Service derives based on @VPCOptions@ for
-- the domain.
awsOpenSearchServiceDomainDetails_vpcOptions :: Lens.Lens' AwsOpenSearchServiceDomainDetails (Prelude.Maybe AwsOpenSearchServiceDomainVpcOptionsDetails)
awsOpenSearchServiceDomainDetails_vpcOptions = Lens.lens (\AwsOpenSearchServiceDomainDetails' {vpcOptions} -> vpcOptions) (\s@AwsOpenSearchServiceDomainDetails' {} a -> s {vpcOptions = a} :: AwsOpenSearchServiceDomainDetails)

-- | Additional options for the domain endpoint.
awsOpenSearchServiceDomainDetails_domainEndpointOptions :: Lens.Lens' AwsOpenSearchServiceDomainDetails (Prelude.Maybe AwsOpenSearchServiceDomainDomainEndpointOptionsDetails)
awsOpenSearchServiceDomainDetails_domainEndpointOptions = Lens.lens (\AwsOpenSearchServiceDomainDetails' {domainEndpointOptions} -> domainEndpointOptions) (\s@AwsOpenSearchServiceDomainDetails' {} a -> s {domainEndpointOptions = a} :: AwsOpenSearchServiceDomainDetails)

-- | The domain endpoints. Used if the OpenSearch domain resides in a VPC.
--
-- This is a map of key-value pairs. The key is always @vpc@. The value is
-- the endpoint.
awsOpenSearchServiceDomainDetails_domainEndpoints :: Lens.Lens' AwsOpenSearchServiceDomainDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
awsOpenSearchServiceDomainDetails_domainEndpoints = Lens.lens (\AwsOpenSearchServiceDomainDetails' {domainEndpoints} -> domainEndpoints) (\s@AwsOpenSearchServiceDomainDetails' {} a -> s {domainEndpoints = a} :: AwsOpenSearchServiceDomainDetails) Prelude.. Lens.mapping Lens.coerced

-- | The domain endpoint.
awsOpenSearchServiceDomainDetails_domainEndpoint :: Lens.Lens' AwsOpenSearchServiceDomainDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainDetails_domainEndpoint = Lens.lens (\AwsOpenSearchServiceDomainDetails' {domainEndpoint} -> domainEndpoint) (\s@AwsOpenSearchServiceDomainDetails' {} a -> s {domainEndpoint = a} :: AwsOpenSearchServiceDomainDetails)

-- | Information about the status of a domain relative to the latest service
-- software.
awsOpenSearchServiceDomainDetails_serviceSoftwareOptions :: Lens.Lens' AwsOpenSearchServiceDomainDetails (Prelude.Maybe AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails)
awsOpenSearchServiceDomainDetails_serviceSoftwareOptions = Lens.lens (\AwsOpenSearchServiceDomainDetails' {serviceSoftwareOptions} -> serviceSoftwareOptions) (\s@AwsOpenSearchServiceDomainDetails' {} a -> s {serviceSoftwareOptions = a} :: AwsOpenSearchServiceDomainDetails)

-- | Configures the CloudWatch Logs to publish for the OpenSearch domain.
awsOpenSearchServiceDomainDetails_logPublishingOptions :: Lens.Lens' AwsOpenSearchServiceDomainDetails (Prelude.Maybe AwsOpenSearchServiceDomainLogPublishingOptionsDetails)
awsOpenSearchServiceDomainDetails_logPublishingOptions = Lens.lens (\AwsOpenSearchServiceDomainDetails' {logPublishingOptions} -> logPublishingOptions) (\s@AwsOpenSearchServiceDomainDetails' {} a -> s {logPublishingOptions = a} :: AwsOpenSearchServiceDomainDetails)

-- | The version of the domain engine.
awsOpenSearchServiceDomainDetails_engineVersion :: Lens.Lens' AwsOpenSearchServiceDomainDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainDetails_engineVersion = Lens.lens (\AwsOpenSearchServiceDomainDetails' {engineVersion} -> engineVersion) (\s@AwsOpenSearchServiceDomainDetails' {} a -> s {engineVersion = a} :: AwsOpenSearchServiceDomainDetails)

instance
  Data.FromJSON
    AwsOpenSearchServiceDomainDetails
  where
  parseJSON =
    Data.withObject
      "AwsOpenSearchServiceDomainDetails"
      ( \x ->
          AwsOpenSearchServiceDomainDetails'
            Prelude.<$> (x Data..:? "NodeToNodeEncryptionOptions")
            Prelude.<*> (x Data..:? "ClusterConfig")
            Prelude.<*> (x Data..:? "AdvancedSecurityOptions")
            Prelude.<*> (x Data..:? "DomainName")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "EncryptionAtRestOptions")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "AccessPolicies")
            Prelude.<*> (x Data..:? "VpcOptions")
            Prelude.<*> (x Data..:? "DomainEndpointOptions")
            Prelude.<*> ( x Data..:? "DomainEndpoints"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DomainEndpoint")
            Prelude.<*> (x Data..:? "ServiceSoftwareOptions")
            Prelude.<*> (x Data..:? "LogPublishingOptions")
            Prelude.<*> (x Data..:? "EngineVersion")
      )

instance
  Prelude.Hashable
    AwsOpenSearchServiceDomainDetails
  where
  hashWithSalt
    _salt
    AwsOpenSearchServiceDomainDetails' {..} =
      _salt
        `Prelude.hashWithSalt` nodeToNodeEncryptionOptions
        `Prelude.hashWithSalt` clusterConfig
        `Prelude.hashWithSalt` advancedSecurityOptions
        `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` encryptionAtRestOptions
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` accessPolicies
        `Prelude.hashWithSalt` vpcOptions
        `Prelude.hashWithSalt` domainEndpointOptions
        `Prelude.hashWithSalt` domainEndpoints
        `Prelude.hashWithSalt` domainEndpoint
        `Prelude.hashWithSalt` serviceSoftwareOptions
        `Prelude.hashWithSalt` logPublishingOptions
        `Prelude.hashWithSalt` engineVersion

instance
  Prelude.NFData
    AwsOpenSearchServiceDomainDetails
  where
  rnf AwsOpenSearchServiceDomainDetails' {..} =
    Prelude.rnf nodeToNodeEncryptionOptions
      `Prelude.seq` Prelude.rnf clusterConfig
      `Prelude.seq` Prelude.rnf advancedSecurityOptions
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf encryptionAtRestOptions
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf accessPolicies
      `Prelude.seq` Prelude.rnf vpcOptions
      `Prelude.seq` Prelude.rnf domainEndpointOptions
      `Prelude.seq` Prelude.rnf domainEndpoints
      `Prelude.seq` Prelude.rnf domainEndpoint
      `Prelude.seq` Prelude.rnf serviceSoftwareOptions
      `Prelude.seq` Prelude.rnf logPublishingOptions
      `Prelude.seq` Prelude.rnf engineVersion

instance
  Data.ToJSON
    AwsOpenSearchServiceDomainDetails
  where
  toJSON AwsOpenSearchServiceDomainDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NodeToNodeEncryptionOptions" Data..=)
              Prelude.<$> nodeToNodeEncryptionOptions,
            ("ClusterConfig" Data..=) Prelude.<$> clusterConfig,
            ("AdvancedSecurityOptions" Data..=)
              Prelude.<$> advancedSecurityOptions,
            ("DomainName" Data..=) Prelude.<$> domainName,
            ("Arn" Data..=) Prelude.<$> arn,
            ("EncryptionAtRestOptions" Data..=)
              Prelude.<$> encryptionAtRestOptions,
            ("Id" Data..=) Prelude.<$> id,
            ("AccessPolicies" Data..=)
              Prelude.<$> accessPolicies,
            ("VpcOptions" Data..=) Prelude.<$> vpcOptions,
            ("DomainEndpointOptions" Data..=)
              Prelude.<$> domainEndpointOptions,
            ("DomainEndpoints" Data..=)
              Prelude.<$> domainEndpoints,
            ("DomainEndpoint" Data..=)
              Prelude.<$> domainEndpoint,
            ("ServiceSoftwareOptions" Data..=)
              Prelude.<$> serviceSoftwareOptions,
            ("LogPublishingOptions" Data..=)
              Prelude.<$> logPublishingOptions,
            ("EngineVersion" Data..=) Prelude.<$> engineVersion
          ]
      )
