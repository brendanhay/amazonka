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
-- Module      : Amazonka.NetworkManager.Types.NetworkResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.NetworkResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a network resource.
--
-- /See:/ 'newNetworkResource' smart constructor.
data NetworkResource = NetworkResource'
  { -- | The Amazon Web Services account ID.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | Information about the resource, in JSON format. Network Manager gets
    -- this information by describing the resource using its Describe API call.
    definition :: Prelude.Maybe Prelude.Text,
    -- | The time that the resource definition was retrieved.
    definitionTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The resource metadata.
    metadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of the gateway.
    registeredGatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the resource.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The resource type.
    --
    -- The following are the supported resource types for Direct Connect:
    --
    -- -   @dxcon@
    --
    -- -   @dx-gateway@
    --
    -- -   @dx-vif@
    --
    -- The following are the supported resource types for Network Manager:
    --
    -- -   @connection@
    --
    -- -   @device@
    --
    -- -   @link@
    --
    -- -   @site@
    --
    -- The following are the supported resource types for Amazon VPC:
    --
    -- -   @customer-gateway@
    --
    -- -   @transit-gateway@
    --
    -- -   @transit-gateway-attachment@
    --
    -- -   @transit-gateway-connect-peer@
    --
    -- -   @transit-gateway-route-table@
    --
    -- -   @vpn-connection@
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The tags.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'networkResource_accountId' - The Amazon Web Services account ID.
--
-- 'awsRegion', 'networkResource_awsRegion' - The Amazon Web Services Region.
--
-- 'coreNetworkId', 'networkResource_coreNetworkId' - The ID of a core network.
--
-- 'definition', 'networkResource_definition' - Information about the resource, in JSON format. Network Manager gets
-- this information by describing the resource using its Describe API call.
--
-- 'definitionTimestamp', 'networkResource_definitionTimestamp' - The time that the resource definition was retrieved.
--
-- 'metadata', 'networkResource_metadata' - The resource metadata.
--
-- 'registeredGatewayArn', 'networkResource_registeredGatewayArn' - The ARN of the gateway.
--
-- 'resourceArn', 'networkResource_resourceArn' - The ARN of the resource.
--
-- 'resourceId', 'networkResource_resourceId' - The ID of the resource.
--
-- 'resourceType', 'networkResource_resourceType' - The resource type.
--
-- The following are the supported resource types for Direct Connect:
--
-- -   @dxcon@
--
-- -   @dx-gateway@
--
-- -   @dx-vif@
--
-- The following are the supported resource types for Network Manager:
--
-- -   @connection@
--
-- -   @device@
--
-- -   @link@
--
-- -   @site@
--
-- The following are the supported resource types for Amazon VPC:
--
-- -   @customer-gateway@
--
-- -   @transit-gateway@
--
-- -   @transit-gateway-attachment@
--
-- -   @transit-gateway-connect-peer@
--
-- -   @transit-gateway-route-table@
--
-- -   @vpn-connection@
--
-- 'tags', 'networkResource_tags' - The tags.
newNetworkResource ::
  NetworkResource
newNetworkResource =
  NetworkResource'
    { accountId = Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      coreNetworkId = Prelude.Nothing,
      definition = Prelude.Nothing,
      definitionTimestamp = Prelude.Nothing,
      metadata = Prelude.Nothing,
      registeredGatewayArn = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Web Services account ID.
networkResource_accountId :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_accountId = Lens.lens (\NetworkResource' {accountId} -> accountId) (\s@NetworkResource' {} a -> s {accountId = a} :: NetworkResource)

-- | The Amazon Web Services Region.
networkResource_awsRegion :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_awsRegion = Lens.lens (\NetworkResource' {awsRegion} -> awsRegion) (\s@NetworkResource' {} a -> s {awsRegion = a} :: NetworkResource)

-- | The ID of a core network.
networkResource_coreNetworkId :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_coreNetworkId = Lens.lens (\NetworkResource' {coreNetworkId} -> coreNetworkId) (\s@NetworkResource' {} a -> s {coreNetworkId = a} :: NetworkResource)

-- | Information about the resource, in JSON format. Network Manager gets
-- this information by describing the resource using its Describe API call.
networkResource_definition :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_definition = Lens.lens (\NetworkResource' {definition} -> definition) (\s@NetworkResource' {} a -> s {definition = a} :: NetworkResource)

-- | The time that the resource definition was retrieved.
networkResource_definitionTimestamp :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.UTCTime)
networkResource_definitionTimestamp = Lens.lens (\NetworkResource' {definitionTimestamp} -> definitionTimestamp) (\s@NetworkResource' {} a -> s {definitionTimestamp = a} :: NetworkResource) Prelude.. Lens.mapping Data._Time

-- | The resource metadata.
networkResource_metadata :: Lens.Lens' NetworkResource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
networkResource_metadata = Lens.lens (\NetworkResource' {metadata} -> metadata) (\s@NetworkResource' {} a -> s {metadata = a} :: NetworkResource) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the gateway.
networkResource_registeredGatewayArn :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_registeredGatewayArn = Lens.lens (\NetworkResource' {registeredGatewayArn} -> registeredGatewayArn) (\s@NetworkResource' {} a -> s {registeredGatewayArn = a} :: NetworkResource)

-- | The ARN of the resource.
networkResource_resourceArn :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_resourceArn = Lens.lens (\NetworkResource' {resourceArn} -> resourceArn) (\s@NetworkResource' {} a -> s {resourceArn = a} :: NetworkResource)

-- | The ID of the resource.
networkResource_resourceId :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_resourceId = Lens.lens (\NetworkResource' {resourceId} -> resourceId) (\s@NetworkResource' {} a -> s {resourceId = a} :: NetworkResource)

-- | The resource type.
--
-- The following are the supported resource types for Direct Connect:
--
-- -   @dxcon@
--
-- -   @dx-gateway@
--
-- -   @dx-vif@
--
-- The following are the supported resource types for Network Manager:
--
-- -   @connection@
--
-- -   @device@
--
-- -   @link@
--
-- -   @site@
--
-- The following are the supported resource types for Amazon VPC:
--
-- -   @customer-gateway@
--
-- -   @transit-gateway@
--
-- -   @transit-gateway-attachment@
--
-- -   @transit-gateway-connect-peer@
--
-- -   @transit-gateway-route-table@
--
-- -   @vpn-connection@
networkResource_resourceType :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_resourceType = Lens.lens (\NetworkResource' {resourceType} -> resourceType) (\s@NetworkResource' {} a -> s {resourceType = a} :: NetworkResource)

-- | The tags.
networkResource_tags :: Lens.Lens' NetworkResource (Prelude.Maybe [Tag])
networkResource_tags = Lens.lens (\NetworkResource' {tags} -> tags) (\s@NetworkResource' {} a -> s {tags = a} :: NetworkResource) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON NetworkResource where
  parseJSON =
    Data.withObject
      "NetworkResource"
      ( \x ->
          NetworkResource'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "AwsRegion")
            Prelude.<*> (x Data..:? "CoreNetworkId")
            Prelude.<*> (x Data..:? "Definition")
            Prelude.<*> (x Data..:? "DefinitionTimestamp")
            Prelude.<*> (x Data..:? "Metadata" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RegisteredGatewayArn")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "ResourceId")
            Prelude.<*> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable NetworkResource where
  hashWithSalt _salt NetworkResource' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` definition
      `Prelude.hashWithSalt` definitionTimestamp
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` registeredGatewayArn
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` tags

instance Prelude.NFData NetworkResource where
  rnf NetworkResource' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf definitionTimestamp
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf registeredGatewayArn
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf tags
