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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.NetworkResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a network resource.
--
-- /See:/ 'newNetworkResource' smart constructor.
data NetworkResource = NetworkResource'
  { -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The tags.
    tags :: Prelude.Maybe [Tag],
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
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The resource metadata.
    metadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Web Services account ID.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the gateway.
    registeredGatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The time that the resource definition was retrieved.
    definitionTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the resource.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | Information about the resource, in JSON format. Network Manager gets
    -- this information by describing the resource using its Describe API call.
    definition :: Prelude.Maybe Prelude.Text
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
-- 'resourceId', 'networkResource_resourceId' - The ID of the resource.
--
-- 'tags', 'networkResource_tags' - The tags.
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
-- 'coreNetworkId', 'networkResource_coreNetworkId' - The ID of a core network.
--
-- 'metadata', 'networkResource_metadata' - The resource metadata.
--
-- 'accountId', 'networkResource_accountId' - The Amazon Web Services account ID.
--
-- 'registeredGatewayArn', 'networkResource_registeredGatewayArn' - The ARN of the gateway.
--
-- 'awsRegion', 'networkResource_awsRegion' - The Amazon Web Services Region.
--
-- 'definitionTimestamp', 'networkResource_definitionTimestamp' - The time that the resource definition was retrieved.
--
-- 'resourceArn', 'networkResource_resourceArn' - The ARN of the resource.
--
-- 'definition', 'networkResource_definition' - Information about the resource, in JSON format. Network Manager gets
-- this information by describing the resource using its Describe API call.
newNetworkResource ::
  NetworkResource
newNetworkResource =
  NetworkResource'
    { resourceId = Prelude.Nothing,
      tags = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      coreNetworkId = Prelude.Nothing,
      metadata = Prelude.Nothing,
      accountId = Prelude.Nothing,
      registeredGatewayArn = Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      definitionTimestamp = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      definition = Prelude.Nothing
    }

-- | The ID of the resource.
networkResource_resourceId :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_resourceId = Lens.lens (\NetworkResource' {resourceId} -> resourceId) (\s@NetworkResource' {} a -> s {resourceId = a} :: NetworkResource)

-- | The tags.
networkResource_tags :: Lens.Lens' NetworkResource (Prelude.Maybe [Tag])
networkResource_tags = Lens.lens (\NetworkResource' {tags} -> tags) (\s@NetworkResource' {} a -> s {tags = a} :: NetworkResource) Prelude.. Lens.mapping Lens.coerced

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

-- | The ID of a core network.
networkResource_coreNetworkId :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_coreNetworkId = Lens.lens (\NetworkResource' {coreNetworkId} -> coreNetworkId) (\s@NetworkResource' {} a -> s {coreNetworkId = a} :: NetworkResource)

-- | The resource metadata.
networkResource_metadata :: Lens.Lens' NetworkResource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
networkResource_metadata = Lens.lens (\NetworkResource' {metadata} -> metadata) (\s@NetworkResource' {} a -> s {metadata = a} :: NetworkResource) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account ID.
networkResource_accountId :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_accountId = Lens.lens (\NetworkResource' {accountId} -> accountId) (\s@NetworkResource' {} a -> s {accountId = a} :: NetworkResource)

-- | The ARN of the gateway.
networkResource_registeredGatewayArn :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_registeredGatewayArn = Lens.lens (\NetworkResource' {registeredGatewayArn} -> registeredGatewayArn) (\s@NetworkResource' {} a -> s {registeredGatewayArn = a} :: NetworkResource)

-- | The Amazon Web Services Region.
networkResource_awsRegion :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_awsRegion = Lens.lens (\NetworkResource' {awsRegion} -> awsRegion) (\s@NetworkResource' {} a -> s {awsRegion = a} :: NetworkResource)

-- | The time that the resource definition was retrieved.
networkResource_definitionTimestamp :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.UTCTime)
networkResource_definitionTimestamp = Lens.lens (\NetworkResource' {definitionTimestamp} -> definitionTimestamp) (\s@NetworkResource' {} a -> s {definitionTimestamp = a} :: NetworkResource) Prelude.. Lens.mapping Core._Time

-- | The ARN of the resource.
networkResource_resourceArn :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_resourceArn = Lens.lens (\NetworkResource' {resourceArn} -> resourceArn) (\s@NetworkResource' {} a -> s {resourceArn = a} :: NetworkResource)

-- | Information about the resource, in JSON format. Network Manager gets
-- this information by describing the resource using its Describe API call.
networkResource_definition :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_definition = Lens.lens (\NetworkResource' {definition} -> definition) (\s@NetworkResource' {} a -> s {definition = a} :: NetworkResource)

instance Core.FromJSON NetworkResource where
  parseJSON =
    Core.withObject
      "NetworkResource"
      ( \x ->
          NetworkResource'
            Prelude.<$> (x Core..:? "ResourceId")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "CoreNetworkId")
            Prelude.<*> (x Core..:? "Metadata" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "RegisteredGatewayArn")
            Prelude.<*> (x Core..:? "AwsRegion")
            Prelude.<*> (x Core..:? "DefinitionTimestamp")
            Prelude.<*> (x Core..:? "ResourceArn")
            Prelude.<*> (x Core..:? "Definition")
      )

instance Prelude.Hashable NetworkResource where
  hashWithSalt _salt NetworkResource' {..} =
    _salt `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` registeredGatewayArn
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` definitionTimestamp
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` definition

instance Prelude.NFData NetworkResource where
  rnf NetworkResource' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf registeredGatewayArn
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf definitionTimestamp
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf definition
