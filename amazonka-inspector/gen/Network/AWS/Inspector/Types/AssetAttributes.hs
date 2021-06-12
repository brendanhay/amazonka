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
-- Module      : Network.AWS.Inspector.Types.AssetAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssetAttributes where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types.NetworkInterface
import Network.AWS.Inspector.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | A collection of attributes of the host from which the finding is
-- generated.
--
-- /See:/ 'newAssetAttributes' smart constructor.
data AssetAttributes = AssetAttributes'
  { -- | The hostname of the EC2 instance where the finding is generated.
    hostname :: Core.Maybe Core.Text,
    -- | The ID of the agent that is installed on the EC2 instance where the
    -- finding is generated.
    agentId :: Core.Maybe Core.Text,
    -- | The ID of the Amazon Machine Image (AMI) that is installed on the EC2
    -- instance where the finding is generated.
    amiId :: Core.Maybe Core.Text,
    -- | The tags related to the EC2 instance where the finding is generated.
    tags :: Core.Maybe [Tag],
    -- | The list of IP v4 addresses of the EC2 instance where the finding is
    -- generated.
    ipv4Addresses :: Core.Maybe [Core.Text],
    -- | An array of the network interfaces interacting with the EC2 instance
    -- where the finding is generated.
    networkInterfaces :: Core.Maybe [NetworkInterface],
    -- | The Auto Scaling group of the EC2 instance where the finding is
    -- generated.
    autoScalingGroup :: Core.Maybe Core.Text,
    -- | The schema version of this data type.
    schemaVersion :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssetAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostname', 'assetAttributes_hostname' - The hostname of the EC2 instance where the finding is generated.
--
-- 'agentId', 'assetAttributes_agentId' - The ID of the agent that is installed on the EC2 instance where the
-- finding is generated.
--
-- 'amiId', 'assetAttributes_amiId' - The ID of the Amazon Machine Image (AMI) that is installed on the EC2
-- instance where the finding is generated.
--
-- 'tags', 'assetAttributes_tags' - The tags related to the EC2 instance where the finding is generated.
--
-- 'ipv4Addresses', 'assetAttributes_ipv4Addresses' - The list of IP v4 addresses of the EC2 instance where the finding is
-- generated.
--
-- 'networkInterfaces', 'assetAttributes_networkInterfaces' - An array of the network interfaces interacting with the EC2 instance
-- where the finding is generated.
--
-- 'autoScalingGroup', 'assetAttributes_autoScalingGroup' - The Auto Scaling group of the EC2 instance where the finding is
-- generated.
--
-- 'schemaVersion', 'assetAttributes_schemaVersion' - The schema version of this data type.
newAssetAttributes ::
  -- | 'schemaVersion'
  Core.Natural ->
  AssetAttributes
newAssetAttributes pSchemaVersion_ =
  AssetAttributes'
    { hostname = Core.Nothing,
      agentId = Core.Nothing,
      amiId = Core.Nothing,
      tags = Core.Nothing,
      ipv4Addresses = Core.Nothing,
      networkInterfaces = Core.Nothing,
      autoScalingGroup = Core.Nothing,
      schemaVersion = pSchemaVersion_
    }

-- | The hostname of the EC2 instance where the finding is generated.
assetAttributes_hostname :: Lens.Lens' AssetAttributes (Core.Maybe Core.Text)
assetAttributes_hostname = Lens.lens (\AssetAttributes' {hostname} -> hostname) (\s@AssetAttributes' {} a -> s {hostname = a} :: AssetAttributes)

-- | The ID of the agent that is installed on the EC2 instance where the
-- finding is generated.
assetAttributes_agentId :: Lens.Lens' AssetAttributes (Core.Maybe Core.Text)
assetAttributes_agentId = Lens.lens (\AssetAttributes' {agentId} -> agentId) (\s@AssetAttributes' {} a -> s {agentId = a} :: AssetAttributes)

-- | The ID of the Amazon Machine Image (AMI) that is installed on the EC2
-- instance where the finding is generated.
assetAttributes_amiId :: Lens.Lens' AssetAttributes (Core.Maybe Core.Text)
assetAttributes_amiId = Lens.lens (\AssetAttributes' {amiId} -> amiId) (\s@AssetAttributes' {} a -> s {amiId = a} :: AssetAttributes)

-- | The tags related to the EC2 instance where the finding is generated.
assetAttributes_tags :: Lens.Lens' AssetAttributes (Core.Maybe [Tag])
assetAttributes_tags = Lens.lens (\AssetAttributes' {tags} -> tags) (\s@AssetAttributes' {} a -> s {tags = a} :: AssetAttributes) Core.. Lens.mapping Lens._Coerce

-- | The list of IP v4 addresses of the EC2 instance where the finding is
-- generated.
assetAttributes_ipv4Addresses :: Lens.Lens' AssetAttributes (Core.Maybe [Core.Text])
assetAttributes_ipv4Addresses = Lens.lens (\AssetAttributes' {ipv4Addresses} -> ipv4Addresses) (\s@AssetAttributes' {} a -> s {ipv4Addresses = a} :: AssetAttributes) Core.. Lens.mapping Lens._Coerce

-- | An array of the network interfaces interacting with the EC2 instance
-- where the finding is generated.
assetAttributes_networkInterfaces :: Lens.Lens' AssetAttributes (Core.Maybe [NetworkInterface])
assetAttributes_networkInterfaces = Lens.lens (\AssetAttributes' {networkInterfaces} -> networkInterfaces) (\s@AssetAttributes' {} a -> s {networkInterfaces = a} :: AssetAttributes) Core.. Lens.mapping Lens._Coerce

-- | The Auto Scaling group of the EC2 instance where the finding is
-- generated.
assetAttributes_autoScalingGroup :: Lens.Lens' AssetAttributes (Core.Maybe Core.Text)
assetAttributes_autoScalingGroup = Lens.lens (\AssetAttributes' {autoScalingGroup} -> autoScalingGroup) (\s@AssetAttributes' {} a -> s {autoScalingGroup = a} :: AssetAttributes)

-- | The schema version of this data type.
assetAttributes_schemaVersion :: Lens.Lens' AssetAttributes Core.Natural
assetAttributes_schemaVersion = Lens.lens (\AssetAttributes' {schemaVersion} -> schemaVersion) (\s@AssetAttributes' {} a -> s {schemaVersion = a} :: AssetAttributes)

instance Core.FromJSON AssetAttributes where
  parseJSON =
    Core.withObject
      "AssetAttributes"
      ( \x ->
          AssetAttributes'
            Core.<$> (x Core..:? "hostname")
            Core.<*> (x Core..:? "agentId")
            Core.<*> (x Core..:? "amiId")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ipv4Addresses" Core..!= Core.mempty)
            Core.<*> (x Core..:? "networkInterfaces" Core..!= Core.mempty)
            Core.<*> (x Core..:? "autoScalingGroup")
            Core.<*> (x Core..: "schemaVersion")
      )

instance Core.Hashable AssetAttributes

instance Core.NFData AssetAttributes
