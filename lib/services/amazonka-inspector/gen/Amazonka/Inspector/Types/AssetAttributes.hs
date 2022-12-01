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
-- Module      : Amazonka.Inspector.Types.AssetAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.AssetAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector.Types.NetworkInterface
import Amazonka.Inspector.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | A collection of attributes of the host from which the finding is
-- generated.
--
-- /See:/ 'newAssetAttributes' smart constructor.
data AssetAttributes = AssetAttributes'
  { -- | The tags related to the EC2 instance where the finding is generated.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the Amazon Machine Image (AMI) that is installed on the EC2
    -- instance where the finding is generated.
    amiId :: Prelude.Maybe Prelude.Text,
    -- | The list of IP v4 addresses of the EC2 instance where the finding is
    -- generated.
    ipv4Addresses :: Prelude.Maybe [Prelude.Text],
    -- | The Auto Scaling group of the EC2 instance where the finding is
    -- generated.
    autoScalingGroup :: Prelude.Maybe Prelude.Text,
    -- | The hostname of the EC2 instance where the finding is generated.
    hostname :: Prelude.Maybe Prelude.Text,
    -- | The ID of the agent that is installed on the EC2 instance where the
    -- finding is generated.
    agentId :: Prelude.Maybe Prelude.Text,
    -- | An array of the network interfaces interacting with the EC2 instance
    -- where the finding is generated.
    networkInterfaces :: Prelude.Maybe [NetworkInterface],
    -- | The schema version of this data type.
    schemaVersion :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'assetAttributes_tags' - The tags related to the EC2 instance where the finding is generated.
--
-- 'amiId', 'assetAttributes_amiId' - The ID of the Amazon Machine Image (AMI) that is installed on the EC2
-- instance where the finding is generated.
--
-- 'ipv4Addresses', 'assetAttributes_ipv4Addresses' - The list of IP v4 addresses of the EC2 instance where the finding is
-- generated.
--
-- 'autoScalingGroup', 'assetAttributes_autoScalingGroup' - The Auto Scaling group of the EC2 instance where the finding is
-- generated.
--
-- 'hostname', 'assetAttributes_hostname' - The hostname of the EC2 instance where the finding is generated.
--
-- 'agentId', 'assetAttributes_agentId' - The ID of the agent that is installed on the EC2 instance where the
-- finding is generated.
--
-- 'networkInterfaces', 'assetAttributes_networkInterfaces' - An array of the network interfaces interacting with the EC2 instance
-- where the finding is generated.
--
-- 'schemaVersion', 'assetAttributes_schemaVersion' - The schema version of this data type.
newAssetAttributes ::
  -- | 'schemaVersion'
  Prelude.Natural ->
  AssetAttributes
newAssetAttributes pSchemaVersion_ =
  AssetAttributes'
    { tags = Prelude.Nothing,
      amiId = Prelude.Nothing,
      ipv4Addresses = Prelude.Nothing,
      autoScalingGroup = Prelude.Nothing,
      hostname = Prelude.Nothing,
      agentId = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing,
      schemaVersion = pSchemaVersion_
    }

-- | The tags related to the EC2 instance where the finding is generated.
assetAttributes_tags :: Lens.Lens' AssetAttributes (Prelude.Maybe [Tag])
assetAttributes_tags = Lens.lens (\AssetAttributes' {tags} -> tags) (\s@AssetAttributes' {} a -> s {tags = a} :: AssetAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Machine Image (AMI) that is installed on the EC2
-- instance where the finding is generated.
assetAttributes_amiId :: Lens.Lens' AssetAttributes (Prelude.Maybe Prelude.Text)
assetAttributes_amiId = Lens.lens (\AssetAttributes' {amiId} -> amiId) (\s@AssetAttributes' {} a -> s {amiId = a} :: AssetAttributes)

-- | The list of IP v4 addresses of the EC2 instance where the finding is
-- generated.
assetAttributes_ipv4Addresses :: Lens.Lens' AssetAttributes (Prelude.Maybe [Prelude.Text])
assetAttributes_ipv4Addresses = Lens.lens (\AssetAttributes' {ipv4Addresses} -> ipv4Addresses) (\s@AssetAttributes' {} a -> s {ipv4Addresses = a} :: AssetAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The Auto Scaling group of the EC2 instance where the finding is
-- generated.
assetAttributes_autoScalingGroup :: Lens.Lens' AssetAttributes (Prelude.Maybe Prelude.Text)
assetAttributes_autoScalingGroup = Lens.lens (\AssetAttributes' {autoScalingGroup} -> autoScalingGroup) (\s@AssetAttributes' {} a -> s {autoScalingGroup = a} :: AssetAttributes)

-- | The hostname of the EC2 instance where the finding is generated.
assetAttributes_hostname :: Lens.Lens' AssetAttributes (Prelude.Maybe Prelude.Text)
assetAttributes_hostname = Lens.lens (\AssetAttributes' {hostname} -> hostname) (\s@AssetAttributes' {} a -> s {hostname = a} :: AssetAttributes)

-- | The ID of the agent that is installed on the EC2 instance where the
-- finding is generated.
assetAttributes_agentId :: Lens.Lens' AssetAttributes (Prelude.Maybe Prelude.Text)
assetAttributes_agentId = Lens.lens (\AssetAttributes' {agentId} -> agentId) (\s@AssetAttributes' {} a -> s {agentId = a} :: AssetAttributes)

-- | An array of the network interfaces interacting with the EC2 instance
-- where the finding is generated.
assetAttributes_networkInterfaces :: Lens.Lens' AssetAttributes (Prelude.Maybe [NetworkInterface])
assetAttributes_networkInterfaces = Lens.lens (\AssetAttributes' {networkInterfaces} -> networkInterfaces) (\s@AssetAttributes' {} a -> s {networkInterfaces = a} :: AssetAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The schema version of this data type.
assetAttributes_schemaVersion :: Lens.Lens' AssetAttributes Prelude.Natural
assetAttributes_schemaVersion = Lens.lens (\AssetAttributes' {schemaVersion} -> schemaVersion) (\s@AssetAttributes' {} a -> s {schemaVersion = a} :: AssetAttributes)

instance Core.FromJSON AssetAttributes where
  parseJSON =
    Core.withObject
      "AssetAttributes"
      ( \x ->
          AssetAttributes'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "amiId")
            Prelude.<*> (x Core..:? "ipv4Addresses" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "autoScalingGroup")
            Prelude.<*> (x Core..:? "hostname")
            Prelude.<*> (x Core..:? "agentId")
            Prelude.<*> ( x Core..:? "networkInterfaces"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "schemaVersion")
      )

instance Prelude.Hashable AssetAttributes where
  hashWithSalt _salt AssetAttributes' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` amiId
      `Prelude.hashWithSalt` ipv4Addresses
      `Prelude.hashWithSalt` autoScalingGroup
      `Prelude.hashWithSalt` hostname
      `Prelude.hashWithSalt` agentId
      `Prelude.hashWithSalt` networkInterfaces
      `Prelude.hashWithSalt` schemaVersion

instance Prelude.NFData AssetAttributes where
  rnf AssetAttributes' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf amiId
      `Prelude.seq` Prelude.rnf ipv4Addresses
      `Prelude.seq` Prelude.rnf autoScalingGroup
      `Prelude.seq` Prelude.rnf hostname
      `Prelude.seq` Prelude.rnf agentId
      `Prelude.seq` Prelude.rnf networkInterfaces
      `Prelude.seq` Prelude.rnf schemaVersion
