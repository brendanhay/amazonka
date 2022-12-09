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
-- Module      : Amazonka.PrivateNetworks.Types.NetworkResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PrivateNetworks.Types.NetworkResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types.HealthStatus
import Amazonka.PrivateNetworks.Types.NameValuePair
import Amazonka.PrivateNetworks.Types.NetworkResourceStatus
import Amazonka.PrivateNetworks.Types.NetworkResourceType
import Amazonka.PrivateNetworks.Types.Position

-- | Information about a network resource.
--
-- /See:/ 'newNetworkResource' smart constructor.
data NetworkResource = NetworkResource'
  { -- | The attributes of the network resource.
    attributes :: Prelude.Maybe [NameValuePair],
    -- | The creation time of the network resource.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The description of the network resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | The health of the network resource.
    health :: Prelude.Maybe HealthStatus,
    -- | The model of the network resource.
    model :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the network on which this network
    -- resource appears.
    networkArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the network resource.
    networkResourceArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the network site on which this network
    -- resource appears.
    networkSiteArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the order used to purchase this
    -- network resource.
    orderArn :: Prelude.Maybe Prelude.Text,
    -- | The position of the network resource.
    position :: Prelude.Maybe Position,
    -- | The serial number of the network resource.
    serialNumber :: Prelude.Maybe Prelude.Text,
    -- | The status of the network resource.
    status :: Prelude.Maybe NetworkResourceStatus,
    -- | The status reason of the network resource.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The type of the network resource.
    type' :: Prelude.Maybe NetworkResourceType,
    -- | The vendor of the network resource.
    vendor :: Prelude.Maybe Prelude.Text
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
-- 'attributes', 'networkResource_attributes' - The attributes of the network resource.
--
-- 'createdAt', 'networkResource_createdAt' - The creation time of the network resource.
--
-- 'description', 'networkResource_description' - The description of the network resource.
--
-- 'health', 'networkResource_health' - The health of the network resource.
--
-- 'model', 'networkResource_model' - The model of the network resource.
--
-- 'networkArn', 'networkResource_networkArn' - The Amazon Resource Name (ARN) of the network on which this network
-- resource appears.
--
-- 'networkResourceArn', 'networkResource_networkResourceArn' - The Amazon Resource Name (ARN) of the network resource.
--
-- 'networkSiteArn', 'networkResource_networkSiteArn' - The Amazon Resource Name (ARN) of the network site on which this network
-- resource appears.
--
-- 'orderArn', 'networkResource_orderArn' - The Amazon Resource Name (ARN) of the order used to purchase this
-- network resource.
--
-- 'position', 'networkResource_position' - The position of the network resource.
--
-- 'serialNumber', 'networkResource_serialNumber' - The serial number of the network resource.
--
-- 'status', 'networkResource_status' - The status of the network resource.
--
-- 'statusReason', 'networkResource_statusReason' - The status reason of the network resource.
--
-- 'type'', 'networkResource_type' - The type of the network resource.
--
-- 'vendor', 'networkResource_vendor' - The vendor of the network resource.
newNetworkResource ::
  NetworkResource
newNetworkResource =
  NetworkResource'
    { attributes = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      health = Prelude.Nothing,
      model = Prelude.Nothing,
      networkArn = Prelude.Nothing,
      networkResourceArn = Prelude.Nothing,
      networkSiteArn = Prelude.Nothing,
      orderArn = Prelude.Nothing,
      position = Prelude.Nothing,
      serialNumber = Prelude.Nothing,
      status = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      type' = Prelude.Nothing,
      vendor = Prelude.Nothing
    }

-- | The attributes of the network resource.
networkResource_attributes :: Lens.Lens' NetworkResource (Prelude.Maybe [NameValuePair])
networkResource_attributes = Lens.lens (\NetworkResource' {attributes} -> attributes) (\s@NetworkResource' {} a -> s {attributes = a} :: NetworkResource) Prelude.. Lens.mapping Lens.coerced

-- | The creation time of the network resource.
networkResource_createdAt :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.UTCTime)
networkResource_createdAt = Lens.lens (\NetworkResource' {createdAt} -> createdAt) (\s@NetworkResource' {} a -> s {createdAt = a} :: NetworkResource) Prelude.. Lens.mapping Data._Time

-- | The description of the network resource.
networkResource_description :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_description = Lens.lens (\NetworkResource' {description} -> description) (\s@NetworkResource' {} a -> s {description = a} :: NetworkResource)

-- | The health of the network resource.
networkResource_health :: Lens.Lens' NetworkResource (Prelude.Maybe HealthStatus)
networkResource_health = Lens.lens (\NetworkResource' {health} -> health) (\s@NetworkResource' {} a -> s {health = a} :: NetworkResource)

-- | The model of the network resource.
networkResource_model :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_model = Lens.lens (\NetworkResource' {model} -> model) (\s@NetworkResource' {} a -> s {model = a} :: NetworkResource)

-- | The Amazon Resource Name (ARN) of the network on which this network
-- resource appears.
networkResource_networkArn :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_networkArn = Lens.lens (\NetworkResource' {networkArn} -> networkArn) (\s@NetworkResource' {} a -> s {networkArn = a} :: NetworkResource)

-- | The Amazon Resource Name (ARN) of the network resource.
networkResource_networkResourceArn :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_networkResourceArn = Lens.lens (\NetworkResource' {networkResourceArn} -> networkResourceArn) (\s@NetworkResource' {} a -> s {networkResourceArn = a} :: NetworkResource)

-- | The Amazon Resource Name (ARN) of the network site on which this network
-- resource appears.
networkResource_networkSiteArn :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_networkSiteArn = Lens.lens (\NetworkResource' {networkSiteArn} -> networkSiteArn) (\s@NetworkResource' {} a -> s {networkSiteArn = a} :: NetworkResource)

-- | The Amazon Resource Name (ARN) of the order used to purchase this
-- network resource.
networkResource_orderArn :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_orderArn = Lens.lens (\NetworkResource' {orderArn} -> orderArn) (\s@NetworkResource' {} a -> s {orderArn = a} :: NetworkResource)

-- | The position of the network resource.
networkResource_position :: Lens.Lens' NetworkResource (Prelude.Maybe Position)
networkResource_position = Lens.lens (\NetworkResource' {position} -> position) (\s@NetworkResource' {} a -> s {position = a} :: NetworkResource)

-- | The serial number of the network resource.
networkResource_serialNumber :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_serialNumber = Lens.lens (\NetworkResource' {serialNumber} -> serialNumber) (\s@NetworkResource' {} a -> s {serialNumber = a} :: NetworkResource)

-- | The status of the network resource.
networkResource_status :: Lens.Lens' NetworkResource (Prelude.Maybe NetworkResourceStatus)
networkResource_status = Lens.lens (\NetworkResource' {status} -> status) (\s@NetworkResource' {} a -> s {status = a} :: NetworkResource)

-- | The status reason of the network resource.
networkResource_statusReason :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_statusReason = Lens.lens (\NetworkResource' {statusReason} -> statusReason) (\s@NetworkResource' {} a -> s {statusReason = a} :: NetworkResource)

-- | The type of the network resource.
networkResource_type :: Lens.Lens' NetworkResource (Prelude.Maybe NetworkResourceType)
networkResource_type = Lens.lens (\NetworkResource' {type'} -> type') (\s@NetworkResource' {} a -> s {type' = a} :: NetworkResource)

-- | The vendor of the network resource.
networkResource_vendor :: Lens.Lens' NetworkResource (Prelude.Maybe Prelude.Text)
networkResource_vendor = Lens.lens (\NetworkResource' {vendor} -> vendor) (\s@NetworkResource' {} a -> s {vendor = a} :: NetworkResource)

instance Data.FromJSON NetworkResource where
  parseJSON =
    Data.withObject
      "NetworkResource"
      ( \x ->
          NetworkResource'
            Prelude.<$> (x Data..:? "attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "health")
            Prelude.<*> (x Data..:? "model")
            Prelude.<*> (x Data..:? "networkArn")
            Prelude.<*> (x Data..:? "networkResourceArn")
            Prelude.<*> (x Data..:? "networkSiteArn")
            Prelude.<*> (x Data..:? "orderArn")
            Prelude.<*> (x Data..:? "position")
            Prelude.<*> (x Data..:? "serialNumber")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "statusReason")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "vendor")
      )

instance Prelude.Hashable NetworkResource where
  hashWithSalt _salt NetworkResource' {..} =
    _salt `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` health
      `Prelude.hashWithSalt` model
      `Prelude.hashWithSalt` networkArn
      `Prelude.hashWithSalt` networkResourceArn
      `Prelude.hashWithSalt` networkSiteArn
      `Prelude.hashWithSalt` orderArn
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` serialNumber
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` vendor

instance Prelude.NFData NetworkResource where
  rnf NetworkResource' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf health
      `Prelude.seq` Prelude.rnf model
      `Prelude.seq` Prelude.rnf networkArn
      `Prelude.seq` Prelude.rnf networkResourceArn
      `Prelude.seq` Prelude.rnf networkSiteArn
      `Prelude.seq` Prelude.rnf orderArn
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf serialNumber
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf vendor
