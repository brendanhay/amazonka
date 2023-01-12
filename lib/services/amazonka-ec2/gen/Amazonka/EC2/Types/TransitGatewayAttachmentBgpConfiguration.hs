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
-- Module      : Amazonka.EC2.Types.TransitGatewayAttachmentBgpConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayAttachmentBgpConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.BgpStatus
import qualified Amazonka.Prelude as Prelude

-- | The BGP configuration information.
--
-- /See:/ 'newTransitGatewayAttachmentBgpConfiguration' smart constructor.
data TransitGatewayAttachmentBgpConfiguration = TransitGatewayAttachmentBgpConfiguration'
  { -- | The BGP status.
    bgpStatus :: Prelude.Maybe BgpStatus,
    -- | The interior BGP peer IP address for the appliance.
    peerAddress :: Prelude.Maybe Prelude.Text,
    -- | The peer Autonomous System Number (ASN).
    peerAsn :: Prelude.Maybe Prelude.Integer,
    -- | The interior BGP peer IP address for the transit gateway.
    transitGatewayAddress :: Prelude.Maybe Prelude.Text,
    -- | The transit gateway Autonomous System Number (ASN).
    transitGatewayAsn :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayAttachmentBgpConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bgpStatus', 'transitGatewayAttachmentBgpConfiguration_bgpStatus' - The BGP status.
--
-- 'peerAddress', 'transitGatewayAttachmentBgpConfiguration_peerAddress' - The interior BGP peer IP address for the appliance.
--
-- 'peerAsn', 'transitGatewayAttachmentBgpConfiguration_peerAsn' - The peer Autonomous System Number (ASN).
--
-- 'transitGatewayAddress', 'transitGatewayAttachmentBgpConfiguration_transitGatewayAddress' - The interior BGP peer IP address for the transit gateway.
--
-- 'transitGatewayAsn', 'transitGatewayAttachmentBgpConfiguration_transitGatewayAsn' - The transit gateway Autonomous System Number (ASN).
newTransitGatewayAttachmentBgpConfiguration ::
  TransitGatewayAttachmentBgpConfiguration
newTransitGatewayAttachmentBgpConfiguration =
  TransitGatewayAttachmentBgpConfiguration'
    { bgpStatus =
        Prelude.Nothing,
      peerAddress = Prelude.Nothing,
      peerAsn = Prelude.Nothing,
      transitGatewayAddress =
        Prelude.Nothing,
      transitGatewayAsn =
        Prelude.Nothing
    }

-- | The BGP status.
transitGatewayAttachmentBgpConfiguration_bgpStatus :: Lens.Lens' TransitGatewayAttachmentBgpConfiguration (Prelude.Maybe BgpStatus)
transitGatewayAttachmentBgpConfiguration_bgpStatus = Lens.lens (\TransitGatewayAttachmentBgpConfiguration' {bgpStatus} -> bgpStatus) (\s@TransitGatewayAttachmentBgpConfiguration' {} a -> s {bgpStatus = a} :: TransitGatewayAttachmentBgpConfiguration)

-- | The interior BGP peer IP address for the appliance.
transitGatewayAttachmentBgpConfiguration_peerAddress :: Lens.Lens' TransitGatewayAttachmentBgpConfiguration (Prelude.Maybe Prelude.Text)
transitGatewayAttachmentBgpConfiguration_peerAddress = Lens.lens (\TransitGatewayAttachmentBgpConfiguration' {peerAddress} -> peerAddress) (\s@TransitGatewayAttachmentBgpConfiguration' {} a -> s {peerAddress = a} :: TransitGatewayAttachmentBgpConfiguration)

-- | The peer Autonomous System Number (ASN).
transitGatewayAttachmentBgpConfiguration_peerAsn :: Lens.Lens' TransitGatewayAttachmentBgpConfiguration (Prelude.Maybe Prelude.Integer)
transitGatewayAttachmentBgpConfiguration_peerAsn = Lens.lens (\TransitGatewayAttachmentBgpConfiguration' {peerAsn} -> peerAsn) (\s@TransitGatewayAttachmentBgpConfiguration' {} a -> s {peerAsn = a} :: TransitGatewayAttachmentBgpConfiguration)

-- | The interior BGP peer IP address for the transit gateway.
transitGatewayAttachmentBgpConfiguration_transitGatewayAddress :: Lens.Lens' TransitGatewayAttachmentBgpConfiguration (Prelude.Maybe Prelude.Text)
transitGatewayAttachmentBgpConfiguration_transitGatewayAddress = Lens.lens (\TransitGatewayAttachmentBgpConfiguration' {transitGatewayAddress} -> transitGatewayAddress) (\s@TransitGatewayAttachmentBgpConfiguration' {} a -> s {transitGatewayAddress = a} :: TransitGatewayAttachmentBgpConfiguration)

-- | The transit gateway Autonomous System Number (ASN).
transitGatewayAttachmentBgpConfiguration_transitGatewayAsn :: Lens.Lens' TransitGatewayAttachmentBgpConfiguration (Prelude.Maybe Prelude.Integer)
transitGatewayAttachmentBgpConfiguration_transitGatewayAsn = Lens.lens (\TransitGatewayAttachmentBgpConfiguration' {transitGatewayAsn} -> transitGatewayAsn) (\s@TransitGatewayAttachmentBgpConfiguration' {} a -> s {transitGatewayAsn = a} :: TransitGatewayAttachmentBgpConfiguration)

instance
  Data.FromXML
    TransitGatewayAttachmentBgpConfiguration
  where
  parseXML x =
    TransitGatewayAttachmentBgpConfiguration'
      Prelude.<$> (x Data..@? "bgpStatus")
        Prelude.<*> (x Data..@? "peerAddress")
        Prelude.<*> (x Data..@? "peerAsn")
        Prelude.<*> (x Data..@? "transitGatewayAddress")
        Prelude.<*> (x Data..@? "transitGatewayAsn")

instance
  Prelude.Hashable
    TransitGatewayAttachmentBgpConfiguration
  where
  hashWithSalt
    _salt
    TransitGatewayAttachmentBgpConfiguration' {..} =
      _salt `Prelude.hashWithSalt` bgpStatus
        `Prelude.hashWithSalt` peerAddress
        `Prelude.hashWithSalt` peerAsn
        `Prelude.hashWithSalt` transitGatewayAddress
        `Prelude.hashWithSalt` transitGatewayAsn

instance
  Prelude.NFData
    TransitGatewayAttachmentBgpConfiguration
  where
  rnf TransitGatewayAttachmentBgpConfiguration' {..} =
    Prelude.rnf bgpStatus
      `Prelude.seq` Prelude.rnf peerAddress
      `Prelude.seq` Prelude.rnf peerAsn
      `Prelude.seq` Prelude.rnf transitGatewayAddress
      `Prelude.seq` Prelude.rnf transitGatewayAsn
