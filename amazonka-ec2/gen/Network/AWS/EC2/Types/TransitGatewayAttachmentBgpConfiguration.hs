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
-- Module      : Network.AWS.EC2.Types.TransitGatewayAttachmentBgpConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAttachmentBgpConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.BgpStatus
import qualified Network.AWS.Lens as Lens

-- | The BGP configuration information.
--
-- /See:/ 'newTransitGatewayAttachmentBgpConfiguration' smart constructor.
data TransitGatewayAttachmentBgpConfiguration = TransitGatewayAttachmentBgpConfiguration'
  { -- | The interior BGP peer IP address for the appliance.
    peerAddress :: Core.Maybe Core.Text,
    -- | The peer Autonomous System Number (ASN).
    peerAsn :: Core.Maybe Core.Integer,
    -- | The BGP status.
    bgpStatus :: Core.Maybe BgpStatus,
    -- | The interior BGP peer IP address for the transit gateway.
    transitGatewayAddress :: Core.Maybe Core.Text,
    -- | The transit gateway Autonomous System Number (ASN).
    transitGatewayAsn :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitGatewayAttachmentBgpConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'peerAddress', 'transitGatewayAttachmentBgpConfiguration_peerAddress' - The interior BGP peer IP address for the appliance.
--
-- 'peerAsn', 'transitGatewayAttachmentBgpConfiguration_peerAsn' - The peer Autonomous System Number (ASN).
--
-- 'bgpStatus', 'transitGatewayAttachmentBgpConfiguration_bgpStatus' - The BGP status.
--
-- 'transitGatewayAddress', 'transitGatewayAttachmentBgpConfiguration_transitGatewayAddress' - The interior BGP peer IP address for the transit gateway.
--
-- 'transitGatewayAsn', 'transitGatewayAttachmentBgpConfiguration_transitGatewayAsn' - The transit gateway Autonomous System Number (ASN).
newTransitGatewayAttachmentBgpConfiguration ::
  TransitGatewayAttachmentBgpConfiguration
newTransitGatewayAttachmentBgpConfiguration =
  TransitGatewayAttachmentBgpConfiguration'
    { peerAddress =
        Core.Nothing,
      peerAsn = Core.Nothing,
      bgpStatus = Core.Nothing,
      transitGatewayAddress =
        Core.Nothing,
      transitGatewayAsn = Core.Nothing
    }

-- | The interior BGP peer IP address for the appliance.
transitGatewayAttachmentBgpConfiguration_peerAddress :: Lens.Lens' TransitGatewayAttachmentBgpConfiguration (Core.Maybe Core.Text)
transitGatewayAttachmentBgpConfiguration_peerAddress = Lens.lens (\TransitGatewayAttachmentBgpConfiguration' {peerAddress} -> peerAddress) (\s@TransitGatewayAttachmentBgpConfiguration' {} a -> s {peerAddress = a} :: TransitGatewayAttachmentBgpConfiguration)

-- | The peer Autonomous System Number (ASN).
transitGatewayAttachmentBgpConfiguration_peerAsn :: Lens.Lens' TransitGatewayAttachmentBgpConfiguration (Core.Maybe Core.Integer)
transitGatewayAttachmentBgpConfiguration_peerAsn = Lens.lens (\TransitGatewayAttachmentBgpConfiguration' {peerAsn} -> peerAsn) (\s@TransitGatewayAttachmentBgpConfiguration' {} a -> s {peerAsn = a} :: TransitGatewayAttachmentBgpConfiguration)

-- | The BGP status.
transitGatewayAttachmentBgpConfiguration_bgpStatus :: Lens.Lens' TransitGatewayAttachmentBgpConfiguration (Core.Maybe BgpStatus)
transitGatewayAttachmentBgpConfiguration_bgpStatus = Lens.lens (\TransitGatewayAttachmentBgpConfiguration' {bgpStatus} -> bgpStatus) (\s@TransitGatewayAttachmentBgpConfiguration' {} a -> s {bgpStatus = a} :: TransitGatewayAttachmentBgpConfiguration)

-- | The interior BGP peer IP address for the transit gateway.
transitGatewayAttachmentBgpConfiguration_transitGatewayAddress :: Lens.Lens' TransitGatewayAttachmentBgpConfiguration (Core.Maybe Core.Text)
transitGatewayAttachmentBgpConfiguration_transitGatewayAddress = Lens.lens (\TransitGatewayAttachmentBgpConfiguration' {transitGatewayAddress} -> transitGatewayAddress) (\s@TransitGatewayAttachmentBgpConfiguration' {} a -> s {transitGatewayAddress = a} :: TransitGatewayAttachmentBgpConfiguration)

-- | The transit gateway Autonomous System Number (ASN).
transitGatewayAttachmentBgpConfiguration_transitGatewayAsn :: Lens.Lens' TransitGatewayAttachmentBgpConfiguration (Core.Maybe Core.Integer)
transitGatewayAttachmentBgpConfiguration_transitGatewayAsn = Lens.lens (\TransitGatewayAttachmentBgpConfiguration' {transitGatewayAsn} -> transitGatewayAsn) (\s@TransitGatewayAttachmentBgpConfiguration' {} a -> s {transitGatewayAsn = a} :: TransitGatewayAttachmentBgpConfiguration)

instance
  Core.FromXML
    TransitGatewayAttachmentBgpConfiguration
  where
  parseXML x =
    TransitGatewayAttachmentBgpConfiguration'
      Core.<$> (x Core..@? "peerAddress")
      Core.<*> (x Core..@? "peerAsn")
      Core.<*> (x Core..@? "bgpStatus")
      Core.<*> (x Core..@? "transitGatewayAddress")
      Core.<*> (x Core..@? "transitGatewayAsn")

instance
  Core.Hashable
    TransitGatewayAttachmentBgpConfiguration

instance
  Core.NFData
    TransitGatewayAttachmentBgpConfiguration
