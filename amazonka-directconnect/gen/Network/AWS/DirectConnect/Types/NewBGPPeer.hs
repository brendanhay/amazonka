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
-- Module      : Network.AWS.DirectConnect.Types.NewBGPPeer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.NewBGPPeer where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types.AddressFamily
import qualified Network.AWS.Lens as Lens

-- | Information about a new BGP peer.
--
-- /See:/ 'newNewBGPPeer' smart constructor.
data NewBGPPeer = NewBGPPeer'
  { -- | The authentication key for BGP configuration. This string has a minimum
    -- length of 6 characters and and a maximun lenth of 80 characters.
    authKey :: Core.Maybe Core.Text,
    -- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
    -- configuration.
    asn :: Core.Maybe Core.Int,
    -- | The address family for the BGP peer.
    addressFamily :: Core.Maybe AddressFamily,
    -- | The IP address assigned to the Amazon interface.
    amazonAddress :: Core.Maybe Core.Text,
    -- | The IP address assigned to the customer interface.
    customerAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NewBGPPeer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authKey', 'newBGPPeer_authKey' - The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
--
-- 'asn', 'newBGPPeer_asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- 'addressFamily', 'newBGPPeer_addressFamily' - The address family for the BGP peer.
--
-- 'amazonAddress', 'newBGPPeer_amazonAddress' - The IP address assigned to the Amazon interface.
--
-- 'customerAddress', 'newBGPPeer_customerAddress' - The IP address assigned to the customer interface.
newNewBGPPeer ::
  NewBGPPeer
newNewBGPPeer =
  NewBGPPeer'
    { authKey = Core.Nothing,
      asn = Core.Nothing,
      addressFamily = Core.Nothing,
      amazonAddress = Core.Nothing,
      customerAddress = Core.Nothing
    }

-- | The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
newBGPPeer_authKey :: Lens.Lens' NewBGPPeer (Core.Maybe Core.Text)
newBGPPeer_authKey = Lens.lens (\NewBGPPeer' {authKey} -> authKey) (\s@NewBGPPeer' {} a -> s {authKey = a} :: NewBGPPeer)

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
newBGPPeer_asn :: Lens.Lens' NewBGPPeer (Core.Maybe Core.Int)
newBGPPeer_asn = Lens.lens (\NewBGPPeer' {asn} -> asn) (\s@NewBGPPeer' {} a -> s {asn = a} :: NewBGPPeer)

-- | The address family for the BGP peer.
newBGPPeer_addressFamily :: Lens.Lens' NewBGPPeer (Core.Maybe AddressFamily)
newBGPPeer_addressFamily = Lens.lens (\NewBGPPeer' {addressFamily} -> addressFamily) (\s@NewBGPPeer' {} a -> s {addressFamily = a} :: NewBGPPeer)

-- | The IP address assigned to the Amazon interface.
newBGPPeer_amazonAddress :: Lens.Lens' NewBGPPeer (Core.Maybe Core.Text)
newBGPPeer_amazonAddress = Lens.lens (\NewBGPPeer' {amazonAddress} -> amazonAddress) (\s@NewBGPPeer' {} a -> s {amazonAddress = a} :: NewBGPPeer)

-- | The IP address assigned to the customer interface.
newBGPPeer_customerAddress :: Lens.Lens' NewBGPPeer (Core.Maybe Core.Text)
newBGPPeer_customerAddress = Lens.lens (\NewBGPPeer' {customerAddress} -> customerAddress) (\s@NewBGPPeer' {} a -> s {customerAddress = a} :: NewBGPPeer)

instance Core.Hashable NewBGPPeer

instance Core.NFData NewBGPPeer

instance Core.ToJSON NewBGPPeer where
  toJSON NewBGPPeer' {..} =
    Core.object
      ( Core.catMaybes
          [ ("authKey" Core..=) Core.<$> authKey,
            ("asn" Core..=) Core.<$> asn,
            ("addressFamily" Core..=) Core.<$> addressFamily,
            ("amazonAddress" Core..=) Core.<$> amazonAddress,
            ("customerAddress" Core..=)
              Core.<$> customerAddress
          ]
      )
