{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DirectConnect.Types.AddressFamily
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a new BGP peer.
--
-- /See:/ 'newNewBGPPeer' smart constructor.
data NewBGPPeer = NewBGPPeer'
  { -- | The authentication key for BGP configuration. This string has a minimum
    -- length of 6 characters and and a maximun lenth of 80 characters.
    authKey :: Prelude.Maybe Prelude.Text,
    -- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
    -- configuration.
    asn :: Prelude.Maybe Prelude.Int,
    -- | The address family for the BGP peer.
    addressFamily :: Prelude.Maybe AddressFamily,
    -- | The IP address assigned to the Amazon interface.
    amazonAddress :: Prelude.Maybe Prelude.Text,
    -- | The IP address assigned to the customer interface.
    customerAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { authKey = Prelude.Nothing,
      asn = Prelude.Nothing,
      addressFamily = Prelude.Nothing,
      amazonAddress = Prelude.Nothing,
      customerAddress = Prelude.Nothing
    }

-- | The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
newBGPPeer_authKey :: Lens.Lens' NewBGPPeer (Prelude.Maybe Prelude.Text)
newBGPPeer_authKey = Lens.lens (\NewBGPPeer' {authKey} -> authKey) (\s@NewBGPPeer' {} a -> s {authKey = a} :: NewBGPPeer)

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
newBGPPeer_asn :: Lens.Lens' NewBGPPeer (Prelude.Maybe Prelude.Int)
newBGPPeer_asn = Lens.lens (\NewBGPPeer' {asn} -> asn) (\s@NewBGPPeer' {} a -> s {asn = a} :: NewBGPPeer)

-- | The address family for the BGP peer.
newBGPPeer_addressFamily :: Lens.Lens' NewBGPPeer (Prelude.Maybe AddressFamily)
newBGPPeer_addressFamily = Lens.lens (\NewBGPPeer' {addressFamily} -> addressFamily) (\s@NewBGPPeer' {} a -> s {addressFamily = a} :: NewBGPPeer)

-- | The IP address assigned to the Amazon interface.
newBGPPeer_amazonAddress :: Lens.Lens' NewBGPPeer (Prelude.Maybe Prelude.Text)
newBGPPeer_amazonAddress = Lens.lens (\NewBGPPeer' {amazonAddress} -> amazonAddress) (\s@NewBGPPeer' {} a -> s {amazonAddress = a} :: NewBGPPeer)

-- | The IP address assigned to the customer interface.
newBGPPeer_customerAddress :: Lens.Lens' NewBGPPeer (Prelude.Maybe Prelude.Text)
newBGPPeer_customerAddress = Lens.lens (\NewBGPPeer' {customerAddress} -> customerAddress) (\s@NewBGPPeer' {} a -> s {customerAddress = a} :: NewBGPPeer)

instance Prelude.Hashable NewBGPPeer

instance Prelude.NFData NewBGPPeer

instance Prelude.ToJSON NewBGPPeer where
  toJSON NewBGPPeer' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("authKey" Prelude..=) Prelude.<$> authKey,
            ("asn" Prelude..=) Prelude.<$> asn,
            ("addressFamily" Prelude..=)
              Prelude.<$> addressFamily,
            ("amazonAddress" Prelude..=)
              Prelude.<$> amazonAddress,
            ("customerAddress" Prelude..=)
              Prelude.<$> customerAddress
          ]
      )
