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
-- Module      : Network.AWS.EC2.Types.PrivateIpAddressSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrivateIpAddressSpecification where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a secondary private IPv4 address for a network interface.
--
-- /See:/ 'newPrivateIpAddressSpecification' smart constructor.
data PrivateIpAddressSpecification = PrivateIpAddressSpecification'
  { -- | Indicates whether the private IPv4 address is the primary private IPv4
    -- address. Only one IPv4 address can be designated as primary.
    primary :: Prelude.Maybe Prelude.Bool,
    -- | The private IPv4 addresses.
    privateIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PrivateIpAddressSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'primary', 'privateIpAddressSpecification_primary' - Indicates whether the private IPv4 address is the primary private IPv4
-- address. Only one IPv4 address can be designated as primary.
--
-- 'privateIpAddress', 'privateIpAddressSpecification_privateIpAddress' - The private IPv4 addresses.
newPrivateIpAddressSpecification ::
  PrivateIpAddressSpecification
newPrivateIpAddressSpecification =
  PrivateIpAddressSpecification'
    { primary =
        Prelude.Nothing,
      privateIpAddress = Prelude.Nothing
    }

-- | Indicates whether the private IPv4 address is the primary private IPv4
-- address. Only one IPv4 address can be designated as primary.
privateIpAddressSpecification_primary :: Lens.Lens' PrivateIpAddressSpecification (Prelude.Maybe Prelude.Bool)
privateIpAddressSpecification_primary = Lens.lens (\PrivateIpAddressSpecification' {primary} -> primary) (\s@PrivateIpAddressSpecification' {} a -> s {primary = a} :: PrivateIpAddressSpecification)

-- | The private IPv4 addresses.
privateIpAddressSpecification_privateIpAddress :: Lens.Lens' PrivateIpAddressSpecification (Prelude.Maybe Prelude.Text)
privateIpAddressSpecification_privateIpAddress = Lens.lens (\PrivateIpAddressSpecification' {privateIpAddress} -> privateIpAddress) (\s@PrivateIpAddressSpecification' {} a -> s {privateIpAddress = a} :: PrivateIpAddressSpecification)

instance
  Prelude.FromXML
    PrivateIpAddressSpecification
  where
  parseXML x =
    PrivateIpAddressSpecification'
      Prelude.<$> (x Prelude..@? "primary")
      Prelude.<*> (x Prelude..@? "privateIpAddress")

instance
  Prelude.Hashable
    PrivateIpAddressSpecification

instance Prelude.NFData PrivateIpAddressSpecification

instance
  Prelude.ToQuery
    PrivateIpAddressSpecification
  where
  toQuery PrivateIpAddressSpecification' {..} =
    Prelude.mconcat
      [ "Primary" Prelude.=: primary,
        "PrivateIpAddress" Prelude.=: privateIpAddress
      ]
