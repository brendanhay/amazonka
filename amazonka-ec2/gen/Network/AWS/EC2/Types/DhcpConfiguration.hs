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
-- Module      : Network.AWS.EC2.Types.DhcpConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DhcpConfiguration where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a DHCP configuration option.
--
-- /See:/ 'newDhcpConfiguration' smart constructor.
data DhcpConfiguration = DhcpConfiguration'
  { -- | The name of a DHCP option.
    key :: Prelude.Maybe Prelude.Text,
    -- | One or more values for the DHCP option.
    values :: Prelude.Maybe [AttributeValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DhcpConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'dhcpConfiguration_key' - The name of a DHCP option.
--
-- 'values', 'dhcpConfiguration_values' - One or more values for the DHCP option.
newDhcpConfiguration ::
  DhcpConfiguration
newDhcpConfiguration =
  DhcpConfiguration'
    { key = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name of a DHCP option.
dhcpConfiguration_key :: Lens.Lens' DhcpConfiguration (Prelude.Maybe Prelude.Text)
dhcpConfiguration_key = Lens.lens (\DhcpConfiguration' {key} -> key) (\s@DhcpConfiguration' {} a -> s {key = a} :: DhcpConfiguration)

-- | One or more values for the DHCP option.
dhcpConfiguration_values :: Lens.Lens' DhcpConfiguration (Prelude.Maybe [AttributeValue])
dhcpConfiguration_values = Lens.lens (\DhcpConfiguration' {values} -> values) (\s@DhcpConfiguration' {} a -> s {values = a} :: DhcpConfiguration) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML DhcpConfiguration where
  parseXML x =
    DhcpConfiguration'
      Prelude.<$> (x Prelude..@? "key")
      Prelude.<*> ( x Prelude..@? "valueSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )

instance Prelude.Hashable DhcpConfiguration

instance Prelude.NFData DhcpConfiguration
