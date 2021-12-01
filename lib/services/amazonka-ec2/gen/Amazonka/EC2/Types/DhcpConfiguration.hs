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
-- Module      : Amazonka.EC2.Types.DhcpConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DhcpConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AttributeValue
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a DHCP configuration option.
--
-- /See:/ 'newDhcpConfiguration' smart constructor.
data DhcpConfiguration = DhcpConfiguration'
  { -- | One or more values for the DHCP option.
    values :: Prelude.Maybe [AttributeValue],
    -- | The name of a DHCP option.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DhcpConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'dhcpConfiguration_values' - One or more values for the DHCP option.
--
-- 'key', 'dhcpConfiguration_key' - The name of a DHCP option.
newDhcpConfiguration ::
  DhcpConfiguration
newDhcpConfiguration =
  DhcpConfiguration'
    { values = Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | One or more values for the DHCP option.
dhcpConfiguration_values :: Lens.Lens' DhcpConfiguration (Prelude.Maybe [AttributeValue])
dhcpConfiguration_values = Lens.lens (\DhcpConfiguration' {values} -> values) (\s@DhcpConfiguration' {} a -> s {values = a} :: DhcpConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of a DHCP option.
dhcpConfiguration_key :: Lens.Lens' DhcpConfiguration (Prelude.Maybe Prelude.Text)
dhcpConfiguration_key = Lens.lens (\DhcpConfiguration' {key} -> key) (\s@DhcpConfiguration' {} a -> s {key = a} :: DhcpConfiguration)

instance Core.FromXML DhcpConfiguration where
  parseXML x =
    DhcpConfiguration'
      Prelude.<$> ( x Core..@? "valueSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "key")

instance Prelude.Hashable DhcpConfiguration where
  hashWithSalt salt' DhcpConfiguration' {..} =
    salt' `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values

instance Prelude.NFData DhcpConfiguration where
  rnf DhcpConfiguration' {..} =
    Prelude.rnf values `Prelude.seq` Prelude.rnf key
