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
-- Module      : Amazonka.EC2.Types.DhcpOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DhcpOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DhcpConfiguration
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a set of DHCP options.
--
-- /See:/ 'newDhcpOptions' smart constructor.
data DhcpOptions = DhcpOptions'
  { -- | One or more DHCP options in the set.
    dhcpConfigurations :: Prelude.Maybe [DhcpConfiguration],
    -- | The ID of the set of DHCP options.
    dhcpOptionsId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the DHCP options
    -- set.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | Any tags assigned to the DHCP options set.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DhcpOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dhcpConfigurations', 'dhcpOptions_dhcpConfigurations' - One or more DHCP options in the set.
--
-- 'dhcpOptionsId', 'dhcpOptions_dhcpOptionsId' - The ID of the set of DHCP options.
--
-- 'ownerId', 'dhcpOptions_ownerId' - The ID of the Amazon Web Services account that owns the DHCP options
-- set.
--
-- 'tags', 'dhcpOptions_tags' - Any tags assigned to the DHCP options set.
newDhcpOptions ::
  DhcpOptions
newDhcpOptions =
  DhcpOptions'
    { dhcpConfigurations = Prelude.Nothing,
      dhcpOptionsId = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | One or more DHCP options in the set.
dhcpOptions_dhcpConfigurations :: Lens.Lens' DhcpOptions (Prelude.Maybe [DhcpConfiguration])
dhcpOptions_dhcpConfigurations = Lens.lens (\DhcpOptions' {dhcpConfigurations} -> dhcpConfigurations) (\s@DhcpOptions' {} a -> s {dhcpConfigurations = a} :: DhcpOptions) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the set of DHCP options.
dhcpOptions_dhcpOptionsId :: Lens.Lens' DhcpOptions (Prelude.Maybe Prelude.Text)
dhcpOptions_dhcpOptionsId = Lens.lens (\DhcpOptions' {dhcpOptionsId} -> dhcpOptionsId) (\s@DhcpOptions' {} a -> s {dhcpOptionsId = a} :: DhcpOptions)

-- | The ID of the Amazon Web Services account that owns the DHCP options
-- set.
dhcpOptions_ownerId :: Lens.Lens' DhcpOptions (Prelude.Maybe Prelude.Text)
dhcpOptions_ownerId = Lens.lens (\DhcpOptions' {ownerId} -> ownerId) (\s@DhcpOptions' {} a -> s {ownerId = a} :: DhcpOptions)

-- | Any tags assigned to the DHCP options set.
dhcpOptions_tags :: Lens.Lens' DhcpOptions (Prelude.Maybe [Tag])
dhcpOptions_tags = Lens.lens (\DhcpOptions' {tags} -> tags) (\s@DhcpOptions' {} a -> s {tags = a} :: DhcpOptions) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML DhcpOptions where
  parseXML x =
    DhcpOptions'
      Prelude.<$> ( x
                      Data..@? "dhcpConfigurationSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "dhcpOptionsId")
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable DhcpOptions where
  hashWithSalt _salt DhcpOptions' {..} =
    _salt
      `Prelude.hashWithSalt` dhcpConfigurations
      `Prelude.hashWithSalt` dhcpOptionsId
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` tags

instance Prelude.NFData DhcpOptions where
  rnf DhcpOptions' {..} =
    Prelude.rnf dhcpConfigurations
      `Prelude.seq` Prelude.rnf dhcpOptionsId
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf tags
