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
-- Module      : Network.AWS.EC2.Types.DhcpOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DhcpOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DhcpConfiguration
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a set of DHCP options.
--
-- /See:/ 'newDhcpOptions' smart constructor.
data DhcpOptions = DhcpOptions'
  { -- | The ID of the AWS account that owns the DHCP options set.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | One or more DHCP options in the set.
    dhcpConfigurations :: Prelude.Maybe [DhcpConfiguration],
    -- | The ID of the set of DHCP options.
    dhcpOptionsId :: Prelude.Maybe Prelude.Text,
    -- | Any tags assigned to the DHCP options set.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DhcpOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'dhcpOptions_ownerId' - The ID of the AWS account that owns the DHCP options set.
--
-- 'dhcpConfigurations', 'dhcpOptions_dhcpConfigurations' - One or more DHCP options in the set.
--
-- 'dhcpOptionsId', 'dhcpOptions_dhcpOptionsId' - The ID of the set of DHCP options.
--
-- 'tags', 'dhcpOptions_tags' - Any tags assigned to the DHCP options set.
newDhcpOptions ::
  DhcpOptions
newDhcpOptions =
  DhcpOptions'
    { ownerId = Prelude.Nothing,
      dhcpConfigurations = Prelude.Nothing,
      dhcpOptionsId = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ID of the AWS account that owns the DHCP options set.
dhcpOptions_ownerId :: Lens.Lens' DhcpOptions (Prelude.Maybe Prelude.Text)
dhcpOptions_ownerId = Lens.lens (\DhcpOptions' {ownerId} -> ownerId) (\s@DhcpOptions' {} a -> s {ownerId = a} :: DhcpOptions)

-- | One or more DHCP options in the set.
dhcpOptions_dhcpConfigurations :: Lens.Lens' DhcpOptions (Prelude.Maybe [DhcpConfiguration])
dhcpOptions_dhcpConfigurations = Lens.lens (\DhcpOptions' {dhcpConfigurations} -> dhcpConfigurations) (\s@DhcpOptions' {} a -> s {dhcpConfigurations = a} :: DhcpOptions) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the set of DHCP options.
dhcpOptions_dhcpOptionsId :: Lens.Lens' DhcpOptions (Prelude.Maybe Prelude.Text)
dhcpOptions_dhcpOptionsId = Lens.lens (\DhcpOptions' {dhcpOptionsId} -> dhcpOptionsId) (\s@DhcpOptions' {} a -> s {dhcpOptionsId = a} :: DhcpOptions)

-- | Any tags assigned to the DHCP options set.
dhcpOptions_tags :: Lens.Lens' DhcpOptions (Prelude.Maybe [Tag])
dhcpOptions_tags = Lens.lens (\DhcpOptions' {tags} -> tags) (\s@DhcpOptions' {} a -> s {tags = a} :: DhcpOptions) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML DhcpOptions where
  parseXML x =
    DhcpOptions'
      Prelude.<$> (x Prelude..@? "ownerId")
      Prelude.<*> ( x Prelude..@? "dhcpConfigurationSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "dhcpOptionsId")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )

instance Prelude.Hashable DhcpOptions

instance Prelude.NFData DhcpOptions
