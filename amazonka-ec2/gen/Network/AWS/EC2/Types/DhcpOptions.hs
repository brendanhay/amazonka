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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DhcpConfiguration
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes a set of DHCP options.
--
-- /See:/ 'newDhcpOptions' smart constructor.
data DhcpOptions = DhcpOptions'
  { -- | The ID of the AWS account that owns the DHCP options set.
    ownerId :: Core.Maybe Core.Text,
    -- | One or more DHCP options in the set.
    dhcpConfigurations :: Core.Maybe [DhcpConfiguration],
    -- | The ID of the set of DHCP options.
    dhcpOptionsId :: Core.Maybe Core.Text,
    -- | Any tags assigned to the DHCP options set.
    tags :: Core.Maybe [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { ownerId = Core.Nothing,
      dhcpConfigurations = Core.Nothing,
      dhcpOptionsId = Core.Nothing,
      tags = Core.Nothing
    }

-- | The ID of the AWS account that owns the DHCP options set.
dhcpOptions_ownerId :: Lens.Lens' DhcpOptions (Core.Maybe Core.Text)
dhcpOptions_ownerId = Lens.lens (\DhcpOptions' {ownerId} -> ownerId) (\s@DhcpOptions' {} a -> s {ownerId = a} :: DhcpOptions)

-- | One or more DHCP options in the set.
dhcpOptions_dhcpConfigurations :: Lens.Lens' DhcpOptions (Core.Maybe [DhcpConfiguration])
dhcpOptions_dhcpConfigurations = Lens.lens (\DhcpOptions' {dhcpConfigurations} -> dhcpConfigurations) (\s@DhcpOptions' {} a -> s {dhcpConfigurations = a} :: DhcpOptions) Core.. Lens.mapping Lens._Coerce

-- | The ID of the set of DHCP options.
dhcpOptions_dhcpOptionsId :: Lens.Lens' DhcpOptions (Core.Maybe Core.Text)
dhcpOptions_dhcpOptionsId = Lens.lens (\DhcpOptions' {dhcpOptionsId} -> dhcpOptionsId) (\s@DhcpOptions' {} a -> s {dhcpOptionsId = a} :: DhcpOptions)

-- | Any tags assigned to the DHCP options set.
dhcpOptions_tags :: Lens.Lens' DhcpOptions (Core.Maybe [Tag])
dhcpOptions_tags = Lens.lens (\DhcpOptions' {tags} -> tags) (\s@DhcpOptions' {} a -> s {tags = a} :: DhcpOptions) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML DhcpOptions where
  parseXML x =
    DhcpOptions'
      Core.<$> (x Core..@? "ownerId")
      Core.<*> ( x Core..@? "dhcpConfigurationSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "dhcpOptionsId")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance Core.Hashable DhcpOptions

instance Core.NFData DhcpOptions
