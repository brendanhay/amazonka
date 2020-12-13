{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DHCPOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DHCPOptions
  ( DHCPOptions (..),

    -- * Smart constructor
    mkDHCPOptions,

    -- * Lenses
    doDHCPConfigurations,
    doOwnerId,
    doDHCPOptionsId,
    doTags,
  )
where

import Network.AWS.EC2.Types.DHCPConfiguration
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a set of DHCP options.
--
-- /See:/ 'mkDHCPOptions' smart constructor.
data DHCPOptions = DHCPOptions'
  { -- | One or more DHCP options in the set.
    dhcpConfigurations :: Lude.Maybe [DHCPConfiguration],
    -- | The ID of the AWS account that owns the DHCP options set.
    ownerId :: Lude.Maybe Lude.Text,
    -- | The ID of the set of DHCP options.
    dhcpOptionsId :: Lude.Maybe Lude.Text,
    -- | Any tags assigned to the DHCP options set.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DHCPOptions' with the minimum fields required to make a request.
--
-- * 'dhcpConfigurations' - One or more DHCP options in the set.
-- * 'ownerId' - The ID of the AWS account that owns the DHCP options set.
-- * 'dhcpOptionsId' - The ID of the set of DHCP options.
-- * 'tags' - Any tags assigned to the DHCP options set.
mkDHCPOptions ::
  DHCPOptions
mkDHCPOptions =
  DHCPOptions'
    { dhcpConfigurations = Lude.Nothing,
      ownerId = Lude.Nothing,
      dhcpOptionsId = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | One or more DHCP options in the set.
--
-- /Note:/ Consider using 'dhcpConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doDHCPConfigurations :: Lens.Lens' DHCPOptions (Lude.Maybe [DHCPConfiguration])
doDHCPConfigurations = Lens.lens (dhcpConfigurations :: DHCPOptions -> Lude.Maybe [DHCPConfiguration]) (\s a -> s {dhcpConfigurations = a} :: DHCPOptions)
{-# DEPRECATED doDHCPConfigurations "Use generic-lens or generic-optics with 'dhcpConfigurations' instead." #-}

-- | The ID of the AWS account that owns the DHCP options set.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doOwnerId :: Lens.Lens' DHCPOptions (Lude.Maybe Lude.Text)
doOwnerId = Lens.lens (ownerId :: DHCPOptions -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: DHCPOptions)
{-# DEPRECATED doOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The ID of the set of DHCP options.
--
-- /Note:/ Consider using 'dhcpOptionsId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doDHCPOptionsId :: Lens.Lens' DHCPOptions (Lude.Maybe Lude.Text)
doDHCPOptionsId = Lens.lens (dhcpOptionsId :: DHCPOptions -> Lude.Maybe Lude.Text) (\s a -> s {dhcpOptionsId = a} :: DHCPOptions)
{-# DEPRECATED doDHCPOptionsId "Use generic-lens or generic-optics with 'dhcpOptionsId' instead." #-}

-- | Any tags assigned to the DHCP options set.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doTags :: Lens.Lens' DHCPOptions (Lude.Maybe [Tag])
doTags = Lens.lens (tags :: DHCPOptions -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: DHCPOptions)
{-# DEPRECATED doTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML DHCPOptions where
  parseXML x =
    DHCPOptions'
      Lude.<$> ( x Lude..@? "dhcpConfigurationSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "dhcpOptionsId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
