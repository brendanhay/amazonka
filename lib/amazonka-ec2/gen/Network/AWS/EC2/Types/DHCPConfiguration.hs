{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DHCPConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DHCPConfiguration
  ( DHCPConfiguration (..),

    -- * Smart constructor
    mkDHCPConfiguration,

    -- * Lenses
    dcValues,
    dcKey,
  )
where

import Network.AWS.EC2.Types.AttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a DHCP configuration option.
--
-- /See:/ 'mkDHCPConfiguration' smart constructor.
data DHCPConfiguration = DHCPConfiguration'
  { -- | One or more values for the DHCP option.
    values :: Lude.Maybe [AttributeValue],
    -- | The name of a DHCP option.
    key :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DHCPConfiguration' with the minimum fields required to make a request.
--
-- * 'values' - One or more values for the DHCP option.
-- * 'key' - The name of a DHCP option.
mkDHCPConfiguration ::
  DHCPConfiguration
mkDHCPConfiguration =
  DHCPConfiguration' {values = Lude.Nothing, key = Lude.Nothing}

-- | One or more values for the DHCP option.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcValues :: Lens.Lens' DHCPConfiguration (Lude.Maybe [AttributeValue])
dcValues = Lens.lens (values :: DHCPConfiguration -> Lude.Maybe [AttributeValue]) (\s a -> s {values = a} :: DHCPConfiguration)
{-# DEPRECATED dcValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The name of a DHCP option.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcKey :: Lens.Lens' DHCPConfiguration (Lude.Maybe Lude.Text)
dcKey = Lens.lens (key :: DHCPConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: DHCPConfiguration)
{-# DEPRECATED dcKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromXML DHCPConfiguration where
  parseXML x =
    DHCPConfiguration'
      Lude.<$> ( x Lude..@? "valueSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "key")
