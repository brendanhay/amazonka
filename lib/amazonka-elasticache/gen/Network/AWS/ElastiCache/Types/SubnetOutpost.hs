{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.SubnetOutpost
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.SubnetOutpost
  ( SubnetOutpost (..),

    -- * Smart constructor
    mkSubnetOutpost,

    -- * Lenses
    soSubnetOutpostARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The ID of the outpost subnet.
--
-- /See:/ 'mkSubnetOutpost' smart constructor.
newtype SubnetOutpost = SubnetOutpost'
  { subnetOutpostARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubnetOutpost' with the minimum fields required to make a request.
--
-- * 'subnetOutpostARN' - The outpost ARN of the subnet.
mkSubnetOutpost ::
  SubnetOutpost
mkSubnetOutpost = SubnetOutpost' {subnetOutpostARN = Lude.Nothing}

-- | The outpost ARN of the subnet.
--
-- /Note:/ Consider using 'subnetOutpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soSubnetOutpostARN :: Lens.Lens' SubnetOutpost (Lude.Maybe Lude.Text)
soSubnetOutpostARN = Lens.lens (subnetOutpostARN :: SubnetOutpost -> Lude.Maybe Lude.Text) (\s a -> s {subnetOutpostARN = a} :: SubnetOutpost)
{-# DEPRECATED soSubnetOutpostARN "Use generic-lens or generic-optics with 'subnetOutpostARN' instead." #-}

instance Lude.FromXML SubnetOutpost where
  parseXML x =
    SubnetOutpost' Lude.<$> (x Lude..@? "SubnetOutpostArn")
