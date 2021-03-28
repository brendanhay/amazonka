{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.SubnetOutpost
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.SubnetOutpost
  ( SubnetOutpost (..)
  -- * Smart constructor
  , mkSubnetOutpost
  -- * Lenses
  , soSubnetOutpostArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The ID of the outpost subnet.
--
-- /See:/ 'mkSubnetOutpost' smart constructor.
newtype SubnetOutpost = SubnetOutpost'
  { subnetOutpostArn :: Core.Maybe Core.Text
    -- ^ The outpost ARN of the subnet.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SubnetOutpost' value with any optional fields omitted.
mkSubnetOutpost
    :: SubnetOutpost
mkSubnetOutpost = SubnetOutpost'{subnetOutpostArn = Core.Nothing}

-- | The outpost ARN of the subnet.
--
-- /Note:/ Consider using 'subnetOutpostArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soSubnetOutpostArn :: Lens.Lens' SubnetOutpost (Core.Maybe Core.Text)
soSubnetOutpostArn = Lens.field @"subnetOutpostArn"
{-# INLINEABLE soSubnetOutpostArn #-}
{-# DEPRECATED subnetOutpostArn "Use generic-lens or generic-optics with 'subnetOutpostArn' instead"  #-}

instance Core.FromXML SubnetOutpost where
        parseXML x
          = SubnetOutpost' Core.<$> (x Core..@? "SubnetOutpostArn")
