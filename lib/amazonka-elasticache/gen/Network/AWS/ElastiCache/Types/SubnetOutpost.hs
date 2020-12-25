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
    soSubnetOutpostArn,
  )
where

import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The ID of the outpost subnet.
--
-- /See:/ 'mkSubnetOutpost' smart constructor.
newtype SubnetOutpost = SubnetOutpost'
  { -- | The outpost ARN of the subnet.
    subnetOutpostArn :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SubnetOutpost' value with any optional fields omitted.
mkSubnetOutpost ::
  SubnetOutpost
mkSubnetOutpost = SubnetOutpost' {subnetOutpostArn = Core.Nothing}

-- | The outpost ARN of the subnet.
--
-- /Note:/ Consider using 'subnetOutpostArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soSubnetOutpostArn :: Lens.Lens' SubnetOutpost (Core.Maybe Types.String)
soSubnetOutpostArn = Lens.field @"subnetOutpostArn"
{-# DEPRECATED soSubnetOutpostArn "Use generic-lens or generic-optics with 'subnetOutpostArn' instead." #-}

instance Core.FromXML SubnetOutpost where
  parseXML x =
    SubnetOutpost' Core.<$> (x Core..@? "SubnetOutpostArn")
