{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PlacementResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PlacementResponse
  ( PlacementResponse (..)
  -- * Smart constructor
  , mkPlacementResponse
  -- * Lenses
  , prGroupName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the placement of an instance.
--
-- /See:/ 'mkPlacementResponse' smart constructor.
newtype PlacementResponse = PlacementResponse'
  { groupName :: Core.Maybe Core.Text
    -- ^ The name of the placement group that the instance is in.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PlacementResponse' value with any optional fields omitted.
mkPlacementResponse
    :: PlacementResponse
mkPlacementResponse = PlacementResponse'{groupName = Core.Nothing}

-- | The name of the placement group that the instance is in.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prGroupName :: Lens.Lens' PlacementResponse (Core.Maybe Core.Text)
prGroupName = Lens.field @"groupName"
{-# INLINEABLE prGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

instance Core.FromXML PlacementResponse where
        parseXML x = PlacementResponse' Core.<$> (x Core..@? "groupName")
