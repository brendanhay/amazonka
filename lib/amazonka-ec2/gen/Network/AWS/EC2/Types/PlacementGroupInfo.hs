{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PlacementGroupInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PlacementGroupInfo
  ( PlacementGroupInfo (..)
  -- * Smart constructor
  , mkPlacementGroupInfo
  -- * Lenses
  , pgiSupportedStrategies
  ) where

import qualified Network.AWS.EC2.Types.PlacementGroupStrategy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the placement group support of the instance type.
--
-- /See:/ 'mkPlacementGroupInfo' smart constructor.
newtype PlacementGroupInfo = PlacementGroupInfo'
  { supportedStrategies :: Core.Maybe [Types.PlacementGroupStrategy]
    -- ^ The supported placement group types.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PlacementGroupInfo' value with any optional fields omitted.
mkPlacementGroupInfo
    :: PlacementGroupInfo
mkPlacementGroupInfo
  = PlacementGroupInfo'{supportedStrategies = Core.Nothing}

-- | The supported placement group types.
--
-- /Note:/ Consider using 'supportedStrategies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgiSupportedStrategies :: Lens.Lens' PlacementGroupInfo (Core.Maybe [Types.PlacementGroupStrategy])
pgiSupportedStrategies = Lens.field @"supportedStrategies"
{-# INLINEABLE pgiSupportedStrategies #-}
{-# DEPRECATED supportedStrategies "Use generic-lens or generic-optics with 'supportedStrategies' instead"  #-}

instance Core.FromXML PlacementGroupInfo where
        parseXML x
          = PlacementGroupInfo' Core.<$>
              (x Core..@? "supportedStrategies" Core..<@>
                 Core.parseXMLList "item")
