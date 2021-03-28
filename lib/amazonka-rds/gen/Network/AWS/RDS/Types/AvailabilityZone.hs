{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.AvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.AvailabilityZone
  ( AvailabilityZone (..)
  -- * Smart constructor
  , mkAvailabilityZone
  -- * Lenses
  , azName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains Availability Zone information.
--
-- This data type is used as an element in the @OrderableDBInstanceOption@ data type.
--
-- /See:/ 'mkAvailabilityZone' smart constructor.
newtype AvailabilityZone = AvailabilityZone'
  { name :: Core.Maybe Core.Text
    -- ^ The name of the Availability Zone.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AvailabilityZone' value with any optional fields omitted.
mkAvailabilityZone
    :: AvailabilityZone
mkAvailabilityZone = AvailabilityZone'{name = Core.Nothing}

-- | The name of the Availability Zone.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azName :: Lens.Lens' AvailabilityZone (Core.Maybe Core.Text)
azName = Lens.field @"name"
{-# INLINEABLE azName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromXML AvailabilityZone where
        parseXML x = AvailabilityZone' Core.<$> (x Core..@? "Name")
