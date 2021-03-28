{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.AvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.AvailabilityZone
  ( AvailabilityZone (..)
  -- * Smart constructor
  , mkAvailabilityZone
  -- * Lenses
  , azName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Name of the availability zone.
--
-- /See:/ 'mkAvailabilityZone' smart constructor.
newtype AvailabilityZone = AvailabilityZone'
  { name :: Core.Maybe Core.Text
    -- ^ Id for the availability zone.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AvailabilityZone' value with any optional fields omitted.
mkAvailabilityZone
    :: AvailabilityZone
mkAvailabilityZone = AvailabilityZone'{name = Core.Nothing}

-- | Id for the availability zone.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azName :: Lens.Lens' AvailabilityZone (Core.Maybe Core.Text)
azName = Lens.field @"name"
{-# INLINEABLE azName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON AvailabilityZone where
        parseJSON
          = Core.withObject "AvailabilityZone" Core.$
              \ x -> AvailabilityZone' Core.<$> (x Core..:? "name")
