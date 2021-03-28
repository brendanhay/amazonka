{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstancesId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ReservedInstancesId
  ( ReservedInstancesId (..)
  -- * Smart constructor
  , mkReservedInstancesId
  -- * Lenses
  , riiReservedInstancesId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the ID of a Reserved Instance.
--
-- /See:/ 'mkReservedInstancesId' smart constructor.
newtype ReservedInstancesId = ReservedInstancesId'
  { reservedInstancesId :: Core.Maybe Core.Text
    -- ^ The ID of the Reserved Instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ReservedInstancesId' value with any optional fields omitted.
mkReservedInstancesId
    :: ReservedInstancesId
mkReservedInstancesId
  = ReservedInstancesId'{reservedInstancesId = Core.Nothing}

-- | The ID of the Reserved Instance.
--
-- /Note:/ Consider using 'reservedInstancesId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riiReservedInstancesId :: Lens.Lens' ReservedInstancesId (Core.Maybe Core.Text)
riiReservedInstancesId = Lens.field @"reservedInstancesId"
{-# INLINEABLE riiReservedInstancesId #-}
{-# DEPRECATED reservedInstancesId "Use generic-lens or generic-optics with 'reservedInstancesId' instead"  #-}

instance Core.FromXML ReservedInstancesId where
        parseXML x
          = ReservedInstancesId' Core.<$> (x Core..@? "reservedInstancesId")
