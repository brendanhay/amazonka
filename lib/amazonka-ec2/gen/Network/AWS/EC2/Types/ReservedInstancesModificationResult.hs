{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstancesModificationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ReservedInstancesModificationResult
  ( ReservedInstancesModificationResult (..)
  -- * Smart constructor
  , mkReservedInstancesModificationResult
  -- * Lenses
  , rimrReservedInstancesId
  , rimrTargetConfiguration
  ) where

import qualified Network.AWS.EC2.Types.ReservedInstancesConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the modification request/s.
--
-- /See:/ 'mkReservedInstancesModificationResult' smart constructor.
data ReservedInstancesModificationResult = ReservedInstancesModificationResult'
  { reservedInstancesId :: Core.Maybe Core.Text
    -- ^ The ID for the Reserved Instances that were created as part of the modification request. This field is only available when the modification is fulfilled.
  , targetConfiguration :: Core.Maybe Types.ReservedInstancesConfiguration
    -- ^ The target Reserved Instances configurations supplied as part of the modification request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReservedInstancesModificationResult' value with any optional fields omitted.
mkReservedInstancesModificationResult
    :: ReservedInstancesModificationResult
mkReservedInstancesModificationResult
  = ReservedInstancesModificationResult'{reservedInstancesId =
                                           Core.Nothing,
                                         targetConfiguration = Core.Nothing}

-- | The ID for the Reserved Instances that were created as part of the modification request. This field is only available when the modification is fulfilled.
--
-- /Note:/ Consider using 'reservedInstancesId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimrReservedInstancesId :: Lens.Lens' ReservedInstancesModificationResult (Core.Maybe Core.Text)
rimrReservedInstancesId = Lens.field @"reservedInstancesId"
{-# INLINEABLE rimrReservedInstancesId #-}
{-# DEPRECATED reservedInstancesId "Use generic-lens or generic-optics with 'reservedInstancesId' instead"  #-}

-- | The target Reserved Instances configurations supplied as part of the modification request.
--
-- /Note:/ Consider using 'targetConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimrTargetConfiguration :: Lens.Lens' ReservedInstancesModificationResult (Core.Maybe Types.ReservedInstancesConfiguration)
rimrTargetConfiguration = Lens.field @"targetConfiguration"
{-# INLINEABLE rimrTargetConfiguration #-}
{-# DEPRECATED targetConfiguration "Use generic-lens or generic-optics with 'targetConfiguration' instead"  #-}

instance Core.FromXML ReservedInstancesModificationResult where
        parseXML x
          = ReservedInstancesModificationResult' Core.<$>
              (x Core..@? "reservedInstancesId") Core.<*>
                x Core..@? "targetConfiguration"
