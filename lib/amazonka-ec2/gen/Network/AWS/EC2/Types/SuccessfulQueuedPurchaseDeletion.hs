{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SuccessfulQueuedPurchaseDeletion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SuccessfulQueuedPurchaseDeletion
  ( SuccessfulQueuedPurchaseDeletion (..)
  -- * Smart constructor
  , mkSuccessfulQueuedPurchaseDeletion
  -- * Lenses
  , sqpdReservedInstancesId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Reserved Instance whose queued purchase was successfully deleted.
--
-- /See:/ 'mkSuccessfulQueuedPurchaseDeletion' smart constructor.
newtype SuccessfulQueuedPurchaseDeletion = SuccessfulQueuedPurchaseDeletion'
  { reservedInstancesId :: Core.Maybe Core.Text
    -- ^ The ID of the Reserved Instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SuccessfulQueuedPurchaseDeletion' value with any optional fields omitted.
mkSuccessfulQueuedPurchaseDeletion
    :: SuccessfulQueuedPurchaseDeletion
mkSuccessfulQueuedPurchaseDeletion
  = SuccessfulQueuedPurchaseDeletion'{reservedInstancesId =
                                        Core.Nothing}

-- | The ID of the Reserved Instance.
--
-- /Note:/ Consider using 'reservedInstancesId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqpdReservedInstancesId :: Lens.Lens' SuccessfulQueuedPurchaseDeletion (Core.Maybe Core.Text)
sqpdReservedInstancesId = Lens.field @"reservedInstancesId"
{-# INLINEABLE sqpdReservedInstancesId #-}
{-# DEPRECATED reservedInstancesId "Use generic-lens or generic-optics with 'reservedInstancesId' instead"  #-}

instance Core.FromXML SuccessfulQueuedPurchaseDeletion where
        parseXML x
          = SuccessfulQueuedPurchaseDeletion' Core.<$>
              (x Core..@? "reservedInstancesId")
