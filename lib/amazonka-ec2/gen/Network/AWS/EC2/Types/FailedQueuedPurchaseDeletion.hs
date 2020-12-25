{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FailedQueuedPurchaseDeletion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FailedQueuedPurchaseDeletion
  ( FailedQueuedPurchaseDeletion (..),

    -- * Smart constructor
    mkFailedQueuedPurchaseDeletion,

    -- * Lenses
    fqpdError,
    fqpdReservedInstancesId,
  )
where

import qualified Network.AWS.EC2.Types.DeleteQueuedReservedInstancesError as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Reserved Instance whose queued purchase was not deleted.
--
-- /See:/ 'mkFailedQueuedPurchaseDeletion' smart constructor.
data FailedQueuedPurchaseDeletion = FailedQueuedPurchaseDeletion'
  { -- | The error.
    error :: Core.Maybe Types.DeleteQueuedReservedInstancesError,
    -- | The ID of the Reserved Instance.
    reservedInstancesId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailedQueuedPurchaseDeletion' value with any optional fields omitted.
mkFailedQueuedPurchaseDeletion ::
  FailedQueuedPurchaseDeletion
mkFailedQueuedPurchaseDeletion =
  FailedQueuedPurchaseDeletion'
    { error = Core.Nothing,
      reservedInstancesId = Core.Nothing
    }

-- | The error.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fqpdError :: Lens.Lens' FailedQueuedPurchaseDeletion (Core.Maybe Types.DeleteQueuedReservedInstancesError)
fqpdError = Lens.field @"error"
{-# DEPRECATED fqpdError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | The ID of the Reserved Instance.
--
-- /Note:/ Consider using 'reservedInstancesId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fqpdReservedInstancesId :: Lens.Lens' FailedQueuedPurchaseDeletion (Core.Maybe Types.String)
fqpdReservedInstancesId = Lens.field @"reservedInstancesId"
{-# DEPRECATED fqpdReservedInstancesId "Use generic-lens or generic-optics with 'reservedInstancesId' instead." #-}

instance Core.FromXML FailedQueuedPurchaseDeletion where
  parseXML x =
    FailedQueuedPurchaseDeletion'
      Core.<$> (x Core..@? "error") Core.<*> (x Core..@? "reservedInstancesId")
