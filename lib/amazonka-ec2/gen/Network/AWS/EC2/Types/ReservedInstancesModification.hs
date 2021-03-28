{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstancesModification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ReservedInstancesModification
  ( ReservedInstancesModification (..)
  -- * Smart constructor
  , mkReservedInstancesModification
  -- * Lenses
  , rimClientToken
  , rimCreateDate
  , rimEffectiveDate
  , rimModificationResults
  , rimReservedInstancesIds
  , rimReservedInstancesModificationId
  , rimStatus
  , rimStatusMessage
  , rimUpdateDate
  ) where

import qualified Network.AWS.EC2.Types.ReservedInstancesId as Types
import qualified Network.AWS.EC2.Types.ReservedInstancesModificationResult as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Reserved Instance modification.
--
-- /See:/ 'mkReservedInstancesModification' smart constructor.
data ReservedInstancesModification = ReservedInstancesModification'
  { clientToken :: Core.Maybe Core.Text
    -- ^ A unique, case-sensitive key supplied by the client to ensure that the request is idempotent. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
  , createDate :: Core.Maybe Core.UTCTime
    -- ^ The time when the modification request was created.
  , effectiveDate :: Core.Maybe Core.UTCTime
    -- ^ The time for the modification to become effective.
  , modificationResults :: Core.Maybe [Types.ReservedInstancesModificationResult]
    -- ^ Contains target configurations along with their corresponding new Reserved Instance IDs.
  , reservedInstancesIds :: Core.Maybe [Types.ReservedInstancesId]
    -- ^ The IDs of one or more Reserved Instances.
  , reservedInstancesModificationId :: Core.Maybe Core.Text
    -- ^ A unique ID for the Reserved Instance modification.
  , status :: Core.Maybe Core.Text
    -- ^ The status of the Reserved Instances modification request.
  , statusMessage :: Core.Maybe Core.Text
    -- ^ The reason for the status.
  , updateDate :: Core.Maybe Core.UTCTime
    -- ^ The time when the modification request was last updated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ReservedInstancesModification' value with any optional fields omitted.
mkReservedInstancesModification
    :: ReservedInstancesModification
mkReservedInstancesModification
  = ReservedInstancesModification'{clientToken = Core.Nothing,
                                   createDate = Core.Nothing, effectiveDate = Core.Nothing,
                                   modificationResults = Core.Nothing,
                                   reservedInstancesIds = Core.Nothing,
                                   reservedInstancesModificationId = Core.Nothing,
                                   status = Core.Nothing, statusMessage = Core.Nothing,
                                   updateDate = Core.Nothing}

-- | A unique, case-sensitive key supplied by the client to ensure that the request is idempotent. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimClientToken :: Lens.Lens' ReservedInstancesModification (Core.Maybe Core.Text)
rimClientToken = Lens.field @"clientToken"
{-# INLINEABLE rimClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The time when the modification request was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimCreateDate :: Lens.Lens' ReservedInstancesModification (Core.Maybe Core.UTCTime)
rimCreateDate = Lens.field @"createDate"
{-# INLINEABLE rimCreateDate #-}
{-# DEPRECATED createDate "Use generic-lens or generic-optics with 'createDate' instead"  #-}

-- | The time for the modification to become effective.
--
-- /Note:/ Consider using 'effectiveDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimEffectiveDate :: Lens.Lens' ReservedInstancesModification (Core.Maybe Core.UTCTime)
rimEffectiveDate = Lens.field @"effectiveDate"
{-# INLINEABLE rimEffectiveDate #-}
{-# DEPRECATED effectiveDate "Use generic-lens or generic-optics with 'effectiveDate' instead"  #-}

-- | Contains target configurations along with their corresponding new Reserved Instance IDs.
--
-- /Note:/ Consider using 'modificationResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimModificationResults :: Lens.Lens' ReservedInstancesModification (Core.Maybe [Types.ReservedInstancesModificationResult])
rimModificationResults = Lens.field @"modificationResults"
{-# INLINEABLE rimModificationResults #-}
{-# DEPRECATED modificationResults "Use generic-lens or generic-optics with 'modificationResults' instead"  #-}

-- | The IDs of one or more Reserved Instances.
--
-- /Note:/ Consider using 'reservedInstancesIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimReservedInstancesIds :: Lens.Lens' ReservedInstancesModification (Core.Maybe [Types.ReservedInstancesId])
rimReservedInstancesIds = Lens.field @"reservedInstancesIds"
{-# INLINEABLE rimReservedInstancesIds #-}
{-# DEPRECATED reservedInstancesIds "Use generic-lens or generic-optics with 'reservedInstancesIds' instead"  #-}

-- | A unique ID for the Reserved Instance modification.
--
-- /Note:/ Consider using 'reservedInstancesModificationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimReservedInstancesModificationId :: Lens.Lens' ReservedInstancesModification (Core.Maybe Core.Text)
rimReservedInstancesModificationId = Lens.field @"reservedInstancesModificationId"
{-# INLINEABLE rimReservedInstancesModificationId #-}
{-# DEPRECATED reservedInstancesModificationId "Use generic-lens or generic-optics with 'reservedInstancesModificationId' instead"  #-}

-- | The status of the Reserved Instances modification request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimStatus :: Lens.Lens' ReservedInstancesModification (Core.Maybe Core.Text)
rimStatus = Lens.field @"status"
{-# INLINEABLE rimStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The reason for the status.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimStatusMessage :: Lens.Lens' ReservedInstancesModification (Core.Maybe Core.Text)
rimStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE rimStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | The time when the modification request was last updated.
--
-- /Note:/ Consider using 'updateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimUpdateDate :: Lens.Lens' ReservedInstancesModification (Core.Maybe Core.UTCTime)
rimUpdateDate = Lens.field @"updateDate"
{-# INLINEABLE rimUpdateDate #-}
{-# DEPRECATED updateDate "Use generic-lens or generic-optics with 'updateDate' instead"  #-}

instance Core.FromXML ReservedInstancesModification where
        parseXML x
          = ReservedInstancesModification' Core.<$>
              (x Core..@? "clientToken") Core.<*> x Core..@? "createDate"
                Core.<*> x Core..@? "effectiveDate"
                Core.<*>
                x Core..@? "modificationResultSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*>
                x Core..@? "reservedInstancesSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*> x Core..@? "reservedInstancesModificationId"
                Core.<*> x Core..@? "status"
                Core.<*> x Core..@? "statusMessage"
                Core.<*> x Core..@? "updateDate"
