{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstancesModification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstancesModification
  ( ReservedInstancesModification (..),

    -- * Smart constructor
    mkReservedInstancesModification,

    -- * Lenses
    rimClientToken,
    rimCreateDate,
    rimEffectiveDate,
    rimModificationResults,
    rimReservedInstancesIds,
    rimReservedInstancesModificationId,
    rimStatus,
    rimStatusMessage,
    rimUpdateDate,
  )
where

import qualified Network.AWS.EC2.Types.ReservedInstancesId as Types
import qualified Network.AWS.EC2.Types.ReservedInstancesModificationResult as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Reserved Instance modification.
--
-- /See:/ 'mkReservedInstancesModification' smart constructor.
data ReservedInstancesModification = ReservedInstancesModification'
  { -- | A unique, case-sensitive key supplied by the client to ensure that the request is idempotent. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
    clientToken :: Core.Maybe Types.String,
    -- | The time when the modification request was created.
    createDate :: Core.Maybe Core.UTCTime,
    -- | The time for the modification to become effective.
    effectiveDate :: Core.Maybe Core.UTCTime,
    -- | Contains target configurations along with their corresponding new Reserved Instance IDs.
    modificationResults :: Core.Maybe [Types.ReservedInstancesModificationResult],
    -- | The IDs of one or more Reserved Instances.
    reservedInstancesIds :: Core.Maybe [Types.ReservedInstancesId],
    -- | A unique ID for the Reserved Instance modification.
    reservedInstancesModificationId :: Core.Maybe Types.String,
    -- | The status of the Reserved Instances modification request.
    status :: Core.Maybe Types.String,
    -- | The reason for the status.
    statusMessage :: Core.Maybe Types.String,
    -- | The time when the modification request was last updated.
    updateDate :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ReservedInstancesModification' value with any optional fields omitted.
mkReservedInstancesModification ::
  ReservedInstancesModification
mkReservedInstancesModification =
  ReservedInstancesModification'
    { clientToken = Core.Nothing,
      createDate = Core.Nothing,
      effectiveDate = Core.Nothing,
      modificationResults = Core.Nothing,
      reservedInstancesIds = Core.Nothing,
      reservedInstancesModificationId = Core.Nothing,
      status = Core.Nothing,
      statusMessage = Core.Nothing,
      updateDate = Core.Nothing
    }

-- | A unique, case-sensitive key supplied by the client to ensure that the request is idempotent. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimClientToken :: Lens.Lens' ReservedInstancesModification (Core.Maybe Types.String)
rimClientToken = Lens.field @"clientToken"
{-# DEPRECATED rimClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The time when the modification request was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimCreateDate :: Lens.Lens' ReservedInstancesModification (Core.Maybe Core.UTCTime)
rimCreateDate = Lens.field @"createDate"
{-# DEPRECATED rimCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The time for the modification to become effective.
--
-- /Note:/ Consider using 'effectiveDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimEffectiveDate :: Lens.Lens' ReservedInstancesModification (Core.Maybe Core.UTCTime)
rimEffectiveDate = Lens.field @"effectiveDate"
{-# DEPRECATED rimEffectiveDate "Use generic-lens or generic-optics with 'effectiveDate' instead." #-}

-- | Contains target configurations along with their corresponding new Reserved Instance IDs.
--
-- /Note:/ Consider using 'modificationResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimModificationResults :: Lens.Lens' ReservedInstancesModification (Core.Maybe [Types.ReservedInstancesModificationResult])
rimModificationResults = Lens.field @"modificationResults"
{-# DEPRECATED rimModificationResults "Use generic-lens or generic-optics with 'modificationResults' instead." #-}

-- | The IDs of one or more Reserved Instances.
--
-- /Note:/ Consider using 'reservedInstancesIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimReservedInstancesIds :: Lens.Lens' ReservedInstancesModification (Core.Maybe [Types.ReservedInstancesId])
rimReservedInstancesIds = Lens.field @"reservedInstancesIds"
{-# DEPRECATED rimReservedInstancesIds "Use generic-lens or generic-optics with 'reservedInstancesIds' instead." #-}

-- | A unique ID for the Reserved Instance modification.
--
-- /Note:/ Consider using 'reservedInstancesModificationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimReservedInstancesModificationId :: Lens.Lens' ReservedInstancesModification (Core.Maybe Types.String)
rimReservedInstancesModificationId = Lens.field @"reservedInstancesModificationId"
{-# DEPRECATED rimReservedInstancesModificationId "Use generic-lens or generic-optics with 'reservedInstancesModificationId' instead." #-}

-- | The status of the Reserved Instances modification request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimStatus :: Lens.Lens' ReservedInstancesModification (Core.Maybe Types.String)
rimStatus = Lens.field @"status"
{-# DEPRECATED rimStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The reason for the status.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimStatusMessage :: Lens.Lens' ReservedInstancesModification (Core.Maybe Types.String)
rimStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED rimStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The time when the modification request was last updated.
--
-- /Note:/ Consider using 'updateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimUpdateDate :: Lens.Lens' ReservedInstancesModification (Core.Maybe Core.UTCTime)
rimUpdateDate = Lens.field @"updateDate"
{-# DEPRECATED rimUpdateDate "Use generic-lens or generic-optics with 'updateDate' instead." #-}

instance Core.FromXML ReservedInstancesModification where
  parseXML x =
    ReservedInstancesModification'
      Core.<$> (x Core..@? "clientToken")
      Core.<*> (x Core..@? "createDate")
      Core.<*> (x Core..@? "effectiveDate")
      Core.<*> ( x Core..@? "modificationResultSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> ( x Core..@? "reservedInstancesSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "reservedInstancesModificationId")
      Core.<*> (x Core..@? "status")
      Core.<*> (x Core..@? "statusMessage")
      Core.<*> (x Core..@? "updateDate")
