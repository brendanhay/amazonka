{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstancesListing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstancesListing
  ( ReservedInstancesListing (..),

    -- * Smart constructor
    mkReservedInstancesListing,

    -- * Lenses
    rilClientToken,
    rilCreateDate,
    rilInstanceCounts,
    rilPriceSchedules,
    rilReservedInstancesId,
    rilReservedInstancesListingId,
    rilStatus,
    rilStatusMessage,
    rilTags,
    rilUpdateDate,
  )
where

import qualified Network.AWS.EC2.Types.ClientToken as Types
import qualified Network.AWS.EC2.Types.InstanceCount as Types
import qualified Network.AWS.EC2.Types.ListingStatus as Types
import qualified Network.AWS.EC2.Types.PriceSchedule as Types
import qualified Network.AWS.EC2.Types.ReservedInstancesListingId as Types
import qualified Network.AWS.EC2.Types.StatusMessage as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Reserved Instance listing.
--
-- /See:/ 'mkReservedInstancesListing' smart constructor.
data ReservedInstancesListing = ReservedInstancesListing'
  { -- | A unique, case-sensitive key supplied by the client to ensure that the request is idempotent. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
    clientToken :: Core.Maybe Types.ClientToken,
    -- | The time the listing was created.
    createDate :: Core.Maybe Core.UTCTime,
    -- | The number of instances in this state.
    instanceCounts :: Core.Maybe [Types.InstanceCount],
    -- | The price of the Reserved Instance listing.
    priceSchedules :: Core.Maybe [Types.PriceSchedule],
    -- | The ID of the Reserved Instance.
    reservedInstancesId :: Core.Maybe Types.String,
    -- | The ID of the Reserved Instance listing.
    reservedInstancesListingId :: Core.Maybe Types.ReservedInstancesListingId,
    -- | The status of the Reserved Instance listing.
    status :: Core.Maybe Types.ListingStatus,
    -- | The reason for the current status of the Reserved Instance listing. The response can be blank.
    statusMessage :: Core.Maybe Types.StatusMessage,
    -- | Any tags assigned to the resource.
    tags :: Core.Maybe [Types.Tag],
    -- | The last modified timestamp of the listing.
    updateDate :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ReservedInstancesListing' value with any optional fields omitted.
mkReservedInstancesListing ::
  ReservedInstancesListing
mkReservedInstancesListing =
  ReservedInstancesListing'
    { clientToken = Core.Nothing,
      createDate = Core.Nothing,
      instanceCounts = Core.Nothing,
      priceSchedules = Core.Nothing,
      reservedInstancesId = Core.Nothing,
      reservedInstancesListingId = Core.Nothing,
      status = Core.Nothing,
      statusMessage = Core.Nothing,
      tags = Core.Nothing,
      updateDate = Core.Nothing
    }

-- | A unique, case-sensitive key supplied by the client to ensure that the request is idempotent. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilClientToken :: Lens.Lens' ReservedInstancesListing (Core.Maybe Types.ClientToken)
rilClientToken = Lens.field @"clientToken"
{-# DEPRECATED rilClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The time the listing was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilCreateDate :: Lens.Lens' ReservedInstancesListing (Core.Maybe Core.UTCTime)
rilCreateDate = Lens.field @"createDate"
{-# DEPRECATED rilCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The number of instances in this state.
--
-- /Note:/ Consider using 'instanceCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilInstanceCounts :: Lens.Lens' ReservedInstancesListing (Core.Maybe [Types.InstanceCount])
rilInstanceCounts = Lens.field @"instanceCounts"
{-# DEPRECATED rilInstanceCounts "Use generic-lens or generic-optics with 'instanceCounts' instead." #-}

-- | The price of the Reserved Instance listing.
--
-- /Note:/ Consider using 'priceSchedules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilPriceSchedules :: Lens.Lens' ReservedInstancesListing (Core.Maybe [Types.PriceSchedule])
rilPriceSchedules = Lens.field @"priceSchedules"
{-# DEPRECATED rilPriceSchedules "Use generic-lens or generic-optics with 'priceSchedules' instead." #-}

-- | The ID of the Reserved Instance.
--
-- /Note:/ Consider using 'reservedInstancesId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilReservedInstancesId :: Lens.Lens' ReservedInstancesListing (Core.Maybe Types.String)
rilReservedInstancesId = Lens.field @"reservedInstancesId"
{-# DEPRECATED rilReservedInstancesId "Use generic-lens or generic-optics with 'reservedInstancesId' instead." #-}

-- | The ID of the Reserved Instance listing.
--
-- /Note:/ Consider using 'reservedInstancesListingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilReservedInstancesListingId :: Lens.Lens' ReservedInstancesListing (Core.Maybe Types.ReservedInstancesListingId)
rilReservedInstancesListingId = Lens.field @"reservedInstancesListingId"
{-# DEPRECATED rilReservedInstancesListingId "Use generic-lens or generic-optics with 'reservedInstancesListingId' instead." #-}

-- | The status of the Reserved Instance listing.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilStatus :: Lens.Lens' ReservedInstancesListing (Core.Maybe Types.ListingStatus)
rilStatus = Lens.field @"status"
{-# DEPRECATED rilStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The reason for the current status of the Reserved Instance listing. The response can be blank.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilStatusMessage :: Lens.Lens' ReservedInstancesListing (Core.Maybe Types.StatusMessage)
rilStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED rilStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | Any tags assigned to the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilTags :: Lens.Lens' ReservedInstancesListing (Core.Maybe [Types.Tag])
rilTags = Lens.field @"tags"
{-# DEPRECATED rilTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The last modified timestamp of the listing.
--
-- /Note:/ Consider using 'updateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilUpdateDate :: Lens.Lens' ReservedInstancesListing (Core.Maybe Core.UTCTime)
rilUpdateDate = Lens.field @"updateDate"
{-# DEPRECATED rilUpdateDate "Use generic-lens or generic-optics with 'updateDate' instead." #-}

instance Core.FromXML ReservedInstancesListing where
  parseXML x =
    ReservedInstancesListing'
      Core.<$> (x Core..@? "clientToken")
      Core.<*> (x Core..@? "createDate")
      Core.<*> (x Core..@? "instanceCounts" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "priceSchedules" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "reservedInstancesId")
      Core.<*> (x Core..@? "reservedInstancesListingId")
      Core.<*> (x Core..@? "status")
      Core.<*> (x Core..@? "statusMessage")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "updateDate")
