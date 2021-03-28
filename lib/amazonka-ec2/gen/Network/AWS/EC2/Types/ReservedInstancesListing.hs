{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstancesListing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ReservedInstancesListing
  ( ReservedInstancesListing (..)
  -- * Smart constructor
  , mkReservedInstancesListing
  -- * Lenses
  , rilClientToken
  , rilCreateDate
  , rilInstanceCounts
  , rilPriceSchedules
  , rilReservedInstancesId
  , rilReservedInstancesListingId
  , rilStatus
  , rilStatusMessage
  , rilTags
  , rilUpdateDate
  ) where

import qualified Network.AWS.EC2.Types.InstanceCount as Types
import qualified Network.AWS.EC2.Types.ListingStatus as Types
import qualified Network.AWS.EC2.Types.PriceSchedule as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Reserved Instance listing.
--
-- /See:/ 'mkReservedInstancesListing' smart constructor.
data ReservedInstancesListing = ReservedInstancesListing'
  { clientToken :: Core.Maybe Core.Text
    -- ^ A unique, case-sensitive key supplied by the client to ensure that the request is idempotent. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
  , createDate :: Core.Maybe Core.UTCTime
    -- ^ The time the listing was created.
  , instanceCounts :: Core.Maybe [Types.InstanceCount]
    -- ^ The number of instances in this state.
  , priceSchedules :: Core.Maybe [Types.PriceSchedule]
    -- ^ The price of the Reserved Instance listing.
  , reservedInstancesId :: Core.Maybe Core.Text
    -- ^ The ID of the Reserved Instance.
  , reservedInstancesListingId :: Core.Maybe Core.Text
    -- ^ The ID of the Reserved Instance listing.
  , status :: Core.Maybe Types.ListingStatus
    -- ^ The status of the Reserved Instance listing.
  , statusMessage :: Core.Maybe Core.Text
    -- ^ The reason for the current status of the Reserved Instance listing. The response can be blank.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the resource.
  , updateDate :: Core.Maybe Core.UTCTime
    -- ^ The last modified timestamp of the listing.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ReservedInstancesListing' value with any optional fields omitted.
mkReservedInstancesListing
    :: ReservedInstancesListing
mkReservedInstancesListing
  = ReservedInstancesListing'{clientToken = Core.Nothing,
                              createDate = Core.Nothing, instanceCounts = Core.Nothing,
                              priceSchedules = Core.Nothing, reservedInstancesId = Core.Nothing,
                              reservedInstancesListingId = Core.Nothing, status = Core.Nothing,
                              statusMessage = Core.Nothing, tags = Core.Nothing,
                              updateDate = Core.Nothing}

-- | A unique, case-sensitive key supplied by the client to ensure that the request is idempotent. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilClientToken :: Lens.Lens' ReservedInstancesListing (Core.Maybe Core.Text)
rilClientToken = Lens.field @"clientToken"
{-# INLINEABLE rilClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The time the listing was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilCreateDate :: Lens.Lens' ReservedInstancesListing (Core.Maybe Core.UTCTime)
rilCreateDate = Lens.field @"createDate"
{-# INLINEABLE rilCreateDate #-}
{-# DEPRECATED createDate "Use generic-lens or generic-optics with 'createDate' instead"  #-}

-- | The number of instances in this state.
--
-- /Note:/ Consider using 'instanceCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilInstanceCounts :: Lens.Lens' ReservedInstancesListing (Core.Maybe [Types.InstanceCount])
rilInstanceCounts = Lens.field @"instanceCounts"
{-# INLINEABLE rilInstanceCounts #-}
{-# DEPRECATED instanceCounts "Use generic-lens or generic-optics with 'instanceCounts' instead"  #-}

-- | The price of the Reserved Instance listing.
--
-- /Note:/ Consider using 'priceSchedules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilPriceSchedules :: Lens.Lens' ReservedInstancesListing (Core.Maybe [Types.PriceSchedule])
rilPriceSchedules = Lens.field @"priceSchedules"
{-# INLINEABLE rilPriceSchedules #-}
{-# DEPRECATED priceSchedules "Use generic-lens or generic-optics with 'priceSchedules' instead"  #-}

-- | The ID of the Reserved Instance.
--
-- /Note:/ Consider using 'reservedInstancesId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilReservedInstancesId :: Lens.Lens' ReservedInstancesListing (Core.Maybe Core.Text)
rilReservedInstancesId = Lens.field @"reservedInstancesId"
{-# INLINEABLE rilReservedInstancesId #-}
{-# DEPRECATED reservedInstancesId "Use generic-lens or generic-optics with 'reservedInstancesId' instead"  #-}

-- | The ID of the Reserved Instance listing.
--
-- /Note:/ Consider using 'reservedInstancesListingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilReservedInstancesListingId :: Lens.Lens' ReservedInstancesListing (Core.Maybe Core.Text)
rilReservedInstancesListingId = Lens.field @"reservedInstancesListingId"
{-# INLINEABLE rilReservedInstancesListingId #-}
{-# DEPRECATED reservedInstancesListingId "Use generic-lens or generic-optics with 'reservedInstancesListingId' instead"  #-}

-- | The status of the Reserved Instance listing.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilStatus :: Lens.Lens' ReservedInstancesListing (Core.Maybe Types.ListingStatus)
rilStatus = Lens.field @"status"
{-# INLINEABLE rilStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The reason for the current status of the Reserved Instance listing. The response can be blank.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilStatusMessage :: Lens.Lens' ReservedInstancesListing (Core.Maybe Core.Text)
rilStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE rilStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | Any tags assigned to the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilTags :: Lens.Lens' ReservedInstancesListing (Core.Maybe [Types.Tag])
rilTags = Lens.field @"tags"
{-# INLINEABLE rilTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The last modified timestamp of the listing.
--
-- /Note:/ Consider using 'updateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilUpdateDate :: Lens.Lens' ReservedInstancesListing (Core.Maybe Core.UTCTime)
rilUpdateDate = Lens.field @"updateDate"
{-# INLINEABLE rilUpdateDate #-}
{-# DEPRECATED updateDate "Use generic-lens or generic-optics with 'updateDate' instead"  #-}

instance Core.FromXML ReservedInstancesListing where
        parseXML x
          = ReservedInstancesListing' Core.<$>
              (x Core..@? "clientToken") Core.<*> x Core..@? "createDate"
                Core.<*>
                x Core..@? "instanceCounts" Core..<@> Core.parseXMLList "item"
                Core.<*>
                x Core..@? "priceSchedules" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "reservedInstancesId"
                Core.<*> x Core..@? "reservedInstancesListingId"
                Core.<*> x Core..@? "status"
                Core.<*> x Core..@? "statusMessage"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "updateDate"
