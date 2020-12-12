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
    rilStatus,
    rilClientToken,
    rilUpdateDate,
    rilCreateDate,
    rilPriceSchedules,
    rilStatusMessage,
    rilReservedInstancesId,
    rilTags,
    rilInstanceCounts,
    rilReservedInstancesListingId,
  )
where

import Network.AWS.EC2.Types.InstanceCount
import Network.AWS.EC2.Types.ListingStatus
import Network.AWS.EC2.Types.PriceSchedule
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a Reserved Instance listing.
--
-- /See:/ 'mkReservedInstancesListing' smart constructor.
data ReservedInstancesListing = ReservedInstancesListing'
  { status ::
      Lude.Maybe ListingStatus,
    clientToken :: Lude.Maybe Lude.Text,
    updateDate :: Lude.Maybe Lude.DateTime,
    createDate :: Lude.Maybe Lude.DateTime,
    priceSchedules ::
      Lude.Maybe [PriceSchedule],
    statusMessage :: Lude.Maybe Lude.Text,
    reservedInstancesId ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    instanceCounts ::
      Lude.Maybe [InstanceCount],
    reservedInstancesListingId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservedInstancesListing' with the minimum fields required to make a request.
--
-- * 'clientToken' - A unique, case-sensitive key supplied by the client to ensure that the request is idempotent. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
-- * 'createDate' - The time the listing was created.
-- * 'instanceCounts' - The number of instances in this state.
-- * 'priceSchedules' - The price of the Reserved Instance listing.
-- * 'reservedInstancesId' - The ID of the Reserved Instance.
-- * 'reservedInstancesListingId' - The ID of the Reserved Instance listing.
-- * 'status' - The status of the Reserved Instance listing.
-- * 'statusMessage' - The reason for the current status of the Reserved Instance listing. The response can be blank.
-- * 'tags' - Any tags assigned to the resource.
-- * 'updateDate' - The last modified timestamp of the listing.
mkReservedInstancesListing ::
  ReservedInstancesListing
mkReservedInstancesListing =
  ReservedInstancesListing'
    { status = Lude.Nothing,
      clientToken = Lude.Nothing,
      updateDate = Lude.Nothing,
      createDate = Lude.Nothing,
      priceSchedules = Lude.Nothing,
      statusMessage = Lude.Nothing,
      reservedInstancesId = Lude.Nothing,
      tags = Lude.Nothing,
      instanceCounts = Lude.Nothing,
      reservedInstancesListingId = Lude.Nothing
    }

-- | The status of the Reserved Instance listing.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilStatus :: Lens.Lens' ReservedInstancesListing (Lude.Maybe ListingStatus)
rilStatus = Lens.lens (status :: ReservedInstancesListing -> Lude.Maybe ListingStatus) (\s a -> s {status = a} :: ReservedInstancesListing)
{-# DEPRECATED rilStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A unique, case-sensitive key supplied by the client to ensure that the request is idempotent. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilClientToken :: Lens.Lens' ReservedInstancesListing (Lude.Maybe Lude.Text)
rilClientToken = Lens.lens (clientToken :: ReservedInstancesListing -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: ReservedInstancesListing)
{-# DEPRECATED rilClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The last modified timestamp of the listing.
--
-- /Note:/ Consider using 'updateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilUpdateDate :: Lens.Lens' ReservedInstancesListing (Lude.Maybe Lude.DateTime)
rilUpdateDate = Lens.lens (updateDate :: ReservedInstancesListing -> Lude.Maybe Lude.DateTime) (\s a -> s {updateDate = a} :: ReservedInstancesListing)
{-# DEPRECATED rilUpdateDate "Use generic-lens or generic-optics with 'updateDate' instead." #-}

-- | The time the listing was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilCreateDate :: Lens.Lens' ReservedInstancesListing (Lude.Maybe Lude.DateTime)
rilCreateDate = Lens.lens (createDate :: ReservedInstancesListing -> Lude.Maybe Lude.DateTime) (\s a -> s {createDate = a} :: ReservedInstancesListing)
{-# DEPRECATED rilCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The price of the Reserved Instance listing.
--
-- /Note:/ Consider using 'priceSchedules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilPriceSchedules :: Lens.Lens' ReservedInstancesListing (Lude.Maybe [PriceSchedule])
rilPriceSchedules = Lens.lens (priceSchedules :: ReservedInstancesListing -> Lude.Maybe [PriceSchedule]) (\s a -> s {priceSchedules = a} :: ReservedInstancesListing)
{-# DEPRECATED rilPriceSchedules "Use generic-lens or generic-optics with 'priceSchedules' instead." #-}

-- | The reason for the current status of the Reserved Instance listing. The response can be blank.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilStatusMessage :: Lens.Lens' ReservedInstancesListing (Lude.Maybe Lude.Text)
rilStatusMessage = Lens.lens (statusMessage :: ReservedInstancesListing -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: ReservedInstancesListing)
{-# DEPRECATED rilStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The ID of the Reserved Instance.
--
-- /Note:/ Consider using 'reservedInstancesId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilReservedInstancesId :: Lens.Lens' ReservedInstancesListing (Lude.Maybe Lude.Text)
rilReservedInstancesId = Lens.lens (reservedInstancesId :: ReservedInstancesListing -> Lude.Maybe Lude.Text) (\s a -> s {reservedInstancesId = a} :: ReservedInstancesListing)
{-# DEPRECATED rilReservedInstancesId "Use generic-lens or generic-optics with 'reservedInstancesId' instead." #-}

-- | Any tags assigned to the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilTags :: Lens.Lens' ReservedInstancesListing (Lude.Maybe [Tag])
rilTags = Lens.lens (tags :: ReservedInstancesListing -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ReservedInstancesListing)
{-# DEPRECATED rilTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The number of instances in this state.
--
-- /Note:/ Consider using 'instanceCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilInstanceCounts :: Lens.Lens' ReservedInstancesListing (Lude.Maybe [InstanceCount])
rilInstanceCounts = Lens.lens (instanceCounts :: ReservedInstancesListing -> Lude.Maybe [InstanceCount]) (\s a -> s {instanceCounts = a} :: ReservedInstancesListing)
{-# DEPRECATED rilInstanceCounts "Use generic-lens or generic-optics with 'instanceCounts' instead." #-}

-- | The ID of the Reserved Instance listing.
--
-- /Note:/ Consider using 'reservedInstancesListingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilReservedInstancesListingId :: Lens.Lens' ReservedInstancesListing (Lude.Maybe Lude.Text)
rilReservedInstancesListingId = Lens.lens (reservedInstancesListingId :: ReservedInstancesListing -> Lude.Maybe Lude.Text) (\s a -> s {reservedInstancesListingId = a} :: ReservedInstancesListing)
{-# DEPRECATED rilReservedInstancesListingId "Use generic-lens or generic-optics with 'reservedInstancesListingId' instead." #-}

instance Lude.FromXML ReservedInstancesListing where
  parseXML x =
    ReservedInstancesListing'
      Lude.<$> (x Lude..@? "status")
      Lude.<*> (x Lude..@? "clientToken")
      Lude.<*> (x Lude..@? "updateDate")
      Lude.<*> (x Lude..@? "createDate")
      Lude.<*> ( x Lude..@? "priceSchedules" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "statusMessage")
      Lude.<*> (x Lude..@? "reservedInstancesId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "instanceCounts" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "reservedInstancesListingId")
