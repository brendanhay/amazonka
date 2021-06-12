{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstancesListing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstancesListing where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceCount
import Network.AWS.EC2.Types.ListingStatus
import Network.AWS.EC2.Types.PriceSchedule
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes a Reserved Instance listing.
--
-- /See:/ 'newReservedInstancesListing' smart constructor.
data ReservedInstancesListing = ReservedInstancesListing'
  { -- | The reason for the current status of the Reserved Instance listing. The
    -- response can be blank.
    statusMessage :: Core.Maybe Core.Text,
    -- | The status of the Reserved Instance listing.
    status :: Core.Maybe ListingStatus,
    -- | The price of the Reserved Instance listing.
    priceSchedules :: Core.Maybe [PriceSchedule],
    -- | The ID of the Reserved Instance.
    reservedInstancesId :: Core.Maybe Core.Text,
    -- | The time the listing was created.
    createDate :: Core.Maybe Core.ISO8601,
    -- | Any tags assigned to the resource.
    tags :: Core.Maybe [Tag],
    -- | The number of instances in this state.
    instanceCounts :: Core.Maybe [InstanceCount],
    -- | The ID of the Reserved Instance listing.
    reservedInstancesListingId :: Core.Maybe Core.Text,
    -- | The last modified timestamp of the listing.
    updateDate :: Core.Maybe Core.ISO8601,
    -- | A unique, case-sensitive key supplied by the client to ensure that the
    -- request is idempotent. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReservedInstancesListing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'reservedInstancesListing_statusMessage' - The reason for the current status of the Reserved Instance listing. The
-- response can be blank.
--
-- 'status', 'reservedInstancesListing_status' - The status of the Reserved Instance listing.
--
-- 'priceSchedules', 'reservedInstancesListing_priceSchedules' - The price of the Reserved Instance listing.
--
-- 'reservedInstancesId', 'reservedInstancesListing_reservedInstancesId' - The ID of the Reserved Instance.
--
-- 'createDate', 'reservedInstancesListing_createDate' - The time the listing was created.
--
-- 'tags', 'reservedInstancesListing_tags' - Any tags assigned to the resource.
--
-- 'instanceCounts', 'reservedInstancesListing_instanceCounts' - The number of instances in this state.
--
-- 'reservedInstancesListingId', 'reservedInstancesListing_reservedInstancesListingId' - The ID of the Reserved Instance listing.
--
-- 'updateDate', 'reservedInstancesListing_updateDate' - The last modified timestamp of the listing.
--
-- 'clientToken', 'reservedInstancesListing_clientToken' - A unique, case-sensitive key supplied by the client to ensure that the
-- request is idempotent. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
newReservedInstancesListing ::
  ReservedInstancesListing
newReservedInstancesListing =
  ReservedInstancesListing'
    { statusMessage =
        Core.Nothing,
      status = Core.Nothing,
      priceSchedules = Core.Nothing,
      reservedInstancesId = Core.Nothing,
      createDate = Core.Nothing,
      tags = Core.Nothing,
      instanceCounts = Core.Nothing,
      reservedInstancesListingId = Core.Nothing,
      updateDate = Core.Nothing,
      clientToken = Core.Nothing
    }

-- | The reason for the current status of the Reserved Instance listing. The
-- response can be blank.
reservedInstancesListing_statusMessage :: Lens.Lens' ReservedInstancesListing (Core.Maybe Core.Text)
reservedInstancesListing_statusMessage = Lens.lens (\ReservedInstancesListing' {statusMessage} -> statusMessage) (\s@ReservedInstancesListing' {} a -> s {statusMessage = a} :: ReservedInstancesListing)

-- | The status of the Reserved Instance listing.
reservedInstancesListing_status :: Lens.Lens' ReservedInstancesListing (Core.Maybe ListingStatus)
reservedInstancesListing_status = Lens.lens (\ReservedInstancesListing' {status} -> status) (\s@ReservedInstancesListing' {} a -> s {status = a} :: ReservedInstancesListing)

-- | The price of the Reserved Instance listing.
reservedInstancesListing_priceSchedules :: Lens.Lens' ReservedInstancesListing (Core.Maybe [PriceSchedule])
reservedInstancesListing_priceSchedules = Lens.lens (\ReservedInstancesListing' {priceSchedules} -> priceSchedules) (\s@ReservedInstancesListing' {} a -> s {priceSchedules = a} :: ReservedInstancesListing) Core.. Lens.mapping Lens._Coerce

-- | The ID of the Reserved Instance.
reservedInstancesListing_reservedInstancesId :: Lens.Lens' ReservedInstancesListing (Core.Maybe Core.Text)
reservedInstancesListing_reservedInstancesId = Lens.lens (\ReservedInstancesListing' {reservedInstancesId} -> reservedInstancesId) (\s@ReservedInstancesListing' {} a -> s {reservedInstancesId = a} :: ReservedInstancesListing)

-- | The time the listing was created.
reservedInstancesListing_createDate :: Lens.Lens' ReservedInstancesListing (Core.Maybe Core.UTCTime)
reservedInstancesListing_createDate = Lens.lens (\ReservedInstancesListing' {createDate} -> createDate) (\s@ReservedInstancesListing' {} a -> s {createDate = a} :: ReservedInstancesListing) Core.. Lens.mapping Core._Time

-- | Any tags assigned to the resource.
reservedInstancesListing_tags :: Lens.Lens' ReservedInstancesListing (Core.Maybe [Tag])
reservedInstancesListing_tags = Lens.lens (\ReservedInstancesListing' {tags} -> tags) (\s@ReservedInstancesListing' {} a -> s {tags = a} :: ReservedInstancesListing) Core.. Lens.mapping Lens._Coerce

-- | The number of instances in this state.
reservedInstancesListing_instanceCounts :: Lens.Lens' ReservedInstancesListing (Core.Maybe [InstanceCount])
reservedInstancesListing_instanceCounts = Lens.lens (\ReservedInstancesListing' {instanceCounts} -> instanceCounts) (\s@ReservedInstancesListing' {} a -> s {instanceCounts = a} :: ReservedInstancesListing) Core.. Lens.mapping Lens._Coerce

-- | The ID of the Reserved Instance listing.
reservedInstancesListing_reservedInstancesListingId :: Lens.Lens' ReservedInstancesListing (Core.Maybe Core.Text)
reservedInstancesListing_reservedInstancesListingId = Lens.lens (\ReservedInstancesListing' {reservedInstancesListingId} -> reservedInstancesListingId) (\s@ReservedInstancesListing' {} a -> s {reservedInstancesListingId = a} :: ReservedInstancesListing)

-- | The last modified timestamp of the listing.
reservedInstancesListing_updateDate :: Lens.Lens' ReservedInstancesListing (Core.Maybe Core.UTCTime)
reservedInstancesListing_updateDate = Lens.lens (\ReservedInstancesListing' {updateDate} -> updateDate) (\s@ReservedInstancesListing' {} a -> s {updateDate = a} :: ReservedInstancesListing) Core.. Lens.mapping Core._Time

-- | A unique, case-sensitive key supplied by the client to ensure that the
-- request is idempotent. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
reservedInstancesListing_clientToken :: Lens.Lens' ReservedInstancesListing (Core.Maybe Core.Text)
reservedInstancesListing_clientToken = Lens.lens (\ReservedInstancesListing' {clientToken} -> clientToken) (\s@ReservedInstancesListing' {} a -> s {clientToken = a} :: ReservedInstancesListing)

instance Core.FromXML ReservedInstancesListing where
  parseXML x =
    ReservedInstancesListing'
      Core.<$> (x Core..@? "statusMessage")
      Core.<*> (x Core..@? "status")
      Core.<*> ( x Core..@? "priceSchedules" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "reservedInstancesId")
      Core.<*> (x Core..@? "createDate")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "instanceCounts" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "reservedInstancesListingId")
      Core.<*> (x Core..@? "updateDate")
      Core.<*> (x Core..@? "clientToken")

instance Core.Hashable ReservedInstancesListing

instance Core.NFData ReservedInstancesListing
