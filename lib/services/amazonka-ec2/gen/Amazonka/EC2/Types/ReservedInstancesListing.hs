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
-- Module      : Amazonka.EC2.Types.ReservedInstancesListing
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ReservedInstancesListing where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceCount
import Amazonka.EC2.Types.ListingStatus
import Amazonka.EC2.Types.PriceSchedule
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a Reserved Instance listing.
--
-- /See:/ 'newReservedInstancesListing' smart constructor.
data ReservedInstancesListing = ReservedInstancesListing'
  { -- | Any tags assigned to the resource.
    tags :: Prelude.Maybe [Tag],
    -- | A unique, case-sensitive key supplied by the client to ensure that the
    -- request is idempotent. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Reserved Instance.
    reservedInstancesId :: Prelude.Maybe Prelude.Text,
    -- | The number of instances in this state.
    instanceCounts :: Prelude.Maybe [InstanceCount],
    -- | The status of the Reserved Instance listing.
    status :: Prelude.Maybe ListingStatus,
    -- | The last modified timestamp of the listing.
    updateDate :: Prelude.Maybe Core.ISO8601,
    -- | The price of the Reserved Instance listing.
    priceSchedules :: Prelude.Maybe [PriceSchedule],
    -- | The time the listing was created.
    createDate :: Prelude.Maybe Core.ISO8601,
    -- | The ID of the Reserved Instance listing.
    reservedInstancesListingId :: Prelude.Maybe Prelude.Text,
    -- | The reason for the current status of the Reserved Instance listing. The
    -- response can be blank.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservedInstancesListing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'reservedInstancesListing_tags' - Any tags assigned to the resource.
--
-- 'clientToken', 'reservedInstancesListing_clientToken' - A unique, case-sensitive key supplied by the client to ensure that the
-- request is idempotent. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'reservedInstancesId', 'reservedInstancesListing_reservedInstancesId' - The ID of the Reserved Instance.
--
-- 'instanceCounts', 'reservedInstancesListing_instanceCounts' - The number of instances in this state.
--
-- 'status', 'reservedInstancesListing_status' - The status of the Reserved Instance listing.
--
-- 'updateDate', 'reservedInstancesListing_updateDate' - The last modified timestamp of the listing.
--
-- 'priceSchedules', 'reservedInstancesListing_priceSchedules' - The price of the Reserved Instance listing.
--
-- 'createDate', 'reservedInstancesListing_createDate' - The time the listing was created.
--
-- 'reservedInstancesListingId', 'reservedInstancesListing_reservedInstancesListingId' - The ID of the Reserved Instance listing.
--
-- 'statusMessage', 'reservedInstancesListing_statusMessage' - The reason for the current status of the Reserved Instance listing. The
-- response can be blank.
newReservedInstancesListing ::
  ReservedInstancesListing
newReservedInstancesListing =
  ReservedInstancesListing'
    { tags = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      reservedInstancesId = Prelude.Nothing,
      instanceCounts = Prelude.Nothing,
      status = Prelude.Nothing,
      updateDate = Prelude.Nothing,
      priceSchedules = Prelude.Nothing,
      createDate = Prelude.Nothing,
      reservedInstancesListingId = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | Any tags assigned to the resource.
reservedInstancesListing_tags :: Lens.Lens' ReservedInstancesListing (Prelude.Maybe [Tag])
reservedInstancesListing_tags = Lens.lens (\ReservedInstancesListing' {tags} -> tags) (\s@ReservedInstancesListing' {} a -> s {tags = a} :: ReservedInstancesListing) Prelude.. Lens.mapping Lens.coerced

-- | A unique, case-sensitive key supplied by the client to ensure that the
-- request is idempotent. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
reservedInstancesListing_clientToken :: Lens.Lens' ReservedInstancesListing (Prelude.Maybe Prelude.Text)
reservedInstancesListing_clientToken = Lens.lens (\ReservedInstancesListing' {clientToken} -> clientToken) (\s@ReservedInstancesListing' {} a -> s {clientToken = a} :: ReservedInstancesListing)

-- | The ID of the Reserved Instance.
reservedInstancesListing_reservedInstancesId :: Lens.Lens' ReservedInstancesListing (Prelude.Maybe Prelude.Text)
reservedInstancesListing_reservedInstancesId = Lens.lens (\ReservedInstancesListing' {reservedInstancesId} -> reservedInstancesId) (\s@ReservedInstancesListing' {} a -> s {reservedInstancesId = a} :: ReservedInstancesListing)

-- | The number of instances in this state.
reservedInstancesListing_instanceCounts :: Lens.Lens' ReservedInstancesListing (Prelude.Maybe [InstanceCount])
reservedInstancesListing_instanceCounts = Lens.lens (\ReservedInstancesListing' {instanceCounts} -> instanceCounts) (\s@ReservedInstancesListing' {} a -> s {instanceCounts = a} :: ReservedInstancesListing) Prelude.. Lens.mapping Lens.coerced

-- | The status of the Reserved Instance listing.
reservedInstancesListing_status :: Lens.Lens' ReservedInstancesListing (Prelude.Maybe ListingStatus)
reservedInstancesListing_status = Lens.lens (\ReservedInstancesListing' {status} -> status) (\s@ReservedInstancesListing' {} a -> s {status = a} :: ReservedInstancesListing)

-- | The last modified timestamp of the listing.
reservedInstancesListing_updateDate :: Lens.Lens' ReservedInstancesListing (Prelude.Maybe Prelude.UTCTime)
reservedInstancesListing_updateDate = Lens.lens (\ReservedInstancesListing' {updateDate} -> updateDate) (\s@ReservedInstancesListing' {} a -> s {updateDate = a} :: ReservedInstancesListing) Prelude.. Lens.mapping Core._Time

-- | The price of the Reserved Instance listing.
reservedInstancesListing_priceSchedules :: Lens.Lens' ReservedInstancesListing (Prelude.Maybe [PriceSchedule])
reservedInstancesListing_priceSchedules = Lens.lens (\ReservedInstancesListing' {priceSchedules} -> priceSchedules) (\s@ReservedInstancesListing' {} a -> s {priceSchedules = a} :: ReservedInstancesListing) Prelude.. Lens.mapping Lens.coerced

-- | The time the listing was created.
reservedInstancesListing_createDate :: Lens.Lens' ReservedInstancesListing (Prelude.Maybe Prelude.UTCTime)
reservedInstancesListing_createDate = Lens.lens (\ReservedInstancesListing' {createDate} -> createDate) (\s@ReservedInstancesListing' {} a -> s {createDate = a} :: ReservedInstancesListing) Prelude.. Lens.mapping Core._Time

-- | The ID of the Reserved Instance listing.
reservedInstancesListing_reservedInstancesListingId :: Lens.Lens' ReservedInstancesListing (Prelude.Maybe Prelude.Text)
reservedInstancesListing_reservedInstancesListingId = Lens.lens (\ReservedInstancesListing' {reservedInstancesListingId} -> reservedInstancesListingId) (\s@ReservedInstancesListing' {} a -> s {reservedInstancesListingId = a} :: ReservedInstancesListing)

-- | The reason for the current status of the Reserved Instance listing. The
-- response can be blank.
reservedInstancesListing_statusMessage :: Lens.Lens' ReservedInstancesListing (Prelude.Maybe Prelude.Text)
reservedInstancesListing_statusMessage = Lens.lens (\ReservedInstancesListing' {statusMessage} -> statusMessage) (\s@ReservedInstancesListing' {} a -> s {statusMessage = a} :: ReservedInstancesListing)

instance Core.FromXML ReservedInstancesListing where
  parseXML x =
    ReservedInstancesListing'
      Prelude.<$> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "clientToken")
      Prelude.<*> (x Core..@? "reservedInstancesId")
      Prelude.<*> ( x Core..@? "instanceCounts" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "status")
      Prelude.<*> (x Core..@? "updateDate")
      Prelude.<*> ( x Core..@? "priceSchedules" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "createDate")
      Prelude.<*> (x Core..@? "reservedInstancesListingId")
      Prelude.<*> (x Core..@? "statusMessage")

instance Prelude.Hashable ReservedInstancesListing where
  hashWithSalt _salt ReservedInstancesListing' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` reservedInstancesId
      `Prelude.hashWithSalt` instanceCounts
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` updateDate
      `Prelude.hashWithSalt` priceSchedules
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` reservedInstancesListingId
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData ReservedInstancesListing where
  rnf ReservedInstancesListing' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf reservedInstancesId
      `Prelude.seq` Prelude.rnf instanceCounts
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updateDate
      `Prelude.seq` Prelude.rnf priceSchedules
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf reservedInstancesListingId
      `Prelude.seq` Prelude.rnf statusMessage
