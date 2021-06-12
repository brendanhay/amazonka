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
-- Module      : Network.AWS.DeviceFarm.Types.OfferingTransaction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.OfferingTransaction where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types.MonetaryAmount
import Network.AWS.DeviceFarm.Types.OfferingStatus
import qualified Network.AWS.Lens as Lens

-- | Represents the metadata of an offering transaction.
--
-- /See:/ 'newOfferingTransaction' smart constructor.
data OfferingTransaction = OfferingTransaction'
  { -- | The status of an offering transaction.
    offeringStatus :: Core.Maybe OfferingStatus,
    -- | The date on which an offering transaction was created.
    createdOn :: Core.Maybe Core.POSIX,
    -- | The cost of an offering transaction.
    cost :: Core.Maybe MonetaryAmount,
    -- | The transaction ID of the offering transaction.
    transactionId :: Core.Maybe Core.Text,
    -- | The ID that corresponds to a device offering promotion.
    offeringPromotionId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OfferingTransaction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'offeringStatus', 'offeringTransaction_offeringStatus' - The status of an offering transaction.
--
-- 'createdOn', 'offeringTransaction_createdOn' - The date on which an offering transaction was created.
--
-- 'cost', 'offeringTransaction_cost' - The cost of an offering transaction.
--
-- 'transactionId', 'offeringTransaction_transactionId' - The transaction ID of the offering transaction.
--
-- 'offeringPromotionId', 'offeringTransaction_offeringPromotionId' - The ID that corresponds to a device offering promotion.
newOfferingTransaction ::
  OfferingTransaction
newOfferingTransaction =
  OfferingTransaction'
    { offeringStatus = Core.Nothing,
      createdOn = Core.Nothing,
      cost = Core.Nothing,
      transactionId = Core.Nothing,
      offeringPromotionId = Core.Nothing
    }

-- | The status of an offering transaction.
offeringTransaction_offeringStatus :: Lens.Lens' OfferingTransaction (Core.Maybe OfferingStatus)
offeringTransaction_offeringStatus = Lens.lens (\OfferingTransaction' {offeringStatus} -> offeringStatus) (\s@OfferingTransaction' {} a -> s {offeringStatus = a} :: OfferingTransaction)

-- | The date on which an offering transaction was created.
offeringTransaction_createdOn :: Lens.Lens' OfferingTransaction (Core.Maybe Core.UTCTime)
offeringTransaction_createdOn = Lens.lens (\OfferingTransaction' {createdOn} -> createdOn) (\s@OfferingTransaction' {} a -> s {createdOn = a} :: OfferingTransaction) Core.. Lens.mapping Core._Time

-- | The cost of an offering transaction.
offeringTransaction_cost :: Lens.Lens' OfferingTransaction (Core.Maybe MonetaryAmount)
offeringTransaction_cost = Lens.lens (\OfferingTransaction' {cost} -> cost) (\s@OfferingTransaction' {} a -> s {cost = a} :: OfferingTransaction)

-- | The transaction ID of the offering transaction.
offeringTransaction_transactionId :: Lens.Lens' OfferingTransaction (Core.Maybe Core.Text)
offeringTransaction_transactionId = Lens.lens (\OfferingTransaction' {transactionId} -> transactionId) (\s@OfferingTransaction' {} a -> s {transactionId = a} :: OfferingTransaction)

-- | The ID that corresponds to a device offering promotion.
offeringTransaction_offeringPromotionId :: Lens.Lens' OfferingTransaction (Core.Maybe Core.Text)
offeringTransaction_offeringPromotionId = Lens.lens (\OfferingTransaction' {offeringPromotionId} -> offeringPromotionId) (\s@OfferingTransaction' {} a -> s {offeringPromotionId = a} :: OfferingTransaction)

instance Core.FromJSON OfferingTransaction where
  parseJSON =
    Core.withObject
      "OfferingTransaction"
      ( \x ->
          OfferingTransaction'
            Core.<$> (x Core..:? "offeringStatus")
            Core.<*> (x Core..:? "createdOn")
            Core.<*> (x Core..:? "cost")
            Core.<*> (x Core..:? "transactionId")
            Core.<*> (x Core..:? "offeringPromotionId")
      )

instance Core.Hashable OfferingTransaction

instance Core.NFData OfferingTransaction
