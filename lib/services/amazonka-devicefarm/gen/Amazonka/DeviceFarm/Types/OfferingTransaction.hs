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
-- Module      : Amazonka.DeviceFarm.Types.OfferingTransaction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.OfferingTransaction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DeviceFarm.Types.MonetaryAmount
import Amazonka.DeviceFarm.Types.OfferingStatus
import qualified Amazonka.Prelude as Prelude

-- | Represents the metadata of an offering transaction.
--
-- /See:/ 'newOfferingTransaction' smart constructor.
data OfferingTransaction = OfferingTransaction'
  { -- | The date on which an offering transaction was created.
    createdOn :: Prelude.Maybe Core.POSIX,
    -- | The ID that corresponds to a device offering promotion.
    offeringPromotionId :: Prelude.Maybe Prelude.Text,
    -- | The transaction ID of the offering transaction.
    transactionId :: Prelude.Maybe Prelude.Text,
    -- | The status of an offering transaction.
    offeringStatus :: Prelude.Maybe OfferingStatus,
    -- | The cost of an offering transaction.
    cost :: Prelude.Maybe MonetaryAmount
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OfferingTransaction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdOn', 'offeringTransaction_createdOn' - The date on which an offering transaction was created.
--
-- 'offeringPromotionId', 'offeringTransaction_offeringPromotionId' - The ID that corresponds to a device offering promotion.
--
-- 'transactionId', 'offeringTransaction_transactionId' - The transaction ID of the offering transaction.
--
-- 'offeringStatus', 'offeringTransaction_offeringStatus' - The status of an offering transaction.
--
-- 'cost', 'offeringTransaction_cost' - The cost of an offering transaction.
newOfferingTransaction ::
  OfferingTransaction
newOfferingTransaction =
  OfferingTransaction'
    { createdOn = Prelude.Nothing,
      offeringPromotionId = Prelude.Nothing,
      transactionId = Prelude.Nothing,
      offeringStatus = Prelude.Nothing,
      cost = Prelude.Nothing
    }

-- | The date on which an offering transaction was created.
offeringTransaction_createdOn :: Lens.Lens' OfferingTransaction (Prelude.Maybe Prelude.UTCTime)
offeringTransaction_createdOn = Lens.lens (\OfferingTransaction' {createdOn} -> createdOn) (\s@OfferingTransaction' {} a -> s {createdOn = a} :: OfferingTransaction) Prelude.. Lens.mapping Core._Time

-- | The ID that corresponds to a device offering promotion.
offeringTransaction_offeringPromotionId :: Lens.Lens' OfferingTransaction (Prelude.Maybe Prelude.Text)
offeringTransaction_offeringPromotionId = Lens.lens (\OfferingTransaction' {offeringPromotionId} -> offeringPromotionId) (\s@OfferingTransaction' {} a -> s {offeringPromotionId = a} :: OfferingTransaction)

-- | The transaction ID of the offering transaction.
offeringTransaction_transactionId :: Lens.Lens' OfferingTransaction (Prelude.Maybe Prelude.Text)
offeringTransaction_transactionId = Lens.lens (\OfferingTransaction' {transactionId} -> transactionId) (\s@OfferingTransaction' {} a -> s {transactionId = a} :: OfferingTransaction)

-- | The status of an offering transaction.
offeringTransaction_offeringStatus :: Lens.Lens' OfferingTransaction (Prelude.Maybe OfferingStatus)
offeringTransaction_offeringStatus = Lens.lens (\OfferingTransaction' {offeringStatus} -> offeringStatus) (\s@OfferingTransaction' {} a -> s {offeringStatus = a} :: OfferingTransaction)

-- | The cost of an offering transaction.
offeringTransaction_cost :: Lens.Lens' OfferingTransaction (Prelude.Maybe MonetaryAmount)
offeringTransaction_cost = Lens.lens (\OfferingTransaction' {cost} -> cost) (\s@OfferingTransaction' {} a -> s {cost = a} :: OfferingTransaction)

instance Core.FromJSON OfferingTransaction where
  parseJSON =
    Core.withObject
      "OfferingTransaction"
      ( \x ->
          OfferingTransaction'
            Prelude.<$> (x Core..:? "createdOn")
            Prelude.<*> (x Core..:? "offeringPromotionId")
            Prelude.<*> (x Core..:? "transactionId")
            Prelude.<*> (x Core..:? "offeringStatus")
            Prelude.<*> (x Core..:? "cost")
      )

instance Prelude.Hashable OfferingTransaction where
  hashWithSalt _salt OfferingTransaction' {..} =
    _salt `Prelude.hashWithSalt` createdOn
      `Prelude.hashWithSalt` offeringPromotionId
      `Prelude.hashWithSalt` transactionId
      `Prelude.hashWithSalt` offeringStatus
      `Prelude.hashWithSalt` cost

instance Prelude.NFData OfferingTransaction where
  rnf OfferingTransaction' {..} =
    Prelude.rnf createdOn
      `Prelude.seq` Prelude.rnf offeringPromotionId
      `Prelude.seq` Prelude.rnf transactionId
      `Prelude.seq` Prelude.rnf offeringStatus
      `Prelude.seq` Prelude.rnf cost
