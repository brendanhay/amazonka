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
-- Module      : Amazonka.MechanicalTurk.Types.BonusPayment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.BonusPayment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing a Bonus payment paid to a Worker.
--
-- /See:/ 'newBonusPayment' smart constructor.
data BonusPayment = BonusPayment'
  { -- | The ID of the assignment associated with this bonus payment.
    assignmentId :: Prelude.Maybe Prelude.Text,
    bonusAmount :: Prelude.Maybe Prelude.Text,
    -- | The date and time of when the bonus was granted.
    grantTime :: Prelude.Maybe Data.POSIX,
    -- | The Reason text given when the bonus was granted, if any.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Worker to whom the bonus was paid.
    workerId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BonusPayment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assignmentId', 'bonusPayment_assignmentId' - The ID of the assignment associated with this bonus payment.
--
-- 'bonusAmount', 'bonusPayment_bonusAmount' - Undocumented member.
--
-- 'grantTime', 'bonusPayment_grantTime' - The date and time of when the bonus was granted.
--
-- 'reason', 'bonusPayment_reason' - The Reason text given when the bonus was granted, if any.
--
-- 'workerId', 'bonusPayment_workerId' - The ID of the Worker to whom the bonus was paid.
newBonusPayment ::
  BonusPayment
newBonusPayment =
  BonusPayment'
    { assignmentId = Prelude.Nothing,
      bonusAmount = Prelude.Nothing,
      grantTime = Prelude.Nothing,
      reason = Prelude.Nothing,
      workerId = Prelude.Nothing
    }

-- | The ID of the assignment associated with this bonus payment.
bonusPayment_assignmentId :: Lens.Lens' BonusPayment (Prelude.Maybe Prelude.Text)
bonusPayment_assignmentId = Lens.lens (\BonusPayment' {assignmentId} -> assignmentId) (\s@BonusPayment' {} a -> s {assignmentId = a} :: BonusPayment)

-- | Undocumented member.
bonusPayment_bonusAmount :: Lens.Lens' BonusPayment (Prelude.Maybe Prelude.Text)
bonusPayment_bonusAmount = Lens.lens (\BonusPayment' {bonusAmount} -> bonusAmount) (\s@BonusPayment' {} a -> s {bonusAmount = a} :: BonusPayment)

-- | The date and time of when the bonus was granted.
bonusPayment_grantTime :: Lens.Lens' BonusPayment (Prelude.Maybe Prelude.UTCTime)
bonusPayment_grantTime = Lens.lens (\BonusPayment' {grantTime} -> grantTime) (\s@BonusPayment' {} a -> s {grantTime = a} :: BonusPayment) Prelude.. Lens.mapping Data._Time

-- | The Reason text given when the bonus was granted, if any.
bonusPayment_reason :: Lens.Lens' BonusPayment (Prelude.Maybe Prelude.Text)
bonusPayment_reason = Lens.lens (\BonusPayment' {reason} -> reason) (\s@BonusPayment' {} a -> s {reason = a} :: BonusPayment)

-- | The ID of the Worker to whom the bonus was paid.
bonusPayment_workerId :: Lens.Lens' BonusPayment (Prelude.Maybe Prelude.Text)
bonusPayment_workerId = Lens.lens (\BonusPayment' {workerId} -> workerId) (\s@BonusPayment' {} a -> s {workerId = a} :: BonusPayment)

instance Data.FromJSON BonusPayment where
  parseJSON =
    Data.withObject
      "BonusPayment"
      ( \x ->
          BonusPayment'
            Prelude.<$> (x Data..:? "AssignmentId")
            Prelude.<*> (x Data..:? "BonusAmount")
            Prelude.<*> (x Data..:? "GrantTime")
            Prelude.<*> (x Data..:? "Reason")
            Prelude.<*> (x Data..:? "WorkerId")
      )

instance Prelude.Hashable BonusPayment where
  hashWithSalt _salt BonusPayment' {..} =
    _salt `Prelude.hashWithSalt` assignmentId
      `Prelude.hashWithSalt` bonusAmount
      `Prelude.hashWithSalt` grantTime
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` workerId

instance Prelude.NFData BonusPayment where
  rnf BonusPayment' {..} =
    Prelude.rnf assignmentId
      `Prelude.seq` Prelude.rnf bonusAmount
      `Prelude.seq` Prelude.rnf grantTime
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf workerId
