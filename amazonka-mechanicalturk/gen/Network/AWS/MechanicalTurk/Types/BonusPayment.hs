{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MechanicalTurk.Types.BonusPayment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.BonusPayment where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing a Bonus payment paid to a Worker.
--
-- /See:/ 'newBonusPayment' smart constructor.
data BonusPayment = BonusPayment'
  { bonusAmount :: Prelude.Maybe Prelude.Text,
    -- | The ID of the assignment associated with this bonus payment.
    assignmentId :: Prelude.Maybe Prelude.Text,
    -- | The date and time of when the bonus was granted.
    grantTime :: Prelude.Maybe Prelude.POSIX,
    -- | The Reason text given when the bonus was granted, if any.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Worker to whom the bonus was paid.
    workerId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BonusPayment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bonusAmount', 'bonusPayment_bonusAmount' - Undocumented member.
--
-- 'assignmentId', 'bonusPayment_assignmentId' - The ID of the assignment associated with this bonus payment.
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
    { bonusAmount = Prelude.Nothing,
      assignmentId = Prelude.Nothing,
      grantTime = Prelude.Nothing,
      reason = Prelude.Nothing,
      workerId = Prelude.Nothing
    }

-- | Undocumented member.
bonusPayment_bonusAmount :: Lens.Lens' BonusPayment (Prelude.Maybe Prelude.Text)
bonusPayment_bonusAmount = Lens.lens (\BonusPayment' {bonusAmount} -> bonusAmount) (\s@BonusPayment' {} a -> s {bonusAmount = a} :: BonusPayment)

-- | The ID of the assignment associated with this bonus payment.
bonusPayment_assignmentId :: Lens.Lens' BonusPayment (Prelude.Maybe Prelude.Text)
bonusPayment_assignmentId = Lens.lens (\BonusPayment' {assignmentId} -> assignmentId) (\s@BonusPayment' {} a -> s {assignmentId = a} :: BonusPayment)

-- | The date and time of when the bonus was granted.
bonusPayment_grantTime :: Lens.Lens' BonusPayment (Prelude.Maybe Prelude.UTCTime)
bonusPayment_grantTime = Lens.lens (\BonusPayment' {grantTime} -> grantTime) (\s@BonusPayment' {} a -> s {grantTime = a} :: BonusPayment) Prelude.. Lens.mapping Prelude._Time

-- | The Reason text given when the bonus was granted, if any.
bonusPayment_reason :: Lens.Lens' BonusPayment (Prelude.Maybe Prelude.Text)
bonusPayment_reason = Lens.lens (\BonusPayment' {reason} -> reason) (\s@BonusPayment' {} a -> s {reason = a} :: BonusPayment)

-- | The ID of the Worker to whom the bonus was paid.
bonusPayment_workerId :: Lens.Lens' BonusPayment (Prelude.Maybe Prelude.Text)
bonusPayment_workerId = Lens.lens (\BonusPayment' {workerId} -> workerId) (\s@BonusPayment' {} a -> s {workerId = a} :: BonusPayment)

instance Prelude.FromJSON BonusPayment where
  parseJSON =
    Prelude.withObject
      "BonusPayment"
      ( \x ->
          BonusPayment'
            Prelude.<$> (x Prelude..:? "BonusAmount")
            Prelude.<*> (x Prelude..:? "AssignmentId")
            Prelude.<*> (x Prelude..:? "GrantTime")
            Prelude.<*> (x Prelude..:? "Reason")
            Prelude.<*> (x Prelude..:? "WorkerId")
      )

instance Prelude.Hashable BonusPayment

instance Prelude.NFData BonusPayment
