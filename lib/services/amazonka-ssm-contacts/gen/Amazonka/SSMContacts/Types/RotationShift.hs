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
-- Module      : Amazonka.SSMContacts.Types.RotationShift
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.RotationShift where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMContacts.Types.ShiftDetails
import Amazonka.SSMContacts.Types.ShiftType

-- | Information about a shift that belongs to an on-call rotation.
--
-- /See:/ 'newRotationShift' smart constructor.
data RotationShift = RotationShift'
  { -- | The Amazon Resource Names (ARNs) of the contacts who are part of the
    -- shift rotation.
    contactIds :: Prelude.Maybe [Prelude.Text],
    -- | Additional information about an on-call rotation shift.
    shiftDetails :: Prelude.Maybe ShiftDetails,
    -- | The type of shift rotation.
    type' :: Prelude.Maybe ShiftType,
    -- | The time a shift rotation begins.
    startTime :: Data.POSIX,
    -- | The time a shift rotation ends.
    endTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RotationShift' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactIds', 'rotationShift_contactIds' - The Amazon Resource Names (ARNs) of the contacts who are part of the
-- shift rotation.
--
-- 'shiftDetails', 'rotationShift_shiftDetails' - Additional information about an on-call rotation shift.
--
-- 'type'', 'rotationShift_type' - The type of shift rotation.
--
-- 'startTime', 'rotationShift_startTime' - The time a shift rotation begins.
--
-- 'endTime', 'rotationShift_endTime' - The time a shift rotation ends.
newRotationShift ::
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  RotationShift
newRotationShift pStartTime_ pEndTime_ =
  RotationShift'
    { contactIds = Prelude.Nothing,
      shiftDetails = Prelude.Nothing,
      type' = Prelude.Nothing,
      startTime = Data._Time Lens.# pStartTime_,
      endTime = Data._Time Lens.# pEndTime_
    }

-- | The Amazon Resource Names (ARNs) of the contacts who are part of the
-- shift rotation.
rotationShift_contactIds :: Lens.Lens' RotationShift (Prelude.Maybe [Prelude.Text])
rotationShift_contactIds = Lens.lens (\RotationShift' {contactIds} -> contactIds) (\s@RotationShift' {} a -> s {contactIds = a} :: RotationShift) Prelude.. Lens.mapping Lens.coerced

-- | Additional information about an on-call rotation shift.
rotationShift_shiftDetails :: Lens.Lens' RotationShift (Prelude.Maybe ShiftDetails)
rotationShift_shiftDetails = Lens.lens (\RotationShift' {shiftDetails} -> shiftDetails) (\s@RotationShift' {} a -> s {shiftDetails = a} :: RotationShift)

-- | The type of shift rotation.
rotationShift_type :: Lens.Lens' RotationShift (Prelude.Maybe ShiftType)
rotationShift_type = Lens.lens (\RotationShift' {type'} -> type') (\s@RotationShift' {} a -> s {type' = a} :: RotationShift)

-- | The time a shift rotation begins.
rotationShift_startTime :: Lens.Lens' RotationShift Prelude.UTCTime
rotationShift_startTime = Lens.lens (\RotationShift' {startTime} -> startTime) (\s@RotationShift' {} a -> s {startTime = a} :: RotationShift) Prelude.. Data._Time

-- | The time a shift rotation ends.
rotationShift_endTime :: Lens.Lens' RotationShift Prelude.UTCTime
rotationShift_endTime = Lens.lens (\RotationShift' {endTime} -> endTime) (\s@RotationShift' {} a -> s {endTime = a} :: RotationShift) Prelude.. Data._Time

instance Data.FromJSON RotationShift where
  parseJSON =
    Data.withObject
      "RotationShift"
      ( \x ->
          RotationShift'
            Prelude.<$> (x Data..:? "ContactIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ShiftDetails")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..: "StartTime")
            Prelude.<*> (x Data..: "EndTime")
      )

instance Prelude.Hashable RotationShift where
  hashWithSalt _salt RotationShift' {..} =
    _salt
      `Prelude.hashWithSalt` contactIds
      `Prelude.hashWithSalt` shiftDetails
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData RotationShift where
  rnf RotationShift' {..} =
    Prelude.rnf contactIds
      `Prelude.seq` Prelude.rnf shiftDetails
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime
