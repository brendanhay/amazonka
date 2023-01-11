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
-- Module      : Amazonka.ChimeSdkMeetings.Types.AttendeeIdItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMeetings.Types.AttendeeIdItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains one or more attendee IDs.
--
-- /See:/ 'newAttendeeIdItem' smart constructor.
data AttendeeIdItem = AttendeeIdItem'
  { -- | A list of one or more attendee IDs.
    attendeeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttendeeIdItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attendeeId', 'attendeeIdItem_attendeeId' - A list of one or more attendee IDs.
newAttendeeIdItem ::
  -- | 'attendeeId'
  Prelude.Text ->
  AttendeeIdItem
newAttendeeIdItem pAttendeeId_ =
  AttendeeIdItem' {attendeeId = pAttendeeId_}

-- | A list of one or more attendee IDs.
attendeeIdItem_attendeeId :: Lens.Lens' AttendeeIdItem Prelude.Text
attendeeIdItem_attendeeId = Lens.lens (\AttendeeIdItem' {attendeeId} -> attendeeId) (\s@AttendeeIdItem' {} a -> s {attendeeId = a} :: AttendeeIdItem)

instance Prelude.Hashable AttendeeIdItem where
  hashWithSalt _salt AttendeeIdItem' {..} =
    _salt `Prelude.hashWithSalt` attendeeId

instance Prelude.NFData AttendeeIdItem where
  rnf AttendeeIdItem' {..} = Prelude.rnf attendeeId

instance Data.ToJSON AttendeeIdItem where
  toJSON AttendeeIdItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AttendeeId" Data..= attendeeId)]
      )
