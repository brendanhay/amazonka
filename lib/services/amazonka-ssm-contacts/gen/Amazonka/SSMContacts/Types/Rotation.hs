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
-- Module      : Amazonka.SSMContacts.Types.Rotation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.Rotation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMContacts.Types.RecurrenceSettings

-- | Information about a rotation in an on-call schedule.
--
-- /See:/ 'newRotation' smart constructor.
data Rotation = Rotation'
  { -- | The Amazon Resource Names (ARNs) of the contacts assigned to the
    -- rotation team.
    contactIds :: Prelude.Maybe [Prelude.Text],
    -- | Information about when an on-call rotation is in effect and how long the
    -- rotation period lasts.
    recurrence :: Prelude.Maybe RecurrenceSettings,
    -- | The date and time the rotation becomes active.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The time zone the rotation’s activity is based on, in Internet Assigned
    -- Numbers Authority (IANA) format. For example: \"America\/Los_Angeles\",
    -- \"UTC\", or \"Asia\/Seoul\".
    timeZoneId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the rotation.
    rotationArn :: Prelude.Text,
    -- | The name of the rotation.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Rotation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactIds', 'rotation_contactIds' - The Amazon Resource Names (ARNs) of the contacts assigned to the
-- rotation team.
--
-- 'recurrence', 'rotation_recurrence' - Information about when an on-call rotation is in effect and how long the
-- rotation period lasts.
--
-- 'startTime', 'rotation_startTime' - The date and time the rotation becomes active.
--
-- 'timeZoneId', 'rotation_timeZoneId' - The time zone the rotation’s activity is based on, in Internet Assigned
-- Numbers Authority (IANA) format. For example: \"America\/Los_Angeles\",
-- \"UTC\", or \"Asia\/Seoul\".
--
-- 'rotationArn', 'rotation_rotationArn' - The Amazon Resource Name (ARN) of the rotation.
--
-- 'name', 'rotation_name' - The name of the rotation.
newRotation ::
  -- | 'rotationArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  Rotation
newRotation pRotationArn_ pName_ =
  Rotation'
    { contactIds = Prelude.Nothing,
      recurrence = Prelude.Nothing,
      startTime = Prelude.Nothing,
      timeZoneId = Prelude.Nothing,
      rotationArn = pRotationArn_,
      name = pName_
    }

-- | The Amazon Resource Names (ARNs) of the contacts assigned to the
-- rotation team.
rotation_contactIds :: Lens.Lens' Rotation (Prelude.Maybe [Prelude.Text])
rotation_contactIds = Lens.lens (\Rotation' {contactIds} -> contactIds) (\s@Rotation' {} a -> s {contactIds = a} :: Rotation) Prelude.. Lens.mapping Lens.coerced

-- | Information about when an on-call rotation is in effect and how long the
-- rotation period lasts.
rotation_recurrence :: Lens.Lens' Rotation (Prelude.Maybe RecurrenceSettings)
rotation_recurrence = Lens.lens (\Rotation' {recurrence} -> recurrence) (\s@Rotation' {} a -> s {recurrence = a} :: Rotation)

-- | The date and time the rotation becomes active.
rotation_startTime :: Lens.Lens' Rotation (Prelude.Maybe Prelude.UTCTime)
rotation_startTime = Lens.lens (\Rotation' {startTime} -> startTime) (\s@Rotation' {} a -> s {startTime = a} :: Rotation) Prelude.. Lens.mapping Data._Time

-- | The time zone the rotation’s activity is based on, in Internet Assigned
-- Numbers Authority (IANA) format. For example: \"America\/Los_Angeles\",
-- \"UTC\", or \"Asia\/Seoul\".
rotation_timeZoneId :: Lens.Lens' Rotation (Prelude.Maybe Prelude.Text)
rotation_timeZoneId = Lens.lens (\Rotation' {timeZoneId} -> timeZoneId) (\s@Rotation' {} a -> s {timeZoneId = a} :: Rotation)

-- | The Amazon Resource Name (ARN) of the rotation.
rotation_rotationArn :: Lens.Lens' Rotation Prelude.Text
rotation_rotationArn = Lens.lens (\Rotation' {rotationArn} -> rotationArn) (\s@Rotation' {} a -> s {rotationArn = a} :: Rotation)

-- | The name of the rotation.
rotation_name :: Lens.Lens' Rotation Prelude.Text
rotation_name = Lens.lens (\Rotation' {name} -> name) (\s@Rotation' {} a -> s {name = a} :: Rotation)

instance Data.FromJSON Rotation where
  parseJSON =
    Data.withObject
      "Rotation"
      ( \x ->
          Rotation'
            Prelude.<$> (x Data..:? "ContactIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Recurrence")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "TimeZoneId")
            Prelude.<*> (x Data..: "RotationArn")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable Rotation where
  hashWithSalt _salt Rotation' {..} =
    _salt
      `Prelude.hashWithSalt` contactIds
      `Prelude.hashWithSalt` recurrence
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` timeZoneId
      `Prelude.hashWithSalt` rotationArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData Rotation where
  rnf Rotation' {..} =
    Prelude.rnf contactIds
      `Prelude.seq` Prelude.rnf recurrence
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf timeZoneId
      `Prelude.seq` Prelude.rnf rotationArn
      `Prelude.seq` Prelude.rnf name
