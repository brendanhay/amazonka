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
-- Module      : Amazonka.SSMContacts.Types.RotationOverride
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.RotationOverride where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an override specified for an on-call rotation.
--
-- /See:/ 'newRotationOverride' smart constructor.
data RotationOverride = RotationOverride'
  { -- | The Amazon Resource Name (ARN) of the override to an on-call rotation.
    rotationOverrideId :: Prelude.Text,
    -- | The Amazon Resource Names (ARNs) of the contacts assigned to the
    -- override of the on-call rotation.
    newContactIds' :: [Prelude.Text],
    -- | The time a rotation override begins.
    startTime :: Data.POSIX,
    -- | The time a rotation override ends.
    endTime :: Data.POSIX,
    -- | The time a rotation override was created.
    createTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RotationOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rotationOverrideId', 'rotationOverride_rotationOverrideId' - The Amazon Resource Name (ARN) of the override to an on-call rotation.
--
-- 'newContactIds'', 'rotationOverride_newContactIds' - The Amazon Resource Names (ARNs) of the contacts assigned to the
-- override of the on-call rotation.
--
-- 'startTime', 'rotationOverride_startTime' - The time a rotation override begins.
--
-- 'endTime', 'rotationOverride_endTime' - The time a rotation override ends.
--
-- 'createTime', 'rotationOverride_createTime' - The time a rotation override was created.
newRotationOverride ::
  -- | 'rotationOverrideId'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'createTime'
  Prelude.UTCTime ->
  RotationOverride
newRotationOverride
  pRotationOverrideId_
  pStartTime_
  pEndTime_
  pCreateTime_ =
    RotationOverride'
      { rotationOverrideId =
          pRotationOverrideId_,
        newContactIds' = Prelude.mempty,
        startTime = Data._Time Lens.# pStartTime_,
        endTime = Data._Time Lens.# pEndTime_,
        createTime = Data._Time Lens.# pCreateTime_
      }

-- | The Amazon Resource Name (ARN) of the override to an on-call rotation.
rotationOverride_rotationOverrideId :: Lens.Lens' RotationOverride Prelude.Text
rotationOverride_rotationOverrideId = Lens.lens (\RotationOverride' {rotationOverrideId} -> rotationOverrideId) (\s@RotationOverride' {} a -> s {rotationOverrideId = a} :: RotationOverride)

-- | The Amazon Resource Names (ARNs) of the contacts assigned to the
-- override of the on-call rotation.
rotationOverride_newContactIds :: Lens.Lens' RotationOverride [Prelude.Text]
rotationOverride_newContactIds = Lens.lens (\RotationOverride' {newContactIds'} -> newContactIds') (\s@RotationOverride' {} a -> s {newContactIds' = a} :: RotationOverride) Prelude.. Lens.coerced

-- | The time a rotation override begins.
rotationOverride_startTime :: Lens.Lens' RotationOverride Prelude.UTCTime
rotationOverride_startTime = Lens.lens (\RotationOverride' {startTime} -> startTime) (\s@RotationOverride' {} a -> s {startTime = a} :: RotationOverride) Prelude.. Data._Time

-- | The time a rotation override ends.
rotationOverride_endTime :: Lens.Lens' RotationOverride Prelude.UTCTime
rotationOverride_endTime = Lens.lens (\RotationOverride' {endTime} -> endTime) (\s@RotationOverride' {} a -> s {endTime = a} :: RotationOverride) Prelude.. Data._Time

-- | The time a rotation override was created.
rotationOverride_createTime :: Lens.Lens' RotationOverride Prelude.UTCTime
rotationOverride_createTime = Lens.lens (\RotationOverride' {createTime} -> createTime) (\s@RotationOverride' {} a -> s {createTime = a} :: RotationOverride) Prelude.. Data._Time

instance Data.FromJSON RotationOverride where
  parseJSON =
    Data.withObject
      "RotationOverride"
      ( \x ->
          RotationOverride'
            Prelude.<$> (x Data..: "RotationOverrideId")
            Prelude.<*> (x Data..:? "NewContactIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "StartTime")
            Prelude.<*> (x Data..: "EndTime")
            Prelude.<*> (x Data..: "CreateTime")
      )

instance Prelude.Hashable RotationOverride where
  hashWithSalt _salt RotationOverride' {..} =
    _salt
      `Prelude.hashWithSalt` rotationOverrideId
      `Prelude.hashWithSalt` newContactIds'
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` createTime

instance Prelude.NFData RotationOverride where
  rnf RotationOverride' {..} =
    Prelude.rnf rotationOverrideId
      `Prelude.seq` Prelude.rnf newContactIds'
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf createTime
