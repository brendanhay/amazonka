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
-- Module      : Amazonka.Chime.Types.RoomRetentionSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.RoomRetentionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The retention settings that determine how long to retain chat-room
-- messages for an Amazon Chime Enterprise account.
--
-- /See:/ 'newRoomRetentionSettings' smart constructor.
data RoomRetentionSettings = RoomRetentionSettings'
  { -- | The number of days for which to retain chat-room messages.
    retentionDays :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoomRetentionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retentionDays', 'roomRetentionSettings_retentionDays' - The number of days for which to retain chat-room messages.
newRoomRetentionSettings ::
  RoomRetentionSettings
newRoomRetentionSettings =
  RoomRetentionSettings'
    { retentionDays =
        Prelude.Nothing
    }

-- | The number of days for which to retain chat-room messages.
roomRetentionSettings_retentionDays :: Lens.Lens' RoomRetentionSettings (Prelude.Maybe Prelude.Natural)
roomRetentionSettings_retentionDays = Lens.lens (\RoomRetentionSettings' {retentionDays} -> retentionDays) (\s@RoomRetentionSettings' {} a -> s {retentionDays = a} :: RoomRetentionSettings)

instance Core.FromJSON RoomRetentionSettings where
  parseJSON =
    Core.withObject
      "RoomRetentionSettings"
      ( \x ->
          RoomRetentionSettings'
            Prelude.<$> (x Core..:? "RetentionDays")
      )

instance Prelude.Hashable RoomRetentionSettings where
  hashWithSalt _salt RoomRetentionSettings' {..} =
    _salt `Prelude.hashWithSalt` retentionDays

instance Prelude.NFData RoomRetentionSettings where
  rnf RoomRetentionSettings' {..} =
    Prelude.rnf retentionDays

instance Core.ToJSON RoomRetentionSettings where
  toJSON RoomRetentionSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RetentionDays" Core..=)
              Prelude.<$> retentionDays
          ]
      )
