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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.SelectedVideoStreams
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.SelectedVideoStreams where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The video streams for a specified media pipeline. The total number of
-- video streams can\'t exceed 25.
--
-- /See:/ 'newSelectedVideoStreams' smart constructor.
data SelectedVideoStreams = SelectedVideoStreams'
  { -- | The attendee IDs of the streams selected for a media pipeline.
    attendeeIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The external user IDs of the streams selected for a media pipeline.
    externalUserIds :: Prelude.Maybe (Prelude.NonEmpty (Data.Sensitive Prelude.Text))
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelectedVideoStreams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attendeeIds', 'selectedVideoStreams_attendeeIds' - The attendee IDs of the streams selected for a media pipeline.
--
-- 'externalUserIds', 'selectedVideoStreams_externalUserIds' - The external user IDs of the streams selected for a media pipeline.
newSelectedVideoStreams ::
  SelectedVideoStreams
newSelectedVideoStreams =
  SelectedVideoStreams'
    { attendeeIds =
        Prelude.Nothing,
      externalUserIds = Prelude.Nothing
    }

-- | The attendee IDs of the streams selected for a media pipeline.
selectedVideoStreams_attendeeIds :: Lens.Lens' SelectedVideoStreams (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
selectedVideoStreams_attendeeIds = Lens.lens (\SelectedVideoStreams' {attendeeIds} -> attendeeIds) (\s@SelectedVideoStreams' {} a -> s {attendeeIds = a} :: SelectedVideoStreams) Prelude.. Lens.mapping Lens.coerced

-- | The external user IDs of the streams selected for a media pipeline.
selectedVideoStreams_externalUserIds :: Lens.Lens' SelectedVideoStreams (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
selectedVideoStreams_externalUserIds = Lens.lens (\SelectedVideoStreams' {externalUserIds} -> externalUserIds) (\s@SelectedVideoStreams' {} a -> s {externalUserIds = a} :: SelectedVideoStreams) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SelectedVideoStreams where
  parseJSON =
    Data.withObject
      "SelectedVideoStreams"
      ( \x ->
          SelectedVideoStreams'
            Prelude.<$> (x Data..:? "AttendeeIds")
            Prelude.<*> (x Data..:? "ExternalUserIds")
      )

instance Prelude.Hashable SelectedVideoStreams where
  hashWithSalt _salt SelectedVideoStreams' {..} =
    _salt
      `Prelude.hashWithSalt` attendeeIds
      `Prelude.hashWithSalt` externalUserIds

instance Prelude.NFData SelectedVideoStreams where
  rnf SelectedVideoStreams' {..} =
    Prelude.rnf attendeeIds
      `Prelude.seq` Prelude.rnf externalUserIds

instance Data.ToJSON SelectedVideoStreams where
  toJSON SelectedVideoStreams' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttendeeIds" Data..=) Prelude.<$> attendeeIds,
            ("ExternalUserIds" Data..=)
              Prelude.<$> externalUserIds
          ]
      )
