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
-- Module      : Amazonka.MediaLive.Types.Scte35TimeSignalScheduleActionSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Scte35TimeSignalScheduleActionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.Scte35Descriptor
import qualified Amazonka.Prelude as Prelude

-- | Settings for a SCTE-35 time_signal.
--
-- /See:/ 'newScte35TimeSignalScheduleActionSettings' smart constructor.
data Scte35TimeSignalScheduleActionSettings = Scte35TimeSignalScheduleActionSettings'
  { -- | The list of SCTE-35 descriptors accompanying the SCTE-35 time_signal.
    scte35Descriptors :: [Scte35Descriptor]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Scte35TimeSignalScheduleActionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scte35Descriptors', 'scte35TimeSignalScheduleActionSettings_scte35Descriptors' - The list of SCTE-35 descriptors accompanying the SCTE-35 time_signal.
newScte35TimeSignalScheduleActionSettings ::
  Scte35TimeSignalScheduleActionSettings
newScte35TimeSignalScheduleActionSettings =
  Scte35TimeSignalScheduleActionSettings'
    { scte35Descriptors =
        Prelude.mempty
    }

-- | The list of SCTE-35 descriptors accompanying the SCTE-35 time_signal.
scte35TimeSignalScheduleActionSettings_scte35Descriptors :: Lens.Lens' Scte35TimeSignalScheduleActionSettings [Scte35Descriptor]
scte35TimeSignalScheduleActionSettings_scte35Descriptors = Lens.lens (\Scte35TimeSignalScheduleActionSettings' {scte35Descriptors} -> scte35Descriptors) (\s@Scte35TimeSignalScheduleActionSettings' {} a -> s {scte35Descriptors = a} :: Scte35TimeSignalScheduleActionSettings) Prelude.. Lens.coerced

instance
  Data.FromJSON
    Scte35TimeSignalScheduleActionSettings
  where
  parseJSON =
    Data.withObject
      "Scte35TimeSignalScheduleActionSettings"
      ( \x ->
          Scte35TimeSignalScheduleActionSettings'
            Prelude.<$> ( x Data..:? "scte35Descriptors"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    Scte35TimeSignalScheduleActionSettings
  where
  hashWithSalt
    _salt
    Scte35TimeSignalScheduleActionSettings' {..} =
      _salt `Prelude.hashWithSalt` scte35Descriptors

instance
  Prelude.NFData
    Scte35TimeSignalScheduleActionSettings
  where
  rnf Scte35TimeSignalScheduleActionSettings' {..} =
    Prelude.rnf scte35Descriptors

instance
  Data.ToJSON
    Scte35TimeSignalScheduleActionSettings
  where
  toJSON Scte35TimeSignalScheduleActionSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("scte35Descriptors" Data..= scte35Descriptors)
          ]
      )
