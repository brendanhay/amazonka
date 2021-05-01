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
-- Module      : Network.AWS.MediaLive.Types.Scte35TimeSignalScheduleActionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35TimeSignalScheduleActionSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Scte35Descriptor
import qualified Network.AWS.Prelude as Prelude

-- | Settings for a SCTE-35 time_signal.
--
-- /See:/ 'newScte35TimeSignalScheduleActionSettings' smart constructor.
data Scte35TimeSignalScheduleActionSettings = Scte35TimeSignalScheduleActionSettings'
  { -- | The list of SCTE-35 descriptors accompanying the SCTE-35 time_signal.
    scte35Descriptors :: [Scte35Descriptor]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
scte35TimeSignalScheduleActionSettings_scte35Descriptors = Lens.lens (\Scte35TimeSignalScheduleActionSettings' {scte35Descriptors} -> scte35Descriptors) (\s@Scte35TimeSignalScheduleActionSettings' {} a -> s {scte35Descriptors = a} :: Scte35TimeSignalScheduleActionSettings) Prelude.. Prelude._Coerce

instance
  Prelude.FromJSON
    Scte35TimeSignalScheduleActionSettings
  where
  parseJSON =
    Prelude.withObject
      "Scte35TimeSignalScheduleActionSettings"
      ( \x ->
          Scte35TimeSignalScheduleActionSettings'
            Prelude.<$> ( x Prelude..:? "scte35Descriptors"
                            Prelude..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    Scte35TimeSignalScheduleActionSettings

instance
  Prelude.NFData
    Scte35TimeSignalScheduleActionSettings

instance
  Prelude.ToJSON
    Scte35TimeSignalScheduleActionSettings
  where
  toJSON Scte35TimeSignalScheduleActionSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("scte35Descriptors" Prelude..= scte35Descriptors)
          ]
      )
