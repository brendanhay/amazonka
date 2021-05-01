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
-- Module      : Network.AWS.MediaLive.Types.AvailSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AvailSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Scte35SpliceInsert
import Network.AWS.MediaLive.Types.Scte35TimeSignalApos
import qualified Network.AWS.Prelude as Prelude

-- | Avail Settings
--
-- /See:/ 'newAvailSettings' smart constructor.
data AvailSettings = AvailSettings'
  { scte35TimeSignalApos :: Prelude.Maybe Scte35TimeSignalApos,
    scte35SpliceInsert :: Prelude.Maybe Scte35SpliceInsert
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AvailSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scte35TimeSignalApos', 'availSettings_scte35TimeSignalApos' - Undocumented member.
--
-- 'scte35SpliceInsert', 'availSettings_scte35SpliceInsert' - Undocumented member.
newAvailSettings ::
  AvailSettings
newAvailSettings =
  AvailSettings'
    { scte35TimeSignalApos =
        Prelude.Nothing,
      scte35SpliceInsert = Prelude.Nothing
    }

-- | Undocumented member.
availSettings_scte35TimeSignalApos :: Lens.Lens' AvailSettings (Prelude.Maybe Scte35TimeSignalApos)
availSettings_scte35TimeSignalApos = Lens.lens (\AvailSettings' {scte35TimeSignalApos} -> scte35TimeSignalApos) (\s@AvailSettings' {} a -> s {scte35TimeSignalApos = a} :: AvailSettings)

-- | Undocumented member.
availSettings_scte35SpliceInsert :: Lens.Lens' AvailSettings (Prelude.Maybe Scte35SpliceInsert)
availSettings_scte35SpliceInsert = Lens.lens (\AvailSettings' {scte35SpliceInsert} -> scte35SpliceInsert) (\s@AvailSettings' {} a -> s {scte35SpliceInsert = a} :: AvailSettings)

instance Prelude.FromJSON AvailSettings where
  parseJSON =
    Prelude.withObject
      "AvailSettings"
      ( \x ->
          AvailSettings'
            Prelude.<$> (x Prelude..:? "scte35TimeSignalApos")
            Prelude.<*> (x Prelude..:? "scte35SpliceInsert")
      )

instance Prelude.Hashable AvailSettings

instance Prelude.NFData AvailSettings

instance Prelude.ToJSON AvailSettings where
  toJSON AvailSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("scte35TimeSignalApos" Prelude..=)
              Prelude.<$> scte35TimeSignalApos,
            ("scte35SpliceInsert" Prelude..=)
              Prelude.<$> scte35SpliceInsert
          ]
      )
