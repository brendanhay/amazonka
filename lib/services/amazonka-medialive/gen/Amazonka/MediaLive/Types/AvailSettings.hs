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
-- Module      : Amazonka.MediaLive.Types.AvailSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AvailSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaLive.Types.Scte35SpliceInsert
import Amazonka.MediaLive.Types.Scte35TimeSignalApos
import qualified Amazonka.Prelude as Prelude

-- | Avail Settings
--
-- /See:/ 'newAvailSettings' smart constructor.
data AvailSettings = AvailSettings'
  { scte35SpliceInsert :: Prelude.Maybe Scte35SpliceInsert,
    scte35TimeSignalApos :: Prelude.Maybe Scte35TimeSignalApos
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AvailSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scte35SpliceInsert', 'availSettings_scte35SpliceInsert' - Undocumented member.
--
-- 'scte35TimeSignalApos', 'availSettings_scte35TimeSignalApos' - Undocumented member.
newAvailSettings ::
  AvailSettings
newAvailSettings =
  AvailSettings'
    { scte35SpliceInsert =
        Prelude.Nothing,
      scte35TimeSignalApos = Prelude.Nothing
    }

-- | Undocumented member.
availSettings_scte35SpliceInsert :: Lens.Lens' AvailSettings (Prelude.Maybe Scte35SpliceInsert)
availSettings_scte35SpliceInsert = Lens.lens (\AvailSettings' {scte35SpliceInsert} -> scte35SpliceInsert) (\s@AvailSettings' {} a -> s {scte35SpliceInsert = a} :: AvailSettings)

-- | Undocumented member.
availSettings_scte35TimeSignalApos :: Lens.Lens' AvailSettings (Prelude.Maybe Scte35TimeSignalApos)
availSettings_scte35TimeSignalApos = Lens.lens (\AvailSettings' {scte35TimeSignalApos} -> scte35TimeSignalApos) (\s@AvailSettings' {} a -> s {scte35TimeSignalApos = a} :: AvailSettings)

instance Core.FromJSON AvailSettings where
  parseJSON =
    Core.withObject
      "AvailSettings"
      ( \x ->
          AvailSettings'
            Prelude.<$> (x Core..:? "scte35SpliceInsert")
            Prelude.<*> (x Core..:? "scte35TimeSignalApos")
      )

instance Prelude.Hashable AvailSettings where
  hashWithSalt _salt AvailSettings' {..} =
    _salt `Prelude.hashWithSalt` scte35SpliceInsert
      `Prelude.hashWithSalt` scte35TimeSignalApos

instance Prelude.NFData AvailSettings where
  rnf AvailSettings' {..} =
    Prelude.rnf scte35SpliceInsert
      `Prelude.seq` Prelude.rnf scte35TimeSignalApos

instance Core.ToJSON AvailSettings where
  toJSON AvailSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("scte35SpliceInsert" Core..=)
              Prelude.<$> scte35SpliceInsert,
            ("scte35TimeSignalApos" Core..=)
              Prelude.<$> scte35TimeSignalApos
          ]
      )
