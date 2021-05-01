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
-- Module      : Network.AWS.MediaConvert.Types.DolbyVisionLevel6Metadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DolbyVisionLevel6Metadata where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Use these settings when you set DolbyVisionLevel6Mode to SPECIFY to
-- override the MaxCLL and MaxFALL values in your input with new values.
--
-- /See:/ 'newDolbyVisionLevel6Metadata' smart constructor.
data DolbyVisionLevel6Metadata = DolbyVisionLevel6Metadata'
  { -- | Maximum Content Light Level. Static HDR metadata that corresponds to the
    -- brightest pixel in the entire stream. Measured in nits.
    maxCll :: Prelude.Maybe Prelude.Natural,
    -- | Maximum Frame-Average Light Level. Static HDR metadata that corresponds
    -- to the highest frame-average brightness in the entire stream. Measured
    -- in nits.
    maxFall :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DolbyVisionLevel6Metadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxCll', 'dolbyVisionLevel6Metadata_maxCll' - Maximum Content Light Level. Static HDR metadata that corresponds to the
-- brightest pixel in the entire stream. Measured in nits.
--
-- 'maxFall', 'dolbyVisionLevel6Metadata_maxFall' - Maximum Frame-Average Light Level. Static HDR metadata that corresponds
-- to the highest frame-average brightness in the entire stream. Measured
-- in nits.
newDolbyVisionLevel6Metadata ::
  DolbyVisionLevel6Metadata
newDolbyVisionLevel6Metadata =
  DolbyVisionLevel6Metadata'
    { maxCll =
        Prelude.Nothing,
      maxFall = Prelude.Nothing
    }

-- | Maximum Content Light Level. Static HDR metadata that corresponds to the
-- brightest pixel in the entire stream. Measured in nits.
dolbyVisionLevel6Metadata_maxCll :: Lens.Lens' DolbyVisionLevel6Metadata (Prelude.Maybe Prelude.Natural)
dolbyVisionLevel6Metadata_maxCll = Lens.lens (\DolbyVisionLevel6Metadata' {maxCll} -> maxCll) (\s@DolbyVisionLevel6Metadata' {} a -> s {maxCll = a} :: DolbyVisionLevel6Metadata)

-- | Maximum Frame-Average Light Level. Static HDR metadata that corresponds
-- to the highest frame-average brightness in the entire stream. Measured
-- in nits.
dolbyVisionLevel6Metadata_maxFall :: Lens.Lens' DolbyVisionLevel6Metadata (Prelude.Maybe Prelude.Natural)
dolbyVisionLevel6Metadata_maxFall = Lens.lens (\DolbyVisionLevel6Metadata' {maxFall} -> maxFall) (\s@DolbyVisionLevel6Metadata' {} a -> s {maxFall = a} :: DolbyVisionLevel6Metadata)

instance Prelude.FromJSON DolbyVisionLevel6Metadata where
  parseJSON =
    Prelude.withObject
      "DolbyVisionLevel6Metadata"
      ( \x ->
          DolbyVisionLevel6Metadata'
            Prelude.<$> (x Prelude..:? "maxCll")
            Prelude.<*> (x Prelude..:? "maxFall")
      )

instance Prelude.Hashable DolbyVisionLevel6Metadata

instance Prelude.NFData DolbyVisionLevel6Metadata

instance Prelude.ToJSON DolbyVisionLevel6Metadata where
  toJSON DolbyVisionLevel6Metadata' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("maxCll" Prelude..=) Prelude.<$> maxCll,
            ("maxFall" Prelude..=) Prelude.<$> maxFall
          ]
      )
