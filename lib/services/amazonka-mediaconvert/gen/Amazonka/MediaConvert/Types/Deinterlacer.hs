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
-- Module      : Amazonka.MediaConvert.Types.Deinterlacer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Deinterlacer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.DeinterlaceAlgorithm
import Amazonka.MediaConvert.Types.DeinterlacerControl
import Amazonka.MediaConvert.Types.DeinterlacerMode
import qualified Amazonka.Prelude as Prelude

-- | Settings for deinterlacer
--
-- /See:/ 'newDeinterlacer' smart constructor.
data Deinterlacer = Deinterlacer'
  { -- | Only applies when you set Deinterlacer (DeinterlaceMode) to Deinterlace
    -- (DEINTERLACE) or Adaptive (ADAPTIVE). Motion adaptive interpolate
    -- (INTERPOLATE) produces sharper pictures, while blend (BLEND) produces
    -- smoother motion. Use (INTERPOLATE_TICKER) OR (BLEND_TICKER) if your
    -- source file includes a ticker, such as a scrolling headline at the
    -- bottom of the frame.
    algorithm :: Prelude.Maybe DeinterlaceAlgorithm,
    -- | - When set to NORMAL (default), the deinterlacer does not convert frames
    -- that are tagged in metadata as progressive. It will only convert those
    -- that are tagged as some other type. - When set to FORCE_ALL_FRAMES, the
    -- deinterlacer converts every frame to progressive - even those that are
    -- already tagged as progressive. Turn Force mode on only if there is a
    -- good chance that the metadata has tagged frames as progressive when they
    -- are not progressive. Do not turn on otherwise; processing frames that
    -- are already progressive into progressive will probably result in lower
    -- quality video.
    control :: Prelude.Maybe DeinterlacerControl,
    -- | Use Deinterlacer (DeinterlaceMode) to choose how the service will do
    -- deinterlacing. Default is Deinterlace. - Deinterlace converts interlaced
    -- to progressive. - Inverse telecine converts Hard Telecine 29.97i to
    -- progressive 23.976p. - Adaptive auto-detects and converts to
    -- progressive.
    mode :: Prelude.Maybe DeinterlacerMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Deinterlacer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithm', 'deinterlacer_algorithm' - Only applies when you set Deinterlacer (DeinterlaceMode) to Deinterlace
-- (DEINTERLACE) or Adaptive (ADAPTIVE). Motion adaptive interpolate
-- (INTERPOLATE) produces sharper pictures, while blend (BLEND) produces
-- smoother motion. Use (INTERPOLATE_TICKER) OR (BLEND_TICKER) if your
-- source file includes a ticker, such as a scrolling headline at the
-- bottom of the frame.
--
-- 'control', 'deinterlacer_control' - - When set to NORMAL (default), the deinterlacer does not convert frames
-- that are tagged in metadata as progressive. It will only convert those
-- that are tagged as some other type. - When set to FORCE_ALL_FRAMES, the
-- deinterlacer converts every frame to progressive - even those that are
-- already tagged as progressive. Turn Force mode on only if there is a
-- good chance that the metadata has tagged frames as progressive when they
-- are not progressive. Do not turn on otherwise; processing frames that
-- are already progressive into progressive will probably result in lower
-- quality video.
--
-- 'mode', 'deinterlacer_mode' - Use Deinterlacer (DeinterlaceMode) to choose how the service will do
-- deinterlacing. Default is Deinterlace. - Deinterlace converts interlaced
-- to progressive. - Inverse telecine converts Hard Telecine 29.97i to
-- progressive 23.976p. - Adaptive auto-detects and converts to
-- progressive.
newDeinterlacer ::
  Deinterlacer
newDeinterlacer =
  Deinterlacer'
    { algorithm = Prelude.Nothing,
      control = Prelude.Nothing,
      mode = Prelude.Nothing
    }

-- | Only applies when you set Deinterlacer (DeinterlaceMode) to Deinterlace
-- (DEINTERLACE) or Adaptive (ADAPTIVE). Motion adaptive interpolate
-- (INTERPOLATE) produces sharper pictures, while blend (BLEND) produces
-- smoother motion. Use (INTERPOLATE_TICKER) OR (BLEND_TICKER) if your
-- source file includes a ticker, such as a scrolling headline at the
-- bottom of the frame.
deinterlacer_algorithm :: Lens.Lens' Deinterlacer (Prelude.Maybe DeinterlaceAlgorithm)
deinterlacer_algorithm = Lens.lens (\Deinterlacer' {algorithm} -> algorithm) (\s@Deinterlacer' {} a -> s {algorithm = a} :: Deinterlacer)

-- | - When set to NORMAL (default), the deinterlacer does not convert frames
-- that are tagged in metadata as progressive. It will only convert those
-- that are tagged as some other type. - When set to FORCE_ALL_FRAMES, the
-- deinterlacer converts every frame to progressive - even those that are
-- already tagged as progressive. Turn Force mode on only if there is a
-- good chance that the metadata has tagged frames as progressive when they
-- are not progressive. Do not turn on otherwise; processing frames that
-- are already progressive into progressive will probably result in lower
-- quality video.
deinterlacer_control :: Lens.Lens' Deinterlacer (Prelude.Maybe DeinterlacerControl)
deinterlacer_control = Lens.lens (\Deinterlacer' {control} -> control) (\s@Deinterlacer' {} a -> s {control = a} :: Deinterlacer)

-- | Use Deinterlacer (DeinterlaceMode) to choose how the service will do
-- deinterlacing. Default is Deinterlace. - Deinterlace converts interlaced
-- to progressive. - Inverse telecine converts Hard Telecine 29.97i to
-- progressive 23.976p. - Adaptive auto-detects and converts to
-- progressive.
deinterlacer_mode :: Lens.Lens' Deinterlacer (Prelude.Maybe DeinterlacerMode)
deinterlacer_mode = Lens.lens (\Deinterlacer' {mode} -> mode) (\s@Deinterlacer' {} a -> s {mode = a} :: Deinterlacer)

instance Data.FromJSON Deinterlacer where
  parseJSON =
    Data.withObject
      "Deinterlacer"
      ( \x ->
          Deinterlacer'
            Prelude.<$> (x Data..:? "algorithm")
            Prelude.<*> (x Data..:? "control")
            Prelude.<*> (x Data..:? "mode")
      )

instance Prelude.Hashable Deinterlacer where
  hashWithSalt _salt Deinterlacer' {..} =
    _salt `Prelude.hashWithSalt` algorithm
      `Prelude.hashWithSalt` control
      `Prelude.hashWithSalt` mode

instance Prelude.NFData Deinterlacer where
  rnf Deinterlacer' {..} =
    Prelude.rnf algorithm
      `Prelude.seq` Prelude.rnf control
      `Prelude.seq` Prelude.rnf mode

instance Data.ToJSON Deinterlacer where
  toJSON Deinterlacer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("algorithm" Data..=) Prelude.<$> algorithm,
            ("control" Data..=) Prelude.<$> control,
            ("mode" Data..=) Prelude.<$> mode
          ]
      )
