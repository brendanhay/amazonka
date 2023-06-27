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
-- Module      : Amazonka.MediaConvert.Types.MotionImageInsertionFramerate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MotionImageInsertionFramerate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For motion overlays that don\'t have a built-in frame rate, specify the
-- frame rate of the overlay in frames per second, as a fraction. For
-- example, specify 24 fps as 24\/1. The overlay frame rate doesn\'t need
-- to match the frame rate of the underlying video.
--
-- /See:/ 'newMotionImageInsertionFramerate' smart constructor.
data MotionImageInsertionFramerate = MotionImageInsertionFramerate'
  { -- | The bottom of the fraction that expresses your overlay frame rate. For
    -- example, if your frame rate is 24 fps, set this value to 1.
    framerateDenominator :: Prelude.Maybe Prelude.Natural,
    -- | The top of the fraction that expresses your overlay frame rate. For
    -- example, if your frame rate is 24 fps, set this value to 24.
    framerateNumerator :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MotionImageInsertionFramerate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'framerateDenominator', 'motionImageInsertionFramerate_framerateDenominator' - The bottom of the fraction that expresses your overlay frame rate. For
-- example, if your frame rate is 24 fps, set this value to 1.
--
-- 'framerateNumerator', 'motionImageInsertionFramerate_framerateNumerator' - The top of the fraction that expresses your overlay frame rate. For
-- example, if your frame rate is 24 fps, set this value to 24.
newMotionImageInsertionFramerate ::
  MotionImageInsertionFramerate
newMotionImageInsertionFramerate =
  MotionImageInsertionFramerate'
    { framerateDenominator =
        Prelude.Nothing,
      framerateNumerator = Prelude.Nothing
    }

-- | The bottom of the fraction that expresses your overlay frame rate. For
-- example, if your frame rate is 24 fps, set this value to 1.
motionImageInsertionFramerate_framerateDenominator :: Lens.Lens' MotionImageInsertionFramerate (Prelude.Maybe Prelude.Natural)
motionImageInsertionFramerate_framerateDenominator = Lens.lens (\MotionImageInsertionFramerate' {framerateDenominator} -> framerateDenominator) (\s@MotionImageInsertionFramerate' {} a -> s {framerateDenominator = a} :: MotionImageInsertionFramerate)

-- | The top of the fraction that expresses your overlay frame rate. For
-- example, if your frame rate is 24 fps, set this value to 24.
motionImageInsertionFramerate_framerateNumerator :: Lens.Lens' MotionImageInsertionFramerate (Prelude.Maybe Prelude.Natural)
motionImageInsertionFramerate_framerateNumerator = Lens.lens (\MotionImageInsertionFramerate' {framerateNumerator} -> framerateNumerator) (\s@MotionImageInsertionFramerate' {} a -> s {framerateNumerator = a} :: MotionImageInsertionFramerate)

instance Data.FromJSON MotionImageInsertionFramerate where
  parseJSON =
    Data.withObject
      "MotionImageInsertionFramerate"
      ( \x ->
          MotionImageInsertionFramerate'
            Prelude.<$> (x Data..:? "framerateDenominator")
            Prelude.<*> (x Data..:? "framerateNumerator")
      )

instance
  Prelude.Hashable
    MotionImageInsertionFramerate
  where
  hashWithSalt _salt MotionImageInsertionFramerate' {..} =
    _salt
      `Prelude.hashWithSalt` framerateDenominator
      `Prelude.hashWithSalt` framerateNumerator

instance Prelude.NFData MotionImageInsertionFramerate where
  rnf MotionImageInsertionFramerate' {..} =
    Prelude.rnf framerateDenominator
      `Prelude.seq` Prelude.rnf framerateNumerator

instance Data.ToJSON MotionImageInsertionFramerate where
  toJSON MotionImageInsertionFramerate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("framerateDenominator" Data..=)
              Prelude.<$> framerateDenominator,
            ("framerateNumerator" Data..=)
              Prelude.<$> framerateNumerator
          ]
      )
