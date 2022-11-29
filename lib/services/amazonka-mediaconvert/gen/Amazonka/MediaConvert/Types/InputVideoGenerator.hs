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
-- Module      : Amazonka.MediaConvert.Types.InputVideoGenerator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.InputVideoGenerator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | When you include Video generator, MediaConvert creates a video input
-- with black frames. Use this setting if you do not have a video input or
-- if you want to add black video frames before, or after, other inputs.
-- You can specify Video generator, or you can specify an Input file, but
-- you cannot specify both. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/video-generator.html
--
-- /See:/ 'newInputVideoGenerator' smart constructor.
data InputVideoGenerator = InputVideoGenerator'
  { -- | Specify an integer value for Black video duration from 50 to 86400000 to
    -- generate a black video input for that many milliseconds. Required when
    -- you include Video generator.
    duration :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputVideoGenerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'duration', 'inputVideoGenerator_duration' - Specify an integer value for Black video duration from 50 to 86400000 to
-- generate a black video input for that many milliseconds. Required when
-- you include Video generator.
newInputVideoGenerator ::
  InputVideoGenerator
newInputVideoGenerator =
  InputVideoGenerator' {duration = Prelude.Nothing}

-- | Specify an integer value for Black video duration from 50 to 86400000 to
-- generate a black video input for that many milliseconds. Required when
-- you include Video generator.
inputVideoGenerator_duration :: Lens.Lens' InputVideoGenerator (Prelude.Maybe Prelude.Natural)
inputVideoGenerator_duration = Lens.lens (\InputVideoGenerator' {duration} -> duration) (\s@InputVideoGenerator' {} a -> s {duration = a} :: InputVideoGenerator)

instance Core.FromJSON InputVideoGenerator where
  parseJSON =
    Core.withObject
      "InputVideoGenerator"
      ( \x ->
          InputVideoGenerator'
            Prelude.<$> (x Core..:? "duration")
      )

instance Prelude.Hashable InputVideoGenerator where
  hashWithSalt _salt InputVideoGenerator' {..} =
    _salt `Prelude.hashWithSalt` duration

instance Prelude.NFData InputVideoGenerator where
  rnf InputVideoGenerator' {..} = Prelude.rnf duration

instance Core.ToJSON InputVideoGenerator where
  toJSON InputVideoGenerator' {..} =
    Core.object
      ( Prelude.catMaybes
          [("duration" Core..=) Prelude.<$> duration]
      )
