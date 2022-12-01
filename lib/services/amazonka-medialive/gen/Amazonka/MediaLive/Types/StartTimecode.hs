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
-- Module      : Amazonka.MediaLive.Types.StartTimecode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.StartTimecode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Settings to identify the start of the clip.
--
-- /See:/ 'newStartTimecode' smart constructor.
data StartTimecode = StartTimecode'
  { -- | The timecode for the frame where you want to start the clip. Optional;
    -- if not specified, the clip starts at first frame in the file. Enter the
    -- timecode as HH:MM:SS:FF or HH:MM:SS;FF.
    timecode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTimecode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timecode', 'startTimecode_timecode' - The timecode for the frame where you want to start the clip. Optional;
-- if not specified, the clip starts at first frame in the file. Enter the
-- timecode as HH:MM:SS:FF or HH:MM:SS;FF.
newStartTimecode ::
  StartTimecode
newStartTimecode =
  StartTimecode' {timecode = Prelude.Nothing}

-- | The timecode for the frame where you want to start the clip. Optional;
-- if not specified, the clip starts at first frame in the file. Enter the
-- timecode as HH:MM:SS:FF or HH:MM:SS;FF.
startTimecode_timecode :: Lens.Lens' StartTimecode (Prelude.Maybe Prelude.Text)
startTimecode_timecode = Lens.lens (\StartTimecode' {timecode} -> timecode) (\s@StartTimecode' {} a -> s {timecode = a} :: StartTimecode)

instance Core.FromJSON StartTimecode where
  parseJSON =
    Core.withObject
      "StartTimecode"
      ( \x ->
          StartTimecode' Prelude.<$> (x Core..:? "timecode")
      )

instance Prelude.Hashable StartTimecode where
  hashWithSalt _salt StartTimecode' {..} =
    _salt `Prelude.hashWithSalt` timecode

instance Prelude.NFData StartTimecode where
  rnf StartTimecode' {..} = Prelude.rnf timecode

instance Core.ToJSON StartTimecode where
  toJSON StartTimecode' {..} =
    Core.object
      ( Prelude.catMaybes
          [("timecode" Core..=) Prelude.<$> timecode]
      )
