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
-- Module      : Network.AWS.MediaLive.Types.StartTimecode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.StartTimecode where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Settings to identify the start of the clip.
--
-- /See:/ 'newStartTimecode' smart constructor.
data StartTimecode = StartTimecode'
  { -- | The timecode for the frame where you want to start the clip. Optional;
    -- if not specified, the clip starts at first frame in the file. Enter the
    -- timecode as HH:MM:SS:FF or HH:MM:SS;FF.
    timecode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON StartTimecode where
  parseJSON =
    Prelude.withObject
      "StartTimecode"
      ( \x ->
          StartTimecode'
            Prelude.<$> (x Prelude..:? "timecode")
      )

instance Prelude.Hashable StartTimecode

instance Prelude.NFData StartTimecode

instance Prelude.ToJSON StartTimecode where
  toJSON StartTimecode' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("timecode" Prelude..=) Prelude.<$> timecode]
      )
