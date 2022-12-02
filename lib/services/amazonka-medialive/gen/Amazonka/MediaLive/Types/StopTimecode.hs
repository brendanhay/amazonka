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
-- Module      : Amazonka.MediaLive.Types.StopTimecode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.StopTimecode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.LastFrameClippingBehavior
import qualified Amazonka.Prelude as Prelude

-- | Settings to identify the end of the clip.
--
-- /See:/ 'newStopTimecode' smart constructor.
data StopTimecode = StopTimecode'
  { -- | If you specify a StopTimecode in an input (in order to clip the file),
    -- you can specify if you want the clip to exclude (the default) or include
    -- the frame specified by the timecode.
    lastFrameClippingBehavior :: Prelude.Maybe LastFrameClippingBehavior,
    -- | The timecode for the frame where you want to stop the clip. Optional; if
    -- not specified, the clip continues to the end of the file. Enter the
    -- timecode as HH:MM:SS:FF or HH:MM:SS;FF.
    timecode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopTimecode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastFrameClippingBehavior', 'stopTimecode_lastFrameClippingBehavior' - If you specify a StopTimecode in an input (in order to clip the file),
-- you can specify if you want the clip to exclude (the default) or include
-- the frame specified by the timecode.
--
-- 'timecode', 'stopTimecode_timecode' - The timecode for the frame where you want to stop the clip. Optional; if
-- not specified, the clip continues to the end of the file. Enter the
-- timecode as HH:MM:SS:FF or HH:MM:SS;FF.
newStopTimecode ::
  StopTimecode
newStopTimecode =
  StopTimecode'
    { lastFrameClippingBehavior =
        Prelude.Nothing,
      timecode = Prelude.Nothing
    }

-- | If you specify a StopTimecode in an input (in order to clip the file),
-- you can specify if you want the clip to exclude (the default) or include
-- the frame specified by the timecode.
stopTimecode_lastFrameClippingBehavior :: Lens.Lens' StopTimecode (Prelude.Maybe LastFrameClippingBehavior)
stopTimecode_lastFrameClippingBehavior = Lens.lens (\StopTimecode' {lastFrameClippingBehavior} -> lastFrameClippingBehavior) (\s@StopTimecode' {} a -> s {lastFrameClippingBehavior = a} :: StopTimecode)

-- | The timecode for the frame where you want to stop the clip. Optional; if
-- not specified, the clip continues to the end of the file. Enter the
-- timecode as HH:MM:SS:FF or HH:MM:SS;FF.
stopTimecode_timecode :: Lens.Lens' StopTimecode (Prelude.Maybe Prelude.Text)
stopTimecode_timecode = Lens.lens (\StopTimecode' {timecode} -> timecode) (\s@StopTimecode' {} a -> s {timecode = a} :: StopTimecode)

instance Data.FromJSON StopTimecode where
  parseJSON =
    Data.withObject
      "StopTimecode"
      ( \x ->
          StopTimecode'
            Prelude.<$> (x Data..:? "lastFrameClippingBehavior")
            Prelude.<*> (x Data..:? "timecode")
      )

instance Prelude.Hashable StopTimecode where
  hashWithSalt _salt StopTimecode' {..} =
    _salt
      `Prelude.hashWithSalt` lastFrameClippingBehavior
      `Prelude.hashWithSalt` timecode

instance Prelude.NFData StopTimecode where
  rnf StopTimecode' {..} =
    Prelude.rnf lastFrameClippingBehavior
      `Prelude.seq` Prelude.rnf timecode

instance Data.ToJSON StopTimecode where
  toJSON StopTimecode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("lastFrameClippingBehavior" Data..=)
              Prelude.<$> lastFrameClippingBehavior,
            ("timecode" Data..=) Prelude.<$> timecode
          ]
      )
