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
-- Module      : Network.AWS.MediaLive.Types.StopTimecode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.StopTimecode where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.LastFrameClippingBehavior
import qualified Network.AWS.Prelude as Prelude

-- | Settings to identify the end of the clip.
--
-- /See:/ 'newStopTimecode' smart constructor.
data StopTimecode = StopTimecode'
  { -- | The timecode for the frame where you want to stop the clip. Optional; if
    -- not specified, the clip continues to the end of the file. Enter the
    -- timecode as HH:MM:SS:FF or HH:MM:SS;FF.
    timecode :: Prelude.Maybe Prelude.Text,
    -- | If you specify a StopTimecode in an input (in order to clip the file),
    -- you can specify if you want the clip to exclude (the default) or include
    -- the frame specified by the timecode.
    lastFrameClippingBehavior :: Prelude.Maybe LastFrameClippingBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopTimecode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timecode', 'stopTimecode_timecode' - The timecode for the frame where you want to stop the clip. Optional; if
-- not specified, the clip continues to the end of the file. Enter the
-- timecode as HH:MM:SS:FF or HH:MM:SS;FF.
--
-- 'lastFrameClippingBehavior', 'stopTimecode_lastFrameClippingBehavior' - If you specify a StopTimecode in an input (in order to clip the file),
-- you can specify if you want the clip to exclude (the default) or include
-- the frame specified by the timecode.
newStopTimecode ::
  StopTimecode
newStopTimecode =
  StopTimecode'
    { timecode = Prelude.Nothing,
      lastFrameClippingBehavior = Prelude.Nothing
    }

-- | The timecode for the frame where you want to stop the clip. Optional; if
-- not specified, the clip continues to the end of the file. Enter the
-- timecode as HH:MM:SS:FF or HH:MM:SS;FF.
stopTimecode_timecode :: Lens.Lens' StopTimecode (Prelude.Maybe Prelude.Text)
stopTimecode_timecode = Lens.lens (\StopTimecode' {timecode} -> timecode) (\s@StopTimecode' {} a -> s {timecode = a} :: StopTimecode)

-- | If you specify a StopTimecode in an input (in order to clip the file),
-- you can specify if you want the clip to exclude (the default) or include
-- the frame specified by the timecode.
stopTimecode_lastFrameClippingBehavior :: Lens.Lens' StopTimecode (Prelude.Maybe LastFrameClippingBehavior)
stopTimecode_lastFrameClippingBehavior = Lens.lens (\StopTimecode' {lastFrameClippingBehavior} -> lastFrameClippingBehavior) (\s@StopTimecode' {} a -> s {lastFrameClippingBehavior = a} :: StopTimecode)

instance Prelude.FromJSON StopTimecode where
  parseJSON =
    Prelude.withObject
      "StopTimecode"
      ( \x ->
          StopTimecode'
            Prelude.<$> (x Prelude..:? "timecode")
            Prelude.<*> (x Prelude..:? "lastFrameClippingBehavior")
      )

instance Prelude.Hashable StopTimecode

instance Prelude.NFData StopTimecode

instance Prelude.ToJSON StopTimecode where
  toJSON StopTimecode' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("timecode" Prelude..=) Prelude.<$> timecode,
            ("lastFrameClippingBehavior" Prelude..=)
              Prelude.<$> lastFrameClippingBehavior
          ]
      )
