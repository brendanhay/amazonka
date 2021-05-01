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
-- Module      : Network.AWS.MediaLive.Types.InputClippingSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputClippingSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputTimecodeSource
import Network.AWS.MediaLive.Types.StartTimecode
import Network.AWS.MediaLive.Types.StopTimecode
import qualified Network.AWS.Prelude as Prelude

-- | Settings to let you create a clip of the file input, in order to set up
-- the input to ingest only a portion of the file.
--
-- /See:/ 'newInputClippingSettings' smart constructor.
data InputClippingSettings = InputClippingSettings'
  { -- | Settings to identify the start of the clip.
    startTimecode :: Prelude.Maybe StartTimecode,
    -- | Settings to identify the end of the clip.
    stopTimecode :: Prelude.Maybe StopTimecode,
    -- | The source of the timecodes in the source being clipped.
    inputTimecodeSource :: InputTimecodeSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InputClippingSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTimecode', 'inputClippingSettings_startTimecode' - Settings to identify the start of the clip.
--
-- 'stopTimecode', 'inputClippingSettings_stopTimecode' - Settings to identify the end of the clip.
--
-- 'inputTimecodeSource', 'inputClippingSettings_inputTimecodeSource' - The source of the timecodes in the source being clipped.
newInputClippingSettings ::
  -- | 'inputTimecodeSource'
  InputTimecodeSource ->
  InputClippingSettings
newInputClippingSettings pInputTimecodeSource_ =
  InputClippingSettings'
    { startTimecode =
        Prelude.Nothing,
      stopTimecode = Prelude.Nothing,
      inputTimecodeSource = pInputTimecodeSource_
    }

-- | Settings to identify the start of the clip.
inputClippingSettings_startTimecode :: Lens.Lens' InputClippingSettings (Prelude.Maybe StartTimecode)
inputClippingSettings_startTimecode = Lens.lens (\InputClippingSettings' {startTimecode} -> startTimecode) (\s@InputClippingSettings' {} a -> s {startTimecode = a} :: InputClippingSettings)

-- | Settings to identify the end of the clip.
inputClippingSettings_stopTimecode :: Lens.Lens' InputClippingSettings (Prelude.Maybe StopTimecode)
inputClippingSettings_stopTimecode = Lens.lens (\InputClippingSettings' {stopTimecode} -> stopTimecode) (\s@InputClippingSettings' {} a -> s {stopTimecode = a} :: InputClippingSettings)

-- | The source of the timecodes in the source being clipped.
inputClippingSettings_inputTimecodeSource :: Lens.Lens' InputClippingSettings InputTimecodeSource
inputClippingSettings_inputTimecodeSource = Lens.lens (\InputClippingSettings' {inputTimecodeSource} -> inputTimecodeSource) (\s@InputClippingSettings' {} a -> s {inputTimecodeSource = a} :: InputClippingSettings)

instance Prelude.FromJSON InputClippingSettings where
  parseJSON =
    Prelude.withObject
      "InputClippingSettings"
      ( \x ->
          InputClippingSettings'
            Prelude.<$> (x Prelude..:? "startTimecode")
            Prelude.<*> (x Prelude..:? "stopTimecode")
            Prelude.<*> (x Prelude..: "inputTimecodeSource")
      )

instance Prelude.Hashable InputClippingSettings

instance Prelude.NFData InputClippingSettings

instance Prelude.ToJSON InputClippingSettings where
  toJSON InputClippingSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("startTimecode" Prelude..=)
              Prelude.<$> startTimecode,
            ("stopTimecode" Prelude..=) Prelude.<$> stopTimecode,
            Prelude.Just
              ( "inputTimecodeSource"
                  Prelude..= inputTimecodeSource
              )
          ]
      )
