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
-- Module      : Amazonka.Rekognition.Types.TextDetectionResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.TextDetectionResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.TextDetection

-- | Information about text detected in a video. Incudes the detected text,
-- the time in milliseconds from the start of the video that the text was
-- detected, and where it was detected on the screen.
--
-- /See:/ 'newTextDetectionResult' smart constructor.
data TextDetectionResult = TextDetectionResult'
  { -- | The time, in milliseconds from the start of the video, that the text was
    -- detected. Note that @Timestamp@ is not guaranteed to be accurate to the
    -- individual frame where the text first appears.
    timestamp :: Prelude.Maybe Prelude.Integer,
    -- | Details about text detected in a video.
    textDetection :: Prelude.Maybe TextDetection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TextDetectionResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestamp', 'textDetectionResult_timestamp' - The time, in milliseconds from the start of the video, that the text was
-- detected. Note that @Timestamp@ is not guaranteed to be accurate to the
-- individual frame where the text first appears.
--
-- 'textDetection', 'textDetectionResult_textDetection' - Details about text detected in a video.
newTextDetectionResult ::
  TextDetectionResult
newTextDetectionResult =
  TextDetectionResult'
    { timestamp = Prelude.Nothing,
      textDetection = Prelude.Nothing
    }

-- | The time, in milliseconds from the start of the video, that the text was
-- detected. Note that @Timestamp@ is not guaranteed to be accurate to the
-- individual frame where the text first appears.
textDetectionResult_timestamp :: Lens.Lens' TextDetectionResult (Prelude.Maybe Prelude.Integer)
textDetectionResult_timestamp = Lens.lens (\TextDetectionResult' {timestamp} -> timestamp) (\s@TextDetectionResult' {} a -> s {timestamp = a} :: TextDetectionResult)

-- | Details about text detected in a video.
textDetectionResult_textDetection :: Lens.Lens' TextDetectionResult (Prelude.Maybe TextDetection)
textDetectionResult_textDetection = Lens.lens (\TextDetectionResult' {textDetection} -> textDetection) (\s@TextDetectionResult' {} a -> s {textDetection = a} :: TextDetectionResult)

instance Core.FromJSON TextDetectionResult where
  parseJSON =
    Core.withObject
      "TextDetectionResult"
      ( \x ->
          TextDetectionResult'
            Prelude.<$> (x Core..:? "Timestamp")
            Prelude.<*> (x Core..:? "TextDetection")
      )

instance Prelude.Hashable TextDetectionResult where
  hashWithSalt _salt TextDetectionResult' {..} =
    _salt `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` textDetection

instance Prelude.NFData TextDetectionResult where
  rnf TextDetectionResult' {..} =
    Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf textDetection
