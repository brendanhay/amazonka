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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.TextDetectionResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.TextDetection

-- | Information about text detected in a video. Incudes the detected text,
-- the time in milliseconds from the start of the video that the text was
-- detected, and where it was detected on the screen.
--
-- /See:/ 'newTextDetectionResult' smart constructor.
data TextDetectionResult = TextDetectionResult'
  { -- | Details about text detected in a video.
    textDetection :: Prelude.Maybe TextDetection,
    -- | The time, in milliseconds from the start of the video, that the text was
    -- detected. Note that @Timestamp@ is not guaranteed to be accurate to the
    -- individual frame where the text first appears.
    timestamp :: Prelude.Maybe Prelude.Integer
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
-- 'textDetection', 'textDetectionResult_textDetection' - Details about text detected in a video.
--
-- 'timestamp', 'textDetectionResult_timestamp' - The time, in milliseconds from the start of the video, that the text was
-- detected. Note that @Timestamp@ is not guaranteed to be accurate to the
-- individual frame where the text first appears.
newTextDetectionResult ::
  TextDetectionResult
newTextDetectionResult =
  TextDetectionResult'
    { textDetection =
        Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | Details about text detected in a video.
textDetectionResult_textDetection :: Lens.Lens' TextDetectionResult (Prelude.Maybe TextDetection)
textDetectionResult_textDetection = Lens.lens (\TextDetectionResult' {textDetection} -> textDetection) (\s@TextDetectionResult' {} a -> s {textDetection = a} :: TextDetectionResult)

-- | The time, in milliseconds from the start of the video, that the text was
-- detected. Note that @Timestamp@ is not guaranteed to be accurate to the
-- individual frame where the text first appears.
textDetectionResult_timestamp :: Lens.Lens' TextDetectionResult (Prelude.Maybe Prelude.Integer)
textDetectionResult_timestamp = Lens.lens (\TextDetectionResult' {timestamp} -> timestamp) (\s@TextDetectionResult' {} a -> s {timestamp = a} :: TextDetectionResult)

instance Data.FromJSON TextDetectionResult where
  parseJSON =
    Data.withObject
      "TextDetectionResult"
      ( \x ->
          TextDetectionResult'
            Prelude.<$> (x Data..:? "TextDetection")
            Prelude.<*> (x Data..:? "Timestamp")
      )

instance Prelude.Hashable TextDetectionResult where
  hashWithSalt _salt TextDetectionResult' {..} =
    _salt
      `Prelude.hashWithSalt` textDetection
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData TextDetectionResult where
  rnf TextDetectionResult' {..} =
    Prelude.rnf textDetection
      `Prelude.seq` Prelude.rnf timestamp
