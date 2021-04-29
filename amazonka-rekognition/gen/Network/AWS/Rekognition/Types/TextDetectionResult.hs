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
-- Module      : Network.AWS.Rekognition.Types.TextDetectionResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TextDetectionResult where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.TextDetection

-- | Information about text detected in a video. Incudes the detected text,
-- the time in milliseconds from the start of the video that the text was
-- detected, and where it was detected on the screen.
--
-- /See:/ 'newTextDetectionResult' smart constructor.
data TextDetectionResult = TextDetectionResult'
  { -- | Details about text detected in a video.
    textDetection :: Prelude.Maybe TextDetection,
    -- | The time, in milliseconds from the start of the video, that the text was
    -- detected.
    timestamp :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- detected.
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
-- detected.
textDetectionResult_timestamp :: Lens.Lens' TextDetectionResult (Prelude.Maybe Prelude.Integer)
textDetectionResult_timestamp = Lens.lens (\TextDetectionResult' {timestamp} -> timestamp) (\s@TextDetectionResult' {} a -> s {timestamp = a} :: TextDetectionResult)

instance Prelude.FromJSON TextDetectionResult where
  parseJSON =
    Prelude.withObject
      "TextDetectionResult"
      ( \x ->
          TextDetectionResult'
            Prelude.<$> (x Prelude..:? "TextDetection")
            Prelude.<*> (x Prelude..:? "Timestamp")
      )

instance Prelude.Hashable TextDetectionResult

instance Prelude.NFData TextDetectionResult
