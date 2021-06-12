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
-- Module      : Network.AWS.Rekognition.Types.ContentModerationDetection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ContentModerationDetection where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types.ModerationLabel

-- | Information about an unsafe content label detection in a stored video.
--
-- /See:/ 'newContentModerationDetection' smart constructor.
data ContentModerationDetection = ContentModerationDetection'
  { -- | Time, in milliseconds from the beginning of the video, that the unsafe
    -- content label was detected.
    timestamp :: Core.Maybe Core.Integer,
    -- | The unsafe content label detected by in the stored video.
    moderationLabel :: Core.Maybe ModerationLabel
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ContentModerationDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestamp', 'contentModerationDetection_timestamp' - Time, in milliseconds from the beginning of the video, that the unsafe
-- content label was detected.
--
-- 'moderationLabel', 'contentModerationDetection_moderationLabel' - The unsafe content label detected by in the stored video.
newContentModerationDetection ::
  ContentModerationDetection
newContentModerationDetection =
  ContentModerationDetection'
    { timestamp =
        Core.Nothing,
      moderationLabel = Core.Nothing
    }

-- | Time, in milliseconds from the beginning of the video, that the unsafe
-- content label was detected.
contentModerationDetection_timestamp :: Lens.Lens' ContentModerationDetection (Core.Maybe Core.Integer)
contentModerationDetection_timestamp = Lens.lens (\ContentModerationDetection' {timestamp} -> timestamp) (\s@ContentModerationDetection' {} a -> s {timestamp = a} :: ContentModerationDetection)

-- | The unsafe content label detected by in the stored video.
contentModerationDetection_moderationLabel :: Lens.Lens' ContentModerationDetection (Core.Maybe ModerationLabel)
contentModerationDetection_moderationLabel = Lens.lens (\ContentModerationDetection' {moderationLabel} -> moderationLabel) (\s@ContentModerationDetection' {} a -> s {moderationLabel = a} :: ContentModerationDetection)

instance Core.FromJSON ContentModerationDetection where
  parseJSON =
    Core.withObject
      "ContentModerationDetection"
      ( \x ->
          ContentModerationDetection'
            Core.<$> (x Core..:? "Timestamp")
            Core.<*> (x Core..:? "ModerationLabel")
      )

instance Core.Hashable ContentModerationDetection

instance Core.NFData ContentModerationDetection
