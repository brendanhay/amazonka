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
-- Module      : Network.AWS.Rekognition.Types.ShotSegment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ShotSegment where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a shot detection segment detected in a video. For more
-- information, see SegmentDetection.
--
-- /See:/ 'newShotSegment' smart constructor.
data ShotSegment = ShotSegment'
  { -- | The confidence that Amazon Rekognition Video has in the accuracy of the
    -- detected segment.
    confidence :: Core.Maybe Core.Double,
    -- | An Identifier for a shot detection segment detected in a video.
    index :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ShotSegment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'shotSegment_confidence' - The confidence that Amazon Rekognition Video has in the accuracy of the
-- detected segment.
--
-- 'index', 'shotSegment_index' - An Identifier for a shot detection segment detected in a video.
newShotSegment ::
  ShotSegment
newShotSegment =
  ShotSegment'
    { confidence = Core.Nothing,
      index = Core.Nothing
    }

-- | The confidence that Amazon Rekognition Video has in the accuracy of the
-- detected segment.
shotSegment_confidence :: Lens.Lens' ShotSegment (Core.Maybe Core.Double)
shotSegment_confidence = Lens.lens (\ShotSegment' {confidence} -> confidence) (\s@ShotSegment' {} a -> s {confidence = a} :: ShotSegment)

-- | An Identifier for a shot detection segment detected in a video.
shotSegment_index :: Lens.Lens' ShotSegment (Core.Maybe Core.Natural)
shotSegment_index = Lens.lens (\ShotSegment' {index} -> index) (\s@ShotSegment' {} a -> s {index = a} :: ShotSegment)

instance Core.FromJSON ShotSegment where
  parseJSON =
    Core.withObject
      "ShotSegment"
      ( \x ->
          ShotSegment'
            Core.<$> (x Core..:? "Confidence")
            Core.<*> (x Core..:? "Index")
      )

instance Core.Hashable ShotSegment

instance Core.NFData ShotSegment
