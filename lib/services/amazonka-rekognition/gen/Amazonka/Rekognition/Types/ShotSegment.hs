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
-- Module      : Amazonka.Rekognition.Types.ShotSegment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.ShotSegment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a shot detection segment detected in a video. For more
-- information, see SegmentDetection.
--
-- /See:/ 'newShotSegment' smart constructor.
data ShotSegment = ShotSegment'
  { -- | An Identifier for a shot detection segment detected in a video.
    index :: Prelude.Maybe Prelude.Natural,
    -- | The confidence that Amazon Rekognition Video has in the accuracy of the
    -- detected segment.
    confidence :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ShotSegment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'index', 'shotSegment_index' - An Identifier for a shot detection segment detected in a video.
--
-- 'confidence', 'shotSegment_confidence' - The confidence that Amazon Rekognition Video has in the accuracy of the
-- detected segment.
newShotSegment ::
  ShotSegment
newShotSegment =
  ShotSegment'
    { index = Prelude.Nothing,
      confidence = Prelude.Nothing
    }

-- | An Identifier for a shot detection segment detected in a video.
shotSegment_index :: Lens.Lens' ShotSegment (Prelude.Maybe Prelude.Natural)
shotSegment_index = Lens.lens (\ShotSegment' {index} -> index) (\s@ShotSegment' {} a -> s {index = a} :: ShotSegment)

-- | The confidence that Amazon Rekognition Video has in the accuracy of the
-- detected segment.
shotSegment_confidence :: Lens.Lens' ShotSegment (Prelude.Maybe Prelude.Double)
shotSegment_confidence = Lens.lens (\ShotSegment' {confidence} -> confidence) (\s@ShotSegment' {} a -> s {confidence = a} :: ShotSegment)

instance Core.FromJSON ShotSegment where
  parseJSON =
    Core.withObject
      "ShotSegment"
      ( \x ->
          ShotSegment'
            Prelude.<$> (x Core..:? "Index")
            Prelude.<*> (x Core..:? "Confidence")
      )

instance Prelude.Hashable ShotSegment where
  hashWithSalt _salt ShotSegment' {..} =
    _salt `Prelude.hashWithSalt` index
      `Prelude.hashWithSalt` confidence

instance Prelude.NFData ShotSegment where
  rnf ShotSegment' {..} =
    Prelude.rnf index
      `Prelude.seq` Prelude.rnf confidence
