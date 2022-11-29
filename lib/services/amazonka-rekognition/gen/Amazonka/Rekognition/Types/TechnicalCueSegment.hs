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
-- Module      : Amazonka.Rekognition.Types.TechnicalCueSegment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.TechnicalCueSegment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.TechnicalCueType

-- | Information about a technical cue segment. For more information, see
-- SegmentDetection.
--
-- /See:/ 'newTechnicalCueSegment' smart constructor.
data TechnicalCueSegment = TechnicalCueSegment'
  { -- | The type of the technical cue.
    type' :: Prelude.Maybe TechnicalCueType,
    -- | The confidence that Amazon Rekognition Video has in the accuracy of the
    -- detected segment.
    confidence :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TechnicalCueSegment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'technicalCueSegment_type' - The type of the technical cue.
--
-- 'confidence', 'technicalCueSegment_confidence' - The confidence that Amazon Rekognition Video has in the accuracy of the
-- detected segment.
newTechnicalCueSegment ::
  TechnicalCueSegment
newTechnicalCueSegment =
  TechnicalCueSegment'
    { type' = Prelude.Nothing,
      confidence = Prelude.Nothing
    }

-- | The type of the technical cue.
technicalCueSegment_type :: Lens.Lens' TechnicalCueSegment (Prelude.Maybe TechnicalCueType)
technicalCueSegment_type = Lens.lens (\TechnicalCueSegment' {type'} -> type') (\s@TechnicalCueSegment' {} a -> s {type' = a} :: TechnicalCueSegment)

-- | The confidence that Amazon Rekognition Video has in the accuracy of the
-- detected segment.
technicalCueSegment_confidence :: Lens.Lens' TechnicalCueSegment (Prelude.Maybe Prelude.Double)
technicalCueSegment_confidence = Lens.lens (\TechnicalCueSegment' {confidence} -> confidence) (\s@TechnicalCueSegment' {} a -> s {confidence = a} :: TechnicalCueSegment)

instance Core.FromJSON TechnicalCueSegment where
  parseJSON =
    Core.withObject
      "TechnicalCueSegment"
      ( \x ->
          TechnicalCueSegment'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Confidence")
      )

instance Prelude.Hashable TechnicalCueSegment where
  hashWithSalt _salt TechnicalCueSegment' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` confidence

instance Prelude.NFData TechnicalCueSegment where
  rnf TechnicalCueSegment' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf confidence
