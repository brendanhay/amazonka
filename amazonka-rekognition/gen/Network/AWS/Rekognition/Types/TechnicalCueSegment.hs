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
-- Module      : Network.AWS.Rekognition.Types.TechnicalCueSegment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TechnicalCueSegment where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.TechnicalCueType

-- | Information about a technical cue segment. For more information, see
-- SegmentDetection.
--
-- /See:/ 'newTechnicalCueSegment' smart constructor.
data TechnicalCueSegment = TechnicalCueSegment'
  { -- | The confidence that Amazon Rekognition Video has in the accuracy of the
    -- detected segment.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | The type of the technical cue.
    type' :: Prelude.Maybe TechnicalCueType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TechnicalCueSegment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'technicalCueSegment_confidence' - The confidence that Amazon Rekognition Video has in the accuracy of the
-- detected segment.
--
-- 'type'', 'technicalCueSegment_type' - The type of the technical cue.
newTechnicalCueSegment ::
  TechnicalCueSegment
newTechnicalCueSegment =
  TechnicalCueSegment'
    { confidence = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The confidence that Amazon Rekognition Video has in the accuracy of the
-- detected segment.
technicalCueSegment_confidence :: Lens.Lens' TechnicalCueSegment (Prelude.Maybe Prelude.Double)
technicalCueSegment_confidence = Lens.lens (\TechnicalCueSegment' {confidence} -> confidence) (\s@TechnicalCueSegment' {} a -> s {confidence = a} :: TechnicalCueSegment)

-- | The type of the technical cue.
technicalCueSegment_type :: Lens.Lens' TechnicalCueSegment (Prelude.Maybe TechnicalCueType)
technicalCueSegment_type = Lens.lens (\TechnicalCueSegment' {type'} -> type') (\s@TechnicalCueSegment' {} a -> s {type' = a} :: TechnicalCueSegment)

instance Prelude.FromJSON TechnicalCueSegment where
  parseJSON =
    Prelude.withObject
      "TechnicalCueSegment"
      ( \x ->
          TechnicalCueSegment'
            Prelude.<$> (x Prelude..:? "Confidence")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable TechnicalCueSegment

instance Prelude.NFData TechnicalCueSegment
