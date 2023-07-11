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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.TechnicalCueSegment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.TechnicalCueType

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON TechnicalCueSegment where
  parseJSON =
    Data.withObject
      "TechnicalCueSegment"
      ( \x ->
          TechnicalCueSegment'
            Prelude.<$> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable TechnicalCueSegment where
  hashWithSalt _salt TechnicalCueSegment' {..} =
    _salt
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` type'

instance Prelude.NFData TechnicalCueSegment where
  rnf TechnicalCueSegment' {..} =
    Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf type'
