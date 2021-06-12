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
-- Module      : Network.AWS.Rekognition.Types.AgeRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.AgeRange where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Structure containing the estimated age range, in years, for a face.
--
-- Amazon Rekognition estimates an age range for faces detected in the
-- input image. Estimated age ranges can overlap. A face of a 5-year-old
-- might have an estimated range of 4-6, while the face of a 6-year-old
-- might have an estimated range of 4-8.
--
-- /See:/ 'newAgeRange' smart constructor.
data AgeRange = AgeRange'
  { -- | The highest estimated age.
    high :: Core.Maybe Core.Natural,
    -- | The lowest estimated age.
    low :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AgeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'high', 'ageRange_high' - The highest estimated age.
--
-- 'low', 'ageRange_low' - The lowest estimated age.
newAgeRange ::
  AgeRange
newAgeRange =
  AgeRange' {high = Core.Nothing, low = Core.Nothing}

-- | The highest estimated age.
ageRange_high :: Lens.Lens' AgeRange (Core.Maybe Core.Natural)
ageRange_high = Lens.lens (\AgeRange' {high} -> high) (\s@AgeRange' {} a -> s {high = a} :: AgeRange)

-- | The lowest estimated age.
ageRange_low :: Lens.Lens' AgeRange (Core.Maybe Core.Natural)
ageRange_low = Lens.lens (\AgeRange' {low} -> low) (\s@AgeRange' {} a -> s {low = a} :: AgeRange)

instance Core.FromJSON AgeRange where
  parseJSON =
    Core.withObject
      "AgeRange"
      ( \x ->
          AgeRange'
            Core.<$> (x Core..:? "High") Core.<*> (x Core..:? "Low")
      )

instance Core.Hashable AgeRange

instance Core.NFData AgeRange
