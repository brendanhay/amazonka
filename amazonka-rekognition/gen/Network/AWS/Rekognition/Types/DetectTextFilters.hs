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
-- Module      : Network.AWS.Rekognition.Types.DetectTextFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.DetectTextFilters where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.DetectionFilter
import Network.AWS.Rekognition.Types.RegionOfInterest

-- | A set of optional parameters that you can use to set the criteria that
-- the text must meet to be included in your response. @WordFilter@ looks
-- at a word’s height, width, and minimum confidence. @RegionOfInterest@
-- lets you set a specific region of the image to look for text in.
--
-- /See:/ 'newDetectTextFilters' smart constructor.
data DetectTextFilters = DetectTextFilters'
  { -- | A Filter focusing on a certain area of the image. Uses a @BoundingBox@
    -- object to set the region of the image.
    regionsOfInterest :: Prelude.Maybe [RegionOfInterest],
    wordFilter :: Prelude.Maybe DetectionFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetectTextFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionsOfInterest', 'detectTextFilters_regionsOfInterest' - A Filter focusing on a certain area of the image. Uses a @BoundingBox@
-- object to set the region of the image.
--
-- 'wordFilter', 'detectTextFilters_wordFilter' - Undocumented member.
newDetectTextFilters ::
  DetectTextFilters
newDetectTextFilters =
  DetectTextFilters'
    { regionsOfInterest =
        Prelude.Nothing,
      wordFilter = Prelude.Nothing
    }

-- | A Filter focusing on a certain area of the image. Uses a @BoundingBox@
-- object to set the region of the image.
detectTextFilters_regionsOfInterest :: Lens.Lens' DetectTextFilters (Prelude.Maybe [RegionOfInterest])
detectTextFilters_regionsOfInterest = Lens.lens (\DetectTextFilters' {regionsOfInterest} -> regionsOfInterest) (\s@DetectTextFilters' {} a -> s {regionsOfInterest = a} :: DetectTextFilters) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
detectTextFilters_wordFilter :: Lens.Lens' DetectTextFilters (Prelude.Maybe DetectionFilter)
detectTextFilters_wordFilter = Lens.lens (\DetectTextFilters' {wordFilter} -> wordFilter) (\s@DetectTextFilters' {} a -> s {wordFilter = a} :: DetectTextFilters)

instance Prelude.Hashable DetectTextFilters

instance Prelude.NFData DetectTextFilters

instance Prelude.ToJSON DetectTextFilters where
  toJSON DetectTextFilters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RegionsOfInterest" Prelude..=)
              Prelude.<$> regionsOfInterest,
            ("WordFilter" Prelude..=) Prelude.<$> wordFilter
          ]
      )
