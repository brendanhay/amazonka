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
-- Module      : Network.AWS.Rekognition.Types.StartTextDetectionFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StartTextDetectionFilters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types.DetectionFilter
import Network.AWS.Rekognition.Types.RegionOfInterest

-- | Set of optional parameters that let you set the criteria text must meet
-- to be included in your response. @WordFilter@ looks at a word\'s height,
-- width and minimum confidence. @RegionOfInterest@ lets you set a specific
-- region of the screen to look for text in.
--
-- /See:/ 'newStartTextDetectionFilters' smart constructor.
data StartTextDetectionFilters = StartTextDetectionFilters'
  { -- | Filter focusing on a certain area of the frame. Uses a @BoundingBox@
    -- object to set the region of the screen.
    regionsOfInterest :: Core.Maybe [RegionOfInterest],
    -- | Filters focusing on qualities of the text, such as confidence or size.
    wordFilter :: Core.Maybe DetectionFilter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartTextDetectionFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionsOfInterest', 'startTextDetectionFilters_regionsOfInterest' - Filter focusing on a certain area of the frame. Uses a @BoundingBox@
-- object to set the region of the screen.
--
-- 'wordFilter', 'startTextDetectionFilters_wordFilter' - Filters focusing on qualities of the text, such as confidence or size.
newStartTextDetectionFilters ::
  StartTextDetectionFilters
newStartTextDetectionFilters =
  StartTextDetectionFilters'
    { regionsOfInterest =
        Core.Nothing,
      wordFilter = Core.Nothing
    }

-- | Filter focusing on a certain area of the frame. Uses a @BoundingBox@
-- object to set the region of the screen.
startTextDetectionFilters_regionsOfInterest :: Lens.Lens' StartTextDetectionFilters (Core.Maybe [RegionOfInterest])
startTextDetectionFilters_regionsOfInterest = Lens.lens (\StartTextDetectionFilters' {regionsOfInterest} -> regionsOfInterest) (\s@StartTextDetectionFilters' {} a -> s {regionsOfInterest = a} :: StartTextDetectionFilters) Core.. Lens.mapping Lens._Coerce

-- | Filters focusing on qualities of the text, such as confidence or size.
startTextDetectionFilters_wordFilter :: Lens.Lens' StartTextDetectionFilters (Core.Maybe DetectionFilter)
startTextDetectionFilters_wordFilter = Lens.lens (\StartTextDetectionFilters' {wordFilter} -> wordFilter) (\s@StartTextDetectionFilters' {} a -> s {wordFilter = a} :: StartTextDetectionFilters)

instance Core.Hashable StartTextDetectionFilters

instance Core.NFData StartTextDetectionFilters

instance Core.ToJSON StartTextDetectionFilters where
  toJSON StartTextDetectionFilters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RegionsOfInterest" Core..=)
              Core.<$> regionsOfInterest,
            ("WordFilter" Core..=) Core.<$> wordFilter
          ]
      )
