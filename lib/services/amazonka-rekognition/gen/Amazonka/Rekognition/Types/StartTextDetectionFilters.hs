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
-- Module      : Amazonka.Rekognition.Types.StartTextDetectionFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.StartTextDetectionFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.DetectionFilter
import Amazonka.Rekognition.Types.RegionOfInterest

-- | Set of optional parameters that let you set the criteria text must meet
-- to be included in your response. @WordFilter@ looks at a word\'s height,
-- width and minimum confidence. @RegionOfInterest@ lets you set a specific
-- region of the screen to look for text in.
--
-- /See:/ 'newStartTextDetectionFilters' smart constructor.
data StartTextDetectionFilters = StartTextDetectionFilters'
  { -- | Filter focusing on a certain area of the frame. Uses a @BoundingBox@
    -- object to set the region of the screen.
    regionsOfInterest :: Prelude.Maybe [RegionOfInterest],
    -- | Filters focusing on qualities of the text, such as confidence or size.
    wordFilter :: Prelude.Maybe DetectionFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      wordFilter = Prelude.Nothing
    }

-- | Filter focusing on a certain area of the frame. Uses a @BoundingBox@
-- object to set the region of the screen.
startTextDetectionFilters_regionsOfInterest :: Lens.Lens' StartTextDetectionFilters (Prelude.Maybe [RegionOfInterest])
startTextDetectionFilters_regionsOfInterest = Lens.lens (\StartTextDetectionFilters' {regionsOfInterest} -> regionsOfInterest) (\s@StartTextDetectionFilters' {} a -> s {regionsOfInterest = a} :: StartTextDetectionFilters) Prelude.. Lens.mapping Lens.coerced

-- | Filters focusing on qualities of the text, such as confidence or size.
startTextDetectionFilters_wordFilter :: Lens.Lens' StartTextDetectionFilters (Prelude.Maybe DetectionFilter)
startTextDetectionFilters_wordFilter = Lens.lens (\StartTextDetectionFilters' {wordFilter} -> wordFilter) (\s@StartTextDetectionFilters' {} a -> s {wordFilter = a} :: StartTextDetectionFilters)

instance Prelude.Hashable StartTextDetectionFilters where
  hashWithSalt _salt StartTextDetectionFilters' {..} =
    _salt
      `Prelude.hashWithSalt` regionsOfInterest
      `Prelude.hashWithSalt` wordFilter

instance Prelude.NFData StartTextDetectionFilters where
  rnf StartTextDetectionFilters' {..} =
    Prelude.rnf regionsOfInterest `Prelude.seq`
      Prelude.rnf wordFilter

instance Data.ToJSON StartTextDetectionFilters where
  toJSON StartTextDetectionFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RegionsOfInterest" Data..=)
              Prelude.<$> regionsOfInterest,
            ("WordFilter" Data..=) Prelude.<$> wordFilter
          ]
      )
