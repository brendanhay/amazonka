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
-- Module      : Network.AWS.ConnectContactLens.Types.CategoryDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ConnectContactLens.Types.CategoryDetails where

import Network.AWS.ConnectContactLens.Types.PointOfInterest
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about the category rule that was matched.
--
-- /See:/ 'newCategoryDetails' smart constructor.
data CategoryDetails = CategoryDetails'
  { -- | The section of audio where the category rule was detected.
    pointsOfInterest :: [PointOfInterest]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CategoryDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pointsOfInterest', 'categoryDetails_pointsOfInterest' - The section of audio where the category rule was detected.
newCategoryDetails ::
  CategoryDetails
newCategoryDetails =
  CategoryDetails' {pointsOfInterest = Prelude.mempty}

-- | The section of audio where the category rule was detected.
categoryDetails_pointsOfInterest :: Lens.Lens' CategoryDetails [PointOfInterest]
categoryDetails_pointsOfInterest = Lens.lens (\CategoryDetails' {pointsOfInterest} -> pointsOfInterest) (\s@CategoryDetails' {} a -> s {pointsOfInterest = a} :: CategoryDetails) Prelude.. Lens.coerced

instance Core.FromJSON CategoryDetails where
  parseJSON =
    Core.withObject
      "CategoryDetails"
      ( \x ->
          CategoryDetails'
            Prelude.<$> ( x Core..:? "PointsOfInterest"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CategoryDetails

instance Prelude.NFData CategoryDetails
