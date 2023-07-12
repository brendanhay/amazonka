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
-- Module      : Amazonka.ConnectContactLens.Types.CategoryDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectContactLens.Types.CategoryDetails where

import Amazonka.ConnectContactLens.Types.PointOfInterest
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON CategoryDetails where
  parseJSON =
    Data.withObject
      "CategoryDetails"
      ( \x ->
          CategoryDetails'
            Prelude.<$> ( x
                            Data..:? "PointsOfInterest"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CategoryDetails where
  hashWithSalt _salt CategoryDetails' {..} =
    _salt `Prelude.hashWithSalt` pointsOfInterest

instance Prelude.NFData CategoryDetails where
  rnf CategoryDetails' {..} =
    Prelude.rnf pointsOfInterest
