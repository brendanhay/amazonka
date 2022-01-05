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
-- Module      : Amazonka.Location.Types.SearchForTextResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.SearchForTextResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Location.Types.Place
import qualified Amazonka.Prelude as Prelude

-- | Contains relevant Places returned by calling @SearchPlaceIndexForText@.
--
-- /See:/ 'newSearchForTextResult' smart constructor.
data SearchForTextResult = SearchForTextResult'
  { -- | Contains details about the relevant point of interest.
    place :: Place
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchForTextResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'place', 'searchForTextResult_place' - Contains details about the relevant point of interest.
newSearchForTextResult ::
  -- | 'place'
  Place ->
  SearchForTextResult
newSearchForTextResult pPlace_ =
  SearchForTextResult' {place = pPlace_}

-- | Contains details about the relevant point of interest.
searchForTextResult_place :: Lens.Lens' SearchForTextResult Place
searchForTextResult_place = Lens.lens (\SearchForTextResult' {place} -> place) (\s@SearchForTextResult' {} a -> s {place = a} :: SearchForTextResult)

instance Core.FromJSON SearchForTextResult where
  parseJSON =
    Core.withObject
      "SearchForTextResult"
      ( \x ->
          SearchForTextResult' Prelude.<$> (x Core..: "Place")
      )

instance Prelude.Hashable SearchForTextResult where
  hashWithSalt _salt SearchForTextResult' {..} =
    _salt `Prelude.hashWithSalt` place

instance Prelude.NFData SearchForTextResult where
  rnf SearchForTextResult' {..} = Prelude.rnf place
