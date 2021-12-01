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
-- Module      : Amazonka.Location.Types.SearchForPositionResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.SearchForPositionResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Location.Types.Place
import qualified Amazonka.Prelude as Prelude

-- | Specifies a single point of interest, or Place as a result of a search
-- query obtained from a dataset configured in the place index resource.
--
-- /See:/ 'newSearchForPositionResult' smart constructor.
data SearchForPositionResult = SearchForPositionResult'
  { -- | Contains details about the relevant point of interest.
    place :: Place
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchForPositionResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'place', 'searchForPositionResult_place' - Contains details about the relevant point of interest.
newSearchForPositionResult ::
  -- | 'place'
  Place ->
  SearchForPositionResult
newSearchForPositionResult pPlace_ =
  SearchForPositionResult' {place = pPlace_}

-- | Contains details about the relevant point of interest.
searchForPositionResult_place :: Lens.Lens' SearchForPositionResult Place
searchForPositionResult_place = Lens.lens (\SearchForPositionResult' {place} -> place) (\s@SearchForPositionResult' {} a -> s {place = a} :: SearchForPositionResult)

instance Core.FromJSON SearchForPositionResult where
  parseJSON =
    Core.withObject
      "SearchForPositionResult"
      ( \x ->
          SearchForPositionResult'
            Prelude.<$> (x Core..: "Place")
      )

instance Prelude.Hashable SearchForPositionResult where
  hashWithSalt salt' SearchForPositionResult' {..} =
    salt' `Prelude.hashWithSalt` place

instance Prelude.NFData SearchForPositionResult where
  rnf SearchForPositionResult' {..} = Prelude.rnf place
