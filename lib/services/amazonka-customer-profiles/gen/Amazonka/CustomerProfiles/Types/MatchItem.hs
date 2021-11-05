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
-- Module      : Amazonka.CustomerProfiles.Types.MatchItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.MatchItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Match group object.
--
-- /See:/ 'newMatchItem' smart constructor.
data MatchItem = MatchItem'
  { -- | A list of identifiers for profiles that match.
    profileIds :: Prelude.Maybe [Prelude.Text],
    -- | The unique identifiers for this group of profiles that match.
    matchId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MatchItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileIds', 'matchItem_profileIds' - A list of identifiers for profiles that match.
--
-- 'matchId', 'matchItem_matchId' - The unique identifiers for this group of profiles that match.
newMatchItem ::
  MatchItem
newMatchItem =
  MatchItem'
    { profileIds = Prelude.Nothing,
      matchId = Prelude.Nothing
    }

-- | A list of identifiers for profiles that match.
matchItem_profileIds :: Lens.Lens' MatchItem (Prelude.Maybe [Prelude.Text])
matchItem_profileIds = Lens.lens (\MatchItem' {profileIds} -> profileIds) (\s@MatchItem' {} a -> s {profileIds = a} :: MatchItem) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifiers for this group of profiles that match.
matchItem_matchId :: Lens.Lens' MatchItem (Prelude.Maybe Prelude.Text)
matchItem_matchId = Lens.lens (\MatchItem' {matchId} -> matchId) (\s@MatchItem' {} a -> s {matchId = a} :: MatchItem)

instance Core.FromJSON MatchItem where
  parseJSON =
    Core.withObject
      "MatchItem"
      ( \x ->
          MatchItem'
            Prelude.<$> (x Core..:? "ProfileIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "MatchId")
      )

instance Prelude.Hashable MatchItem

instance Prelude.NFData MatchItem
