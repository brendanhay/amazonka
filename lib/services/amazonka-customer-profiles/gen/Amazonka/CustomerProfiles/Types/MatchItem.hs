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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.MatchItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Match group object.
--
-- /See:/ 'newMatchItem' smart constructor.
data MatchItem = MatchItem'
  { -- | A number between 0 and 1, where a higher score means higher similarity.
    -- Examining match confidence scores lets you distinguish between groups of
    -- similar records in which the system is highly confident (which you may
    -- decide to merge), groups of similar records about which the system is
    -- uncertain (which you may decide to have reviewed by a human), and groups
    -- of similar records that the system deems to be unlikely (which you may
    -- decide to reject). Given confidence scores vary as per the data input,
    -- it should not be used an absolute measure of matching quality.
    confidenceScore :: Prelude.Maybe Prelude.Double,
    -- | The unique identifiers for this group of profiles that match.
    matchId :: Prelude.Maybe Prelude.Text,
    -- | A list of identifiers for profiles that match.
    profileIds :: Prelude.Maybe [Prelude.Text]
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
-- 'confidenceScore', 'matchItem_confidenceScore' - A number between 0 and 1, where a higher score means higher similarity.
-- Examining match confidence scores lets you distinguish between groups of
-- similar records in which the system is highly confident (which you may
-- decide to merge), groups of similar records about which the system is
-- uncertain (which you may decide to have reviewed by a human), and groups
-- of similar records that the system deems to be unlikely (which you may
-- decide to reject). Given confidence scores vary as per the data input,
-- it should not be used an absolute measure of matching quality.
--
-- 'matchId', 'matchItem_matchId' - The unique identifiers for this group of profiles that match.
--
-- 'profileIds', 'matchItem_profileIds' - A list of identifiers for profiles that match.
newMatchItem ::
  MatchItem
newMatchItem =
  MatchItem'
    { confidenceScore = Prelude.Nothing,
      matchId = Prelude.Nothing,
      profileIds = Prelude.Nothing
    }

-- | A number between 0 and 1, where a higher score means higher similarity.
-- Examining match confidence scores lets you distinguish between groups of
-- similar records in which the system is highly confident (which you may
-- decide to merge), groups of similar records about which the system is
-- uncertain (which you may decide to have reviewed by a human), and groups
-- of similar records that the system deems to be unlikely (which you may
-- decide to reject). Given confidence scores vary as per the data input,
-- it should not be used an absolute measure of matching quality.
matchItem_confidenceScore :: Lens.Lens' MatchItem (Prelude.Maybe Prelude.Double)
matchItem_confidenceScore = Lens.lens (\MatchItem' {confidenceScore} -> confidenceScore) (\s@MatchItem' {} a -> s {confidenceScore = a} :: MatchItem)

-- | The unique identifiers for this group of profiles that match.
matchItem_matchId :: Lens.Lens' MatchItem (Prelude.Maybe Prelude.Text)
matchItem_matchId = Lens.lens (\MatchItem' {matchId} -> matchId) (\s@MatchItem' {} a -> s {matchId = a} :: MatchItem)

-- | A list of identifiers for profiles that match.
matchItem_profileIds :: Lens.Lens' MatchItem (Prelude.Maybe [Prelude.Text])
matchItem_profileIds = Lens.lens (\MatchItem' {profileIds} -> profileIds) (\s@MatchItem' {} a -> s {profileIds = a} :: MatchItem) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON MatchItem where
  parseJSON =
    Data.withObject
      "MatchItem"
      ( \x ->
          MatchItem'
            Prelude.<$> (x Data..:? "ConfidenceScore")
            Prelude.<*> (x Data..:? "MatchId")
            Prelude.<*> (x Data..:? "ProfileIds" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable MatchItem where
  hashWithSalt _salt MatchItem' {..} =
    _salt
      `Prelude.hashWithSalt` confidenceScore
      `Prelude.hashWithSalt` matchId
      `Prelude.hashWithSalt` profileIds

instance Prelude.NFData MatchItem where
  rnf MatchItem' {..} =
    Prelude.rnf confidenceScore
      `Prelude.seq` Prelude.rnf matchId
      `Prelude.seq` Prelude.rnf profileIds
