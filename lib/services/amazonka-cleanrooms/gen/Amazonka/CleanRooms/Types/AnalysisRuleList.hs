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
-- Module      : Amazonka.CleanRooms.Types.AnalysisRuleList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.AnalysisRuleList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A type of analysis rule that enables row-level analysis.
--
-- /See:/ 'newAnalysisRuleList' smart constructor.
data AnalysisRuleList = AnalysisRuleList'
  { -- | Columns that can be used to join a configured table with the table of
    -- the member who can query and another members\' configured tables.
    joinColumns :: Prelude.NonEmpty Prelude.Text,
    -- | Columns that can be listed in the output.
    listColumns :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalysisRuleList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'joinColumns', 'analysisRuleList_joinColumns' - Columns that can be used to join a configured table with the table of
-- the member who can query and another members\' configured tables.
--
-- 'listColumns', 'analysisRuleList_listColumns' - Columns that can be listed in the output.
newAnalysisRuleList ::
  -- | 'joinColumns'
  Prelude.NonEmpty Prelude.Text ->
  AnalysisRuleList
newAnalysisRuleList pJoinColumns_ =
  AnalysisRuleList'
    { joinColumns =
        Lens.coerced Lens.# pJoinColumns_,
      listColumns = Prelude.mempty
    }

-- | Columns that can be used to join a configured table with the table of
-- the member who can query and another members\' configured tables.
analysisRuleList_joinColumns :: Lens.Lens' AnalysisRuleList (Prelude.NonEmpty Prelude.Text)
analysisRuleList_joinColumns = Lens.lens (\AnalysisRuleList' {joinColumns} -> joinColumns) (\s@AnalysisRuleList' {} a -> s {joinColumns = a} :: AnalysisRuleList) Prelude.. Lens.coerced

-- | Columns that can be listed in the output.
analysisRuleList_listColumns :: Lens.Lens' AnalysisRuleList [Prelude.Text]
analysisRuleList_listColumns = Lens.lens (\AnalysisRuleList' {listColumns} -> listColumns) (\s@AnalysisRuleList' {} a -> s {listColumns = a} :: AnalysisRuleList) Prelude.. Lens.coerced

instance Data.FromJSON AnalysisRuleList where
  parseJSON =
    Data.withObject
      "AnalysisRuleList"
      ( \x ->
          AnalysisRuleList'
            Prelude.<$> (x Data..: "joinColumns")
            Prelude.<*> (x Data..:? "listColumns" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AnalysisRuleList where
  hashWithSalt _salt AnalysisRuleList' {..} =
    _salt
      `Prelude.hashWithSalt` joinColumns
      `Prelude.hashWithSalt` listColumns

instance Prelude.NFData AnalysisRuleList where
  rnf AnalysisRuleList' {..} =
    Prelude.rnf joinColumns
      `Prelude.seq` Prelude.rnf listColumns

instance Data.ToJSON AnalysisRuleList where
  toJSON AnalysisRuleList' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("joinColumns" Data..= joinColumns),
            Prelude.Just ("listColumns" Data..= listColumns)
          ]
      )
