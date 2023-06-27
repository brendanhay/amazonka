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
-- Module      : Amazonka.CleanRooms.Types.AnalysisRulePolicyV1
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.AnalysisRulePolicyV1 where

import Amazonka.CleanRooms.Types.AnalysisRuleAggregation
import Amazonka.CleanRooms.Types.AnalysisRuleList
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Controls on the query specifications that can be run on configured
-- table..
--
-- /See:/ 'newAnalysisRulePolicyV1' smart constructor.
data AnalysisRulePolicyV1 = AnalysisRulePolicyV1'
  { -- | Analysis rule type that enables only aggregation queries on a configured
    -- table.
    aggregation :: Prelude.Maybe AnalysisRuleAggregation,
    -- | Analysis rule type that enables only list queries on a configured table.
    list :: Prelude.Maybe AnalysisRuleList
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalysisRulePolicyV1' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregation', 'analysisRulePolicyV1_aggregation' - Analysis rule type that enables only aggregation queries on a configured
-- table.
--
-- 'list', 'analysisRulePolicyV1_list' - Analysis rule type that enables only list queries on a configured table.
newAnalysisRulePolicyV1 ::
  AnalysisRulePolicyV1
newAnalysisRulePolicyV1 =
  AnalysisRulePolicyV1'
    { aggregation =
        Prelude.Nothing,
      list = Prelude.Nothing
    }

-- | Analysis rule type that enables only aggregation queries on a configured
-- table.
analysisRulePolicyV1_aggregation :: Lens.Lens' AnalysisRulePolicyV1 (Prelude.Maybe AnalysisRuleAggregation)
analysisRulePolicyV1_aggregation = Lens.lens (\AnalysisRulePolicyV1' {aggregation} -> aggregation) (\s@AnalysisRulePolicyV1' {} a -> s {aggregation = a} :: AnalysisRulePolicyV1)

-- | Analysis rule type that enables only list queries on a configured table.
analysisRulePolicyV1_list :: Lens.Lens' AnalysisRulePolicyV1 (Prelude.Maybe AnalysisRuleList)
analysisRulePolicyV1_list = Lens.lens (\AnalysisRulePolicyV1' {list} -> list) (\s@AnalysisRulePolicyV1' {} a -> s {list = a} :: AnalysisRulePolicyV1)

instance Data.FromJSON AnalysisRulePolicyV1 where
  parseJSON =
    Data.withObject
      "AnalysisRulePolicyV1"
      ( \x ->
          AnalysisRulePolicyV1'
            Prelude.<$> (x Data..:? "aggregation")
            Prelude.<*> (x Data..:? "list")
      )

instance Prelude.Hashable AnalysisRulePolicyV1 where
  hashWithSalt _salt AnalysisRulePolicyV1' {..} =
    _salt
      `Prelude.hashWithSalt` aggregation
      `Prelude.hashWithSalt` list

instance Prelude.NFData AnalysisRulePolicyV1 where
  rnf AnalysisRulePolicyV1' {..} =
    Prelude.rnf aggregation
      `Prelude.seq` Prelude.rnf list
