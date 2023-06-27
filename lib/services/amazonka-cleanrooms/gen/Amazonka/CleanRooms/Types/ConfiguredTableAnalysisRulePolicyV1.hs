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
-- Module      : Amazonka.CleanRooms.Types.ConfiguredTableAnalysisRulePolicyV1
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.ConfiguredTableAnalysisRulePolicyV1 where

import Amazonka.CleanRooms.Types.AnalysisRuleAggregation
import Amazonka.CleanRooms.Types.AnalysisRuleList
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Controls on the query specifications that can be run on a configured
-- table.
--
-- /See:/ 'newConfiguredTableAnalysisRulePolicyV1' smart constructor.
data ConfiguredTableAnalysisRulePolicyV1 = ConfiguredTableAnalysisRulePolicyV1'
  { -- | Analysis rule type that enables only aggregation queries on a configured
    -- table.
    aggregation :: Prelude.Maybe AnalysisRuleAggregation,
    -- | Analysis rule type that enables only list queries on a configured table.
    list :: Prelude.Maybe AnalysisRuleList
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfiguredTableAnalysisRulePolicyV1' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregation', 'configuredTableAnalysisRulePolicyV1_aggregation' - Analysis rule type that enables only aggregation queries on a configured
-- table.
--
-- 'list', 'configuredTableAnalysisRulePolicyV1_list' - Analysis rule type that enables only list queries on a configured table.
newConfiguredTableAnalysisRulePolicyV1 ::
  ConfiguredTableAnalysisRulePolicyV1
newConfiguredTableAnalysisRulePolicyV1 =
  ConfiguredTableAnalysisRulePolicyV1'
    { aggregation =
        Prelude.Nothing,
      list = Prelude.Nothing
    }

-- | Analysis rule type that enables only aggregation queries on a configured
-- table.
configuredTableAnalysisRulePolicyV1_aggregation :: Lens.Lens' ConfiguredTableAnalysisRulePolicyV1 (Prelude.Maybe AnalysisRuleAggregation)
configuredTableAnalysisRulePolicyV1_aggregation = Lens.lens (\ConfiguredTableAnalysisRulePolicyV1' {aggregation} -> aggregation) (\s@ConfiguredTableAnalysisRulePolicyV1' {} a -> s {aggregation = a} :: ConfiguredTableAnalysisRulePolicyV1)

-- | Analysis rule type that enables only list queries on a configured table.
configuredTableAnalysisRulePolicyV1_list :: Lens.Lens' ConfiguredTableAnalysisRulePolicyV1 (Prelude.Maybe AnalysisRuleList)
configuredTableAnalysisRulePolicyV1_list = Lens.lens (\ConfiguredTableAnalysisRulePolicyV1' {list} -> list) (\s@ConfiguredTableAnalysisRulePolicyV1' {} a -> s {list = a} :: ConfiguredTableAnalysisRulePolicyV1)

instance
  Data.FromJSON
    ConfiguredTableAnalysisRulePolicyV1
  where
  parseJSON =
    Data.withObject
      "ConfiguredTableAnalysisRulePolicyV1"
      ( \x ->
          ConfiguredTableAnalysisRulePolicyV1'
            Prelude.<$> (x Data..:? "aggregation")
            Prelude.<*> (x Data..:? "list")
      )

instance
  Prelude.Hashable
    ConfiguredTableAnalysisRulePolicyV1
  where
  hashWithSalt
    _salt
    ConfiguredTableAnalysisRulePolicyV1' {..} =
      _salt
        `Prelude.hashWithSalt` aggregation
        `Prelude.hashWithSalt` list

instance
  Prelude.NFData
    ConfiguredTableAnalysisRulePolicyV1
  where
  rnf ConfiguredTableAnalysisRulePolicyV1' {..} =
    Prelude.rnf aggregation
      `Prelude.seq` Prelude.rnf list

instance
  Data.ToJSON
    ConfiguredTableAnalysisRulePolicyV1
  where
  toJSON ConfiguredTableAnalysisRulePolicyV1' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("aggregation" Data..=) Prelude.<$> aggregation,
            ("list" Data..=) Prelude.<$> list
          ]
      )
