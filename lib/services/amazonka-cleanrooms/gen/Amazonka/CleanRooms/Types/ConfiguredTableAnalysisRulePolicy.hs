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
-- Module      : Amazonka.CleanRooms.Types.ConfiguredTableAnalysisRulePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.ConfiguredTableAnalysisRulePolicy where

import Amazonka.CleanRooms.Types.ConfiguredTableAnalysisRulePolicyV1
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Controls on the query specifications that can be run on a configured
-- table.
--
-- /See:/ 'newConfiguredTableAnalysisRulePolicy' smart constructor.
data ConfiguredTableAnalysisRulePolicy = ConfiguredTableAnalysisRulePolicy'
  { -- | Controls on the query specifications that can be run on a configured
    -- table.
    v1 :: Prelude.Maybe ConfiguredTableAnalysisRulePolicyV1
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfiguredTableAnalysisRulePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'v1', 'configuredTableAnalysisRulePolicy_v1' - Controls on the query specifications that can be run on a configured
-- table.
newConfiguredTableAnalysisRulePolicy ::
  ConfiguredTableAnalysisRulePolicy
newConfiguredTableAnalysisRulePolicy =
  ConfiguredTableAnalysisRulePolicy'
    { v1 =
        Prelude.Nothing
    }

-- | Controls on the query specifications that can be run on a configured
-- table.
configuredTableAnalysisRulePolicy_v1 :: Lens.Lens' ConfiguredTableAnalysisRulePolicy (Prelude.Maybe ConfiguredTableAnalysisRulePolicyV1)
configuredTableAnalysisRulePolicy_v1 = Lens.lens (\ConfiguredTableAnalysisRulePolicy' {v1} -> v1) (\s@ConfiguredTableAnalysisRulePolicy' {} a -> s {v1 = a} :: ConfiguredTableAnalysisRulePolicy)

instance
  Data.FromJSON
    ConfiguredTableAnalysisRulePolicy
  where
  parseJSON =
    Data.withObject
      "ConfiguredTableAnalysisRulePolicy"
      ( \x ->
          ConfiguredTableAnalysisRulePolicy'
            Prelude.<$> (x Data..:? "v1")
      )

instance
  Prelude.Hashable
    ConfiguredTableAnalysisRulePolicy
  where
  hashWithSalt
    _salt
    ConfiguredTableAnalysisRulePolicy' {..} =
      _salt `Prelude.hashWithSalt` v1

instance
  Prelude.NFData
    ConfiguredTableAnalysisRulePolicy
  where
  rnf ConfiguredTableAnalysisRulePolicy' {..} =
    Prelude.rnf v1

instance
  Data.ToJSON
    ConfiguredTableAnalysisRulePolicy
  where
  toJSON ConfiguredTableAnalysisRulePolicy' {..} =
    Data.object
      (Prelude.catMaybes [("v1" Data..=) Prelude.<$> v1])
