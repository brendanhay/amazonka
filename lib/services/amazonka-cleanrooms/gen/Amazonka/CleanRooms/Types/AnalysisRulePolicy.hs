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
-- Module      : Amazonka.CleanRooms.Types.AnalysisRulePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.AnalysisRulePolicy where

import Amazonka.CleanRooms.Types.AnalysisRulePolicyV1
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Controls on the query specifications that can be run on configured
-- table..
--
-- /See:/ 'newAnalysisRulePolicy' smart constructor.
data AnalysisRulePolicy = AnalysisRulePolicy'
  { -- | Controls on the query specifications that can be run on configured
    -- table..
    v1 :: Prelude.Maybe AnalysisRulePolicyV1
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalysisRulePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'v1', 'analysisRulePolicy_v1' - Controls on the query specifications that can be run on configured
-- table..
newAnalysisRulePolicy ::
  AnalysisRulePolicy
newAnalysisRulePolicy =
  AnalysisRulePolicy' {v1 = Prelude.Nothing}

-- | Controls on the query specifications that can be run on configured
-- table..
analysisRulePolicy_v1 :: Lens.Lens' AnalysisRulePolicy (Prelude.Maybe AnalysisRulePolicyV1)
analysisRulePolicy_v1 = Lens.lens (\AnalysisRulePolicy' {v1} -> v1) (\s@AnalysisRulePolicy' {} a -> s {v1 = a} :: AnalysisRulePolicy)

instance Data.FromJSON AnalysisRulePolicy where
  parseJSON =
    Data.withObject
      "AnalysisRulePolicy"
      ( \x ->
          AnalysisRulePolicy' Prelude.<$> (x Data..:? "v1")
      )

instance Prelude.Hashable AnalysisRulePolicy where
  hashWithSalt _salt AnalysisRulePolicy' {..} =
    _salt `Prelude.hashWithSalt` v1

instance Prelude.NFData AnalysisRulePolicy where
  rnf AnalysisRulePolicy' {..} = Prelude.rnf v1
