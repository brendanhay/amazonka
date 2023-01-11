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
-- Module      : Amazonka.Support.Types.TrustedAdvisorCategorySpecificSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Types.TrustedAdvisorCategorySpecificSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Support.Types.TrustedAdvisorCostOptimizingSummary

-- | The container for summary information that relates to the category of
-- the Trusted Advisor check.
--
-- /See:/ 'newTrustedAdvisorCategorySpecificSummary' smart constructor.
data TrustedAdvisorCategorySpecificSummary = TrustedAdvisorCategorySpecificSummary'
  { -- | The summary information about cost savings for a Trusted Advisor check
    -- that is in the Cost Optimizing category.
    costOptimizing :: Prelude.Maybe TrustedAdvisorCostOptimizingSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrustedAdvisorCategorySpecificSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'costOptimizing', 'trustedAdvisorCategorySpecificSummary_costOptimizing' - The summary information about cost savings for a Trusted Advisor check
-- that is in the Cost Optimizing category.
newTrustedAdvisorCategorySpecificSummary ::
  TrustedAdvisorCategorySpecificSummary
newTrustedAdvisorCategorySpecificSummary =
  TrustedAdvisorCategorySpecificSummary'
    { costOptimizing =
        Prelude.Nothing
    }

-- | The summary information about cost savings for a Trusted Advisor check
-- that is in the Cost Optimizing category.
trustedAdvisorCategorySpecificSummary_costOptimizing :: Lens.Lens' TrustedAdvisorCategorySpecificSummary (Prelude.Maybe TrustedAdvisorCostOptimizingSummary)
trustedAdvisorCategorySpecificSummary_costOptimizing = Lens.lens (\TrustedAdvisorCategorySpecificSummary' {costOptimizing} -> costOptimizing) (\s@TrustedAdvisorCategorySpecificSummary' {} a -> s {costOptimizing = a} :: TrustedAdvisorCategorySpecificSummary)

instance
  Data.FromJSON
    TrustedAdvisorCategorySpecificSummary
  where
  parseJSON =
    Data.withObject
      "TrustedAdvisorCategorySpecificSummary"
      ( \x ->
          TrustedAdvisorCategorySpecificSummary'
            Prelude.<$> (x Data..:? "costOptimizing")
      )

instance
  Prelude.Hashable
    TrustedAdvisorCategorySpecificSummary
  where
  hashWithSalt
    _salt
    TrustedAdvisorCategorySpecificSummary' {..} =
      _salt `Prelude.hashWithSalt` costOptimizing

instance
  Prelude.NFData
    TrustedAdvisorCategorySpecificSummary
  where
  rnf TrustedAdvisorCategorySpecificSummary' {..} =
    Prelude.rnf costOptimizing
