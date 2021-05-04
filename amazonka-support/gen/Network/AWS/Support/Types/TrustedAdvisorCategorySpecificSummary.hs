{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Support.Types.TrustedAdvisorCategorySpecificSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.TrustedAdvisorCategorySpecificSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Support.Types.TrustedAdvisorCostOptimizingSummary

-- | The container for summary information that relates to the category of
-- the Trusted Advisor check.
--
-- /See:/ 'newTrustedAdvisorCategorySpecificSummary' smart constructor.
data TrustedAdvisorCategorySpecificSummary = TrustedAdvisorCategorySpecificSummary'
  { -- | The summary information about cost savings for a Trusted Advisor check
    -- that is in the Cost Optimizing category.
    costOptimizing :: Prelude.Maybe TrustedAdvisorCostOptimizingSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.FromJSON
    TrustedAdvisorCategorySpecificSummary
  where
  parseJSON =
    Prelude.withObject
      "TrustedAdvisorCategorySpecificSummary"
      ( \x ->
          TrustedAdvisorCategorySpecificSummary'
            Prelude.<$> (x Prelude..:? "costOptimizing")
      )

instance
  Prelude.Hashable
    TrustedAdvisorCategorySpecificSummary

instance
  Prelude.NFData
    TrustedAdvisorCategorySpecificSummary
