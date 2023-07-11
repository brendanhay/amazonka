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
-- Module      : Amazonka.Support.Types.TrustedAdvisorCheckResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Types.TrustedAdvisorCheckResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Support.Types.TrustedAdvisorCategorySpecificSummary
import Amazonka.Support.Types.TrustedAdvisorResourceDetail
import Amazonka.Support.Types.TrustedAdvisorResourcesSummary

-- | The results of a Trusted Advisor check returned by
-- DescribeTrustedAdvisorCheckResult.
--
-- /See:/ 'newTrustedAdvisorCheckResult' smart constructor.
data TrustedAdvisorCheckResult = TrustedAdvisorCheckResult'
  { -- | The unique identifier for the Trusted Advisor check.
    checkId :: Prelude.Text,
    -- | The time of the last refresh of the check.
    timestamp :: Prelude.Text,
    -- | The alert status of the check: \"ok\" (green), \"warning\" (yellow),
    -- \"error\" (red), or \"not_available\".
    status :: Prelude.Text,
    resourcesSummary :: TrustedAdvisorResourcesSummary,
    -- | Summary information that relates to the category of the check. Cost
    -- Optimizing is the only category that is currently supported.
    categorySpecificSummary :: TrustedAdvisorCategorySpecificSummary,
    -- | The details about each resource listed in the check result.
    flaggedResources :: [TrustedAdvisorResourceDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrustedAdvisorCheckResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checkId', 'trustedAdvisorCheckResult_checkId' - The unique identifier for the Trusted Advisor check.
--
-- 'timestamp', 'trustedAdvisorCheckResult_timestamp' - The time of the last refresh of the check.
--
-- 'status', 'trustedAdvisorCheckResult_status' - The alert status of the check: \"ok\" (green), \"warning\" (yellow),
-- \"error\" (red), or \"not_available\".
--
-- 'resourcesSummary', 'trustedAdvisorCheckResult_resourcesSummary' - Undocumented member.
--
-- 'categorySpecificSummary', 'trustedAdvisorCheckResult_categorySpecificSummary' - Summary information that relates to the category of the check. Cost
-- Optimizing is the only category that is currently supported.
--
-- 'flaggedResources', 'trustedAdvisorCheckResult_flaggedResources' - The details about each resource listed in the check result.
newTrustedAdvisorCheckResult ::
  -- | 'checkId'
  Prelude.Text ->
  -- | 'timestamp'
  Prelude.Text ->
  -- | 'status'
  Prelude.Text ->
  -- | 'resourcesSummary'
  TrustedAdvisorResourcesSummary ->
  -- | 'categorySpecificSummary'
  TrustedAdvisorCategorySpecificSummary ->
  TrustedAdvisorCheckResult
newTrustedAdvisorCheckResult
  pCheckId_
  pTimestamp_
  pStatus_
  pResourcesSummary_
  pCategorySpecificSummary_ =
    TrustedAdvisorCheckResult'
      { checkId = pCheckId_,
        timestamp = pTimestamp_,
        status = pStatus_,
        resourcesSummary = pResourcesSummary_,
        categorySpecificSummary =
          pCategorySpecificSummary_,
        flaggedResources = Prelude.mempty
      }

-- | The unique identifier for the Trusted Advisor check.
trustedAdvisorCheckResult_checkId :: Lens.Lens' TrustedAdvisorCheckResult Prelude.Text
trustedAdvisorCheckResult_checkId = Lens.lens (\TrustedAdvisorCheckResult' {checkId} -> checkId) (\s@TrustedAdvisorCheckResult' {} a -> s {checkId = a} :: TrustedAdvisorCheckResult)

-- | The time of the last refresh of the check.
trustedAdvisorCheckResult_timestamp :: Lens.Lens' TrustedAdvisorCheckResult Prelude.Text
trustedAdvisorCheckResult_timestamp = Lens.lens (\TrustedAdvisorCheckResult' {timestamp} -> timestamp) (\s@TrustedAdvisorCheckResult' {} a -> s {timestamp = a} :: TrustedAdvisorCheckResult)

-- | The alert status of the check: \"ok\" (green), \"warning\" (yellow),
-- \"error\" (red), or \"not_available\".
trustedAdvisorCheckResult_status :: Lens.Lens' TrustedAdvisorCheckResult Prelude.Text
trustedAdvisorCheckResult_status = Lens.lens (\TrustedAdvisorCheckResult' {status} -> status) (\s@TrustedAdvisorCheckResult' {} a -> s {status = a} :: TrustedAdvisorCheckResult)

-- | Undocumented member.
trustedAdvisorCheckResult_resourcesSummary :: Lens.Lens' TrustedAdvisorCheckResult TrustedAdvisorResourcesSummary
trustedAdvisorCheckResult_resourcesSummary = Lens.lens (\TrustedAdvisorCheckResult' {resourcesSummary} -> resourcesSummary) (\s@TrustedAdvisorCheckResult' {} a -> s {resourcesSummary = a} :: TrustedAdvisorCheckResult)

-- | Summary information that relates to the category of the check. Cost
-- Optimizing is the only category that is currently supported.
trustedAdvisorCheckResult_categorySpecificSummary :: Lens.Lens' TrustedAdvisorCheckResult TrustedAdvisorCategorySpecificSummary
trustedAdvisorCheckResult_categorySpecificSummary = Lens.lens (\TrustedAdvisorCheckResult' {categorySpecificSummary} -> categorySpecificSummary) (\s@TrustedAdvisorCheckResult' {} a -> s {categorySpecificSummary = a} :: TrustedAdvisorCheckResult)

-- | The details about each resource listed in the check result.
trustedAdvisorCheckResult_flaggedResources :: Lens.Lens' TrustedAdvisorCheckResult [TrustedAdvisorResourceDetail]
trustedAdvisorCheckResult_flaggedResources = Lens.lens (\TrustedAdvisorCheckResult' {flaggedResources} -> flaggedResources) (\s@TrustedAdvisorCheckResult' {} a -> s {flaggedResources = a} :: TrustedAdvisorCheckResult) Prelude.. Lens.coerced

instance Data.FromJSON TrustedAdvisorCheckResult where
  parseJSON =
    Data.withObject
      "TrustedAdvisorCheckResult"
      ( \x ->
          TrustedAdvisorCheckResult'
            Prelude.<$> (x Data..: "checkId")
            Prelude.<*> (x Data..: "timestamp")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "resourcesSummary")
            Prelude.<*> (x Data..: "categorySpecificSummary")
            Prelude.<*> ( x
                            Data..:? "flaggedResources"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TrustedAdvisorCheckResult where
  hashWithSalt _salt TrustedAdvisorCheckResult' {..} =
    _salt
      `Prelude.hashWithSalt` checkId
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` resourcesSummary
      `Prelude.hashWithSalt` categorySpecificSummary
      `Prelude.hashWithSalt` flaggedResources

instance Prelude.NFData TrustedAdvisorCheckResult where
  rnf TrustedAdvisorCheckResult' {..} =
    Prelude.rnf checkId
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf resourcesSummary
      `Prelude.seq` Prelude.rnf categorySpecificSummary
      `Prelude.seq` Prelude.rnf flaggedResources
