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
-- Module      : Amazonka.Support.Types.TrustedAdvisorResourcesSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Types.TrustedAdvisorResourcesSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about Amazon Web Services resources that were analyzed in a call
-- to Trusted Advisor DescribeTrustedAdvisorCheckSummaries.
--
-- /See:/ 'newTrustedAdvisorResourcesSummary' smart constructor.
data TrustedAdvisorResourcesSummary = TrustedAdvisorResourcesSummary'
  { -- | The number of Amazon Web Services resources that were analyzed by the
    -- Trusted Advisor check.
    resourcesProcessed :: Prelude.Integer,
    -- | The number of Amazon Web Services resources that were flagged (listed)
    -- by the Trusted Advisor check.
    resourcesFlagged :: Prelude.Integer,
    -- | The number of Amazon Web Services resources ignored by Trusted Advisor
    -- because information was unavailable.
    resourcesIgnored :: Prelude.Integer,
    -- | The number of Amazon Web Services resources ignored by Trusted Advisor
    -- because they were marked as suppressed by the user.
    resourcesSuppressed :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrustedAdvisorResourcesSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourcesProcessed', 'trustedAdvisorResourcesSummary_resourcesProcessed' - The number of Amazon Web Services resources that were analyzed by the
-- Trusted Advisor check.
--
-- 'resourcesFlagged', 'trustedAdvisorResourcesSummary_resourcesFlagged' - The number of Amazon Web Services resources that were flagged (listed)
-- by the Trusted Advisor check.
--
-- 'resourcesIgnored', 'trustedAdvisorResourcesSummary_resourcesIgnored' - The number of Amazon Web Services resources ignored by Trusted Advisor
-- because information was unavailable.
--
-- 'resourcesSuppressed', 'trustedAdvisorResourcesSummary_resourcesSuppressed' - The number of Amazon Web Services resources ignored by Trusted Advisor
-- because they were marked as suppressed by the user.
newTrustedAdvisorResourcesSummary ::
  -- | 'resourcesProcessed'
  Prelude.Integer ->
  -- | 'resourcesFlagged'
  Prelude.Integer ->
  -- | 'resourcesIgnored'
  Prelude.Integer ->
  -- | 'resourcesSuppressed'
  Prelude.Integer ->
  TrustedAdvisorResourcesSummary
newTrustedAdvisorResourcesSummary
  pResourcesProcessed_
  pResourcesFlagged_
  pResourcesIgnored_
  pResourcesSuppressed_ =
    TrustedAdvisorResourcesSummary'
      { resourcesProcessed =
          pResourcesProcessed_,
        resourcesFlagged = pResourcesFlagged_,
        resourcesIgnored = pResourcesIgnored_,
        resourcesSuppressed = pResourcesSuppressed_
      }

-- | The number of Amazon Web Services resources that were analyzed by the
-- Trusted Advisor check.
trustedAdvisorResourcesSummary_resourcesProcessed :: Lens.Lens' TrustedAdvisorResourcesSummary Prelude.Integer
trustedAdvisorResourcesSummary_resourcesProcessed = Lens.lens (\TrustedAdvisorResourcesSummary' {resourcesProcessed} -> resourcesProcessed) (\s@TrustedAdvisorResourcesSummary' {} a -> s {resourcesProcessed = a} :: TrustedAdvisorResourcesSummary)

-- | The number of Amazon Web Services resources that were flagged (listed)
-- by the Trusted Advisor check.
trustedAdvisorResourcesSummary_resourcesFlagged :: Lens.Lens' TrustedAdvisorResourcesSummary Prelude.Integer
trustedAdvisorResourcesSummary_resourcesFlagged = Lens.lens (\TrustedAdvisorResourcesSummary' {resourcesFlagged} -> resourcesFlagged) (\s@TrustedAdvisorResourcesSummary' {} a -> s {resourcesFlagged = a} :: TrustedAdvisorResourcesSummary)

-- | The number of Amazon Web Services resources ignored by Trusted Advisor
-- because information was unavailable.
trustedAdvisorResourcesSummary_resourcesIgnored :: Lens.Lens' TrustedAdvisorResourcesSummary Prelude.Integer
trustedAdvisorResourcesSummary_resourcesIgnored = Lens.lens (\TrustedAdvisorResourcesSummary' {resourcesIgnored} -> resourcesIgnored) (\s@TrustedAdvisorResourcesSummary' {} a -> s {resourcesIgnored = a} :: TrustedAdvisorResourcesSummary)

-- | The number of Amazon Web Services resources ignored by Trusted Advisor
-- because they were marked as suppressed by the user.
trustedAdvisorResourcesSummary_resourcesSuppressed :: Lens.Lens' TrustedAdvisorResourcesSummary Prelude.Integer
trustedAdvisorResourcesSummary_resourcesSuppressed = Lens.lens (\TrustedAdvisorResourcesSummary' {resourcesSuppressed} -> resourcesSuppressed) (\s@TrustedAdvisorResourcesSummary' {} a -> s {resourcesSuppressed = a} :: TrustedAdvisorResourcesSummary)

instance Data.FromJSON TrustedAdvisorResourcesSummary where
  parseJSON =
    Data.withObject
      "TrustedAdvisorResourcesSummary"
      ( \x ->
          TrustedAdvisorResourcesSummary'
            Prelude.<$> (x Data..: "resourcesProcessed")
            Prelude.<*> (x Data..: "resourcesFlagged")
            Prelude.<*> (x Data..: "resourcesIgnored")
            Prelude.<*> (x Data..: "resourcesSuppressed")
      )

instance
  Prelude.Hashable
    TrustedAdvisorResourcesSummary
  where
  hashWithSalt
    _salt
    TrustedAdvisorResourcesSummary' {..} =
      _salt
        `Prelude.hashWithSalt` resourcesProcessed
        `Prelude.hashWithSalt` resourcesFlagged
        `Prelude.hashWithSalt` resourcesIgnored
        `Prelude.hashWithSalt` resourcesSuppressed

instance
  Prelude.NFData
    TrustedAdvisorResourcesSummary
  where
  rnf TrustedAdvisorResourcesSummary' {..} =
    Prelude.rnf resourcesProcessed `Prelude.seq`
      Prelude.rnf resourcesFlagged `Prelude.seq`
        Prelude.rnf resourcesIgnored `Prelude.seq`
          Prelude.rnf resourcesSuppressed
