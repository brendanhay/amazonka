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
-- Module      : Network.AWS.Support.Types.TrustedAdvisorResourcesSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.TrustedAdvisorResourcesSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about AWS resources that were analyzed in a call to Trusted
-- Advisor DescribeTrustedAdvisorCheckSummaries.
--
-- /See:/ 'newTrustedAdvisorResourcesSummary' smart constructor.
data TrustedAdvisorResourcesSummary = TrustedAdvisorResourcesSummary'
  { -- | The number of AWS resources that were analyzed by the Trusted Advisor
    -- check.
    resourcesProcessed :: Prelude.Integer,
    -- | The number of AWS resources that were flagged (listed) by the Trusted
    -- Advisor check.
    resourcesFlagged :: Prelude.Integer,
    -- | The number of AWS resources ignored by Trusted Advisor because
    -- information was unavailable.
    resourcesIgnored :: Prelude.Integer,
    -- | The number of AWS resources ignored by Trusted Advisor because they were
    -- marked as suppressed by the user.
    resourcesSuppressed :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TrustedAdvisorResourcesSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourcesProcessed', 'trustedAdvisorResourcesSummary_resourcesProcessed' - The number of AWS resources that were analyzed by the Trusted Advisor
-- check.
--
-- 'resourcesFlagged', 'trustedAdvisorResourcesSummary_resourcesFlagged' - The number of AWS resources that were flagged (listed) by the Trusted
-- Advisor check.
--
-- 'resourcesIgnored', 'trustedAdvisorResourcesSummary_resourcesIgnored' - The number of AWS resources ignored by Trusted Advisor because
-- information was unavailable.
--
-- 'resourcesSuppressed', 'trustedAdvisorResourcesSummary_resourcesSuppressed' - The number of AWS resources ignored by Trusted Advisor because they were
-- marked as suppressed by the user.
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

-- | The number of AWS resources that were analyzed by the Trusted Advisor
-- check.
trustedAdvisorResourcesSummary_resourcesProcessed :: Lens.Lens' TrustedAdvisorResourcesSummary Prelude.Integer
trustedAdvisorResourcesSummary_resourcesProcessed = Lens.lens (\TrustedAdvisorResourcesSummary' {resourcesProcessed} -> resourcesProcessed) (\s@TrustedAdvisorResourcesSummary' {} a -> s {resourcesProcessed = a} :: TrustedAdvisorResourcesSummary)

-- | The number of AWS resources that were flagged (listed) by the Trusted
-- Advisor check.
trustedAdvisorResourcesSummary_resourcesFlagged :: Lens.Lens' TrustedAdvisorResourcesSummary Prelude.Integer
trustedAdvisorResourcesSummary_resourcesFlagged = Lens.lens (\TrustedAdvisorResourcesSummary' {resourcesFlagged} -> resourcesFlagged) (\s@TrustedAdvisorResourcesSummary' {} a -> s {resourcesFlagged = a} :: TrustedAdvisorResourcesSummary)

-- | The number of AWS resources ignored by Trusted Advisor because
-- information was unavailable.
trustedAdvisorResourcesSummary_resourcesIgnored :: Lens.Lens' TrustedAdvisorResourcesSummary Prelude.Integer
trustedAdvisorResourcesSummary_resourcesIgnored = Lens.lens (\TrustedAdvisorResourcesSummary' {resourcesIgnored} -> resourcesIgnored) (\s@TrustedAdvisorResourcesSummary' {} a -> s {resourcesIgnored = a} :: TrustedAdvisorResourcesSummary)

-- | The number of AWS resources ignored by Trusted Advisor because they were
-- marked as suppressed by the user.
trustedAdvisorResourcesSummary_resourcesSuppressed :: Lens.Lens' TrustedAdvisorResourcesSummary Prelude.Integer
trustedAdvisorResourcesSummary_resourcesSuppressed = Lens.lens (\TrustedAdvisorResourcesSummary' {resourcesSuppressed} -> resourcesSuppressed) (\s@TrustedAdvisorResourcesSummary' {} a -> s {resourcesSuppressed = a} :: TrustedAdvisorResourcesSummary)

instance
  Prelude.FromJSON
    TrustedAdvisorResourcesSummary
  where
  parseJSON =
    Prelude.withObject
      "TrustedAdvisorResourcesSummary"
      ( \x ->
          TrustedAdvisorResourcesSummary'
            Prelude.<$> (x Prelude..: "resourcesProcessed")
            Prelude.<*> (x Prelude..: "resourcesFlagged")
            Prelude.<*> (x Prelude..: "resourcesIgnored")
            Prelude.<*> (x Prelude..: "resourcesSuppressed")
      )

instance
  Prelude.Hashable
    TrustedAdvisorResourcesSummary

instance
  Prelude.NFData
    TrustedAdvisorResourcesSummary
