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
-- Module      : Amazonka.MacieV2.Types.UsageRecord
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.UsageRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.UsageByAccount
import qualified Amazonka.Prelude as Prelude

-- | Provides quota and aggregated usage data for an Amazon Macie account.
--
-- /See:/ 'newUsageRecord' smart constructor.
data UsageRecord = UsageRecord'
  { -- | The unique identifier for the Amazon Web Services account that the data
    -- applies to.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in UTC and extended ISO 8601 format, when the free
    -- trial of automated sensitive data discovery started for the account. If
    -- the account is a member account in an organization, this value is the
    -- same as the value for the organization\'s Amazon Macie administrator
    -- account.
    automatedDiscoveryFreeTrialStartDate :: Prelude.Maybe Data.ISO8601,
    -- | The date and time, in UTC and extended ISO 8601 format, when the Amazon
    -- Macie free trial started for the account.
    freeTrialStartDate :: Prelude.Maybe Data.ISO8601,
    -- | An array of objects that contains usage data and quotas for the account.
    -- Each object contains the data for a specific usage metric and the
    -- corresponding quota.
    usage :: Prelude.Maybe [UsageByAccount]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UsageRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'usageRecord_accountId' - The unique identifier for the Amazon Web Services account that the data
-- applies to.
--
-- 'automatedDiscoveryFreeTrialStartDate', 'usageRecord_automatedDiscoveryFreeTrialStartDate' - The date and time, in UTC and extended ISO 8601 format, when the free
-- trial of automated sensitive data discovery started for the account. If
-- the account is a member account in an organization, this value is the
-- same as the value for the organization\'s Amazon Macie administrator
-- account.
--
-- 'freeTrialStartDate', 'usageRecord_freeTrialStartDate' - The date and time, in UTC and extended ISO 8601 format, when the Amazon
-- Macie free trial started for the account.
--
-- 'usage', 'usageRecord_usage' - An array of objects that contains usage data and quotas for the account.
-- Each object contains the data for a specific usage metric and the
-- corresponding quota.
newUsageRecord ::
  UsageRecord
newUsageRecord =
  UsageRecord'
    { accountId = Prelude.Nothing,
      automatedDiscoveryFreeTrialStartDate =
        Prelude.Nothing,
      freeTrialStartDate = Prelude.Nothing,
      usage = Prelude.Nothing
    }

-- | The unique identifier for the Amazon Web Services account that the data
-- applies to.
usageRecord_accountId :: Lens.Lens' UsageRecord (Prelude.Maybe Prelude.Text)
usageRecord_accountId = Lens.lens (\UsageRecord' {accountId} -> accountId) (\s@UsageRecord' {} a -> s {accountId = a} :: UsageRecord)

-- | The date and time, in UTC and extended ISO 8601 format, when the free
-- trial of automated sensitive data discovery started for the account. If
-- the account is a member account in an organization, this value is the
-- same as the value for the organization\'s Amazon Macie administrator
-- account.
usageRecord_automatedDiscoveryFreeTrialStartDate :: Lens.Lens' UsageRecord (Prelude.Maybe Prelude.UTCTime)
usageRecord_automatedDiscoveryFreeTrialStartDate = Lens.lens (\UsageRecord' {automatedDiscoveryFreeTrialStartDate} -> automatedDiscoveryFreeTrialStartDate) (\s@UsageRecord' {} a -> s {automatedDiscoveryFreeTrialStartDate = a} :: UsageRecord) Prelude.. Lens.mapping Data._Time

-- | The date and time, in UTC and extended ISO 8601 format, when the Amazon
-- Macie free trial started for the account.
usageRecord_freeTrialStartDate :: Lens.Lens' UsageRecord (Prelude.Maybe Prelude.UTCTime)
usageRecord_freeTrialStartDate = Lens.lens (\UsageRecord' {freeTrialStartDate} -> freeTrialStartDate) (\s@UsageRecord' {} a -> s {freeTrialStartDate = a} :: UsageRecord) Prelude.. Lens.mapping Data._Time

-- | An array of objects that contains usage data and quotas for the account.
-- Each object contains the data for a specific usage metric and the
-- corresponding quota.
usageRecord_usage :: Lens.Lens' UsageRecord (Prelude.Maybe [UsageByAccount])
usageRecord_usage = Lens.lens (\UsageRecord' {usage} -> usage) (\s@UsageRecord' {} a -> s {usage = a} :: UsageRecord) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON UsageRecord where
  parseJSON =
    Data.withObject
      "UsageRecord"
      ( \x ->
          UsageRecord'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "automatedDiscoveryFreeTrialStartDate")
            Prelude.<*> (x Data..:? "freeTrialStartDate")
            Prelude.<*> (x Data..:? "usage" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable UsageRecord where
  hashWithSalt _salt UsageRecord' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` automatedDiscoveryFreeTrialStartDate
      `Prelude.hashWithSalt` freeTrialStartDate
      `Prelude.hashWithSalt` usage

instance Prelude.NFData UsageRecord where
  rnf UsageRecord' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf automatedDiscoveryFreeTrialStartDate `Prelude.seq`
        Prelude.rnf freeTrialStartDate `Prelude.seq`
          Prelude.rnf usage
