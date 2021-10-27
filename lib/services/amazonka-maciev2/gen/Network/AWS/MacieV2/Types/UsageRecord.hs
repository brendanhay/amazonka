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
-- Module      : Network.AWS.MacieV2.Types.UsageRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types.UsageRecord where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MacieV2.Types.UsageByAccount
import qualified Network.AWS.Prelude as Prelude

-- | Provides quota and aggregated usage data for an Amazon Macie account.
--
-- /See:/ 'newUsageRecord' smart constructor.
data UsageRecord = UsageRecord'
  { -- | The unique identifier for the Amazon Web Services account that the data
    -- applies to.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in UTC and extended ISO 8601 format, when the free
    -- trial started for the account.
    freeTrialStartDate :: Prelude.Maybe Core.POSIX,
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
-- 'freeTrialStartDate', 'usageRecord_freeTrialStartDate' - The date and time, in UTC and extended ISO 8601 format, when the free
-- trial started for the account.
--
-- 'usage', 'usageRecord_usage' - An array of objects that contains usage data and quotas for the account.
-- Each object contains the data for a specific usage metric and the
-- corresponding quota.
newUsageRecord ::
  UsageRecord
newUsageRecord =
  UsageRecord'
    { accountId = Prelude.Nothing,
      freeTrialStartDate = Prelude.Nothing,
      usage = Prelude.Nothing
    }

-- | The unique identifier for the Amazon Web Services account that the data
-- applies to.
usageRecord_accountId :: Lens.Lens' UsageRecord (Prelude.Maybe Prelude.Text)
usageRecord_accountId = Lens.lens (\UsageRecord' {accountId} -> accountId) (\s@UsageRecord' {} a -> s {accountId = a} :: UsageRecord)

-- | The date and time, in UTC and extended ISO 8601 format, when the free
-- trial started for the account.
usageRecord_freeTrialStartDate :: Lens.Lens' UsageRecord (Prelude.Maybe Prelude.UTCTime)
usageRecord_freeTrialStartDate = Lens.lens (\UsageRecord' {freeTrialStartDate} -> freeTrialStartDate) (\s@UsageRecord' {} a -> s {freeTrialStartDate = a} :: UsageRecord) Prelude.. Lens.mapping Core._Time

-- | An array of objects that contains usage data and quotas for the account.
-- Each object contains the data for a specific usage metric and the
-- corresponding quota.
usageRecord_usage :: Lens.Lens' UsageRecord (Prelude.Maybe [UsageByAccount])
usageRecord_usage = Lens.lens (\UsageRecord' {usage} -> usage) (\s@UsageRecord' {} a -> s {usage = a} :: UsageRecord) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON UsageRecord where
  parseJSON =
    Core.withObject
      "UsageRecord"
      ( \x ->
          UsageRecord'
            Prelude.<$> (x Core..:? "accountId")
            Prelude.<*> (x Core..:? "freeTrialStartDate")
            Prelude.<*> (x Core..:? "usage" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable UsageRecord

instance Prelude.NFData UsageRecord
