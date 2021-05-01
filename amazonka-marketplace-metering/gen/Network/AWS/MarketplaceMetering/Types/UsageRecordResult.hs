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
-- Module      : Network.AWS.MarketplaceMetering.Types.UsageRecordResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceMetering.Types.UsageRecordResult where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceMetering.Types.UsageRecord
import Network.AWS.MarketplaceMetering.Types.UsageRecordResultStatus
import qualified Network.AWS.Prelude as Prelude

-- | A UsageRecordResult indicates the status of a given UsageRecord
-- processed by BatchMeterUsage.
--
-- /See:/ 'newUsageRecordResult' smart constructor.
data UsageRecordResult = UsageRecordResult'
  { -- | The UsageRecordResult Status indicates the status of an individual
    -- UsageRecord processed by BatchMeterUsage.
    --
    -- -   /Success/- The UsageRecord was accepted and honored by
    --     BatchMeterUsage.
    --
    -- -   /CustomerNotSubscribed/- The CustomerIdentifier specified is not
    --     subscribed to your product. The UsageRecord was not honored. Future
    --     UsageRecords for this customer will fail until the customer
    --     subscribes to your product.
    --
    -- -   /DuplicateRecord/- Indicates that the UsageRecord was invalid and
    --     not honored. A previously metered UsageRecord had the same customer,
    --     dimension, and time, but a different quantity.
    status :: Prelude.Maybe UsageRecordResultStatus,
    -- | The MeteringRecordId is a unique identifier for this metering event.
    meteringRecordId :: Prelude.Maybe Prelude.Text,
    -- | The UsageRecord that was part of the BatchMeterUsage request.
    usageRecord :: Prelude.Maybe UsageRecord
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UsageRecordResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'usageRecordResult_status' - The UsageRecordResult Status indicates the status of an individual
-- UsageRecord processed by BatchMeterUsage.
--
-- -   /Success/- The UsageRecord was accepted and honored by
--     BatchMeterUsage.
--
-- -   /CustomerNotSubscribed/- The CustomerIdentifier specified is not
--     subscribed to your product. The UsageRecord was not honored. Future
--     UsageRecords for this customer will fail until the customer
--     subscribes to your product.
--
-- -   /DuplicateRecord/- Indicates that the UsageRecord was invalid and
--     not honored. A previously metered UsageRecord had the same customer,
--     dimension, and time, but a different quantity.
--
-- 'meteringRecordId', 'usageRecordResult_meteringRecordId' - The MeteringRecordId is a unique identifier for this metering event.
--
-- 'usageRecord', 'usageRecordResult_usageRecord' - The UsageRecord that was part of the BatchMeterUsage request.
newUsageRecordResult ::
  UsageRecordResult
newUsageRecordResult =
  UsageRecordResult'
    { status = Prelude.Nothing,
      meteringRecordId = Prelude.Nothing,
      usageRecord = Prelude.Nothing
    }

-- | The UsageRecordResult Status indicates the status of an individual
-- UsageRecord processed by BatchMeterUsage.
--
-- -   /Success/- The UsageRecord was accepted and honored by
--     BatchMeterUsage.
--
-- -   /CustomerNotSubscribed/- The CustomerIdentifier specified is not
--     subscribed to your product. The UsageRecord was not honored. Future
--     UsageRecords for this customer will fail until the customer
--     subscribes to your product.
--
-- -   /DuplicateRecord/- Indicates that the UsageRecord was invalid and
--     not honored. A previously metered UsageRecord had the same customer,
--     dimension, and time, but a different quantity.
usageRecordResult_status :: Lens.Lens' UsageRecordResult (Prelude.Maybe UsageRecordResultStatus)
usageRecordResult_status = Lens.lens (\UsageRecordResult' {status} -> status) (\s@UsageRecordResult' {} a -> s {status = a} :: UsageRecordResult)

-- | The MeteringRecordId is a unique identifier for this metering event.
usageRecordResult_meteringRecordId :: Lens.Lens' UsageRecordResult (Prelude.Maybe Prelude.Text)
usageRecordResult_meteringRecordId = Lens.lens (\UsageRecordResult' {meteringRecordId} -> meteringRecordId) (\s@UsageRecordResult' {} a -> s {meteringRecordId = a} :: UsageRecordResult)

-- | The UsageRecord that was part of the BatchMeterUsage request.
usageRecordResult_usageRecord :: Lens.Lens' UsageRecordResult (Prelude.Maybe UsageRecord)
usageRecordResult_usageRecord = Lens.lens (\UsageRecordResult' {usageRecord} -> usageRecord) (\s@UsageRecordResult' {} a -> s {usageRecord = a} :: UsageRecordResult)

instance Prelude.FromJSON UsageRecordResult where
  parseJSON =
    Prelude.withObject
      "UsageRecordResult"
      ( \x ->
          UsageRecordResult'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "MeteringRecordId")
            Prelude.<*> (x Prelude..:? "UsageRecord")
      )

instance Prelude.Hashable UsageRecordResult

instance Prelude.NFData UsageRecordResult
