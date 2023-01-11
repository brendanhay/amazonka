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
-- Module      : Amazonka.MarketplaceMetering.Types.UsageRecordResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MarketplaceMetering.Types.UsageRecordResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MarketplaceMetering.Types.UsageRecord
import Amazonka.MarketplaceMetering.Types.UsageRecordResultStatus
import qualified Amazonka.Prelude as Prelude

-- | A @UsageRecordResult@ indicates the status of a given @UsageRecord@
-- processed by @BatchMeterUsage@.
--
-- /See:/ 'newUsageRecordResult' smart constructor.
data UsageRecordResult = UsageRecordResult'
  { -- | The @MeteringRecordId@ is a unique identifier for this metering event.
    meteringRecordId :: Prelude.Maybe Prelude.Text,
    -- | The @UsageRecordResult@ @Status@ indicates the status of an individual
    -- @UsageRecord@ processed by @BatchMeterUsage@.
    --
    -- -   /Success/- The @UsageRecord@ was accepted and honored by
    --     @BatchMeterUsage@.
    --
    -- -   /CustomerNotSubscribed/- The @CustomerIdentifier@ specified is not
    --     able to use your product. The @UsageRecord@ was not honored. There
    --     are three causes for this result:
    --
    --     -   The customer identifier is invalid.
    --
    --     -   The customer identifier provided in the metering record does not
    --         have an active agreement or subscription with this product.
    --         Future @UsageRecords@ for this customer will fail until the
    --         customer subscribes to your product.
    --
    --     -   The customer\'s AWS account was suspended.
    --
    -- -   /DuplicateRecord/- Indicates that the @UsageRecord@ was invalid and
    --     not honored. A previously metered @UsageRecord@ had the same
    --     customer, dimension, and time, but a different quantity.
    status :: Prelude.Maybe UsageRecordResultStatus,
    -- | The @UsageRecord@ that was part of the @BatchMeterUsage@ request.
    usageRecord :: Prelude.Maybe UsageRecord
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UsageRecordResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meteringRecordId', 'usageRecordResult_meteringRecordId' - The @MeteringRecordId@ is a unique identifier for this metering event.
--
-- 'status', 'usageRecordResult_status' - The @UsageRecordResult@ @Status@ indicates the status of an individual
-- @UsageRecord@ processed by @BatchMeterUsage@.
--
-- -   /Success/- The @UsageRecord@ was accepted and honored by
--     @BatchMeterUsage@.
--
-- -   /CustomerNotSubscribed/- The @CustomerIdentifier@ specified is not
--     able to use your product. The @UsageRecord@ was not honored. There
--     are three causes for this result:
--
--     -   The customer identifier is invalid.
--
--     -   The customer identifier provided in the metering record does not
--         have an active agreement or subscription with this product.
--         Future @UsageRecords@ for this customer will fail until the
--         customer subscribes to your product.
--
--     -   The customer\'s AWS account was suspended.
--
-- -   /DuplicateRecord/- Indicates that the @UsageRecord@ was invalid and
--     not honored. A previously metered @UsageRecord@ had the same
--     customer, dimension, and time, but a different quantity.
--
-- 'usageRecord', 'usageRecordResult_usageRecord' - The @UsageRecord@ that was part of the @BatchMeterUsage@ request.
newUsageRecordResult ::
  UsageRecordResult
newUsageRecordResult =
  UsageRecordResult'
    { meteringRecordId =
        Prelude.Nothing,
      status = Prelude.Nothing,
      usageRecord = Prelude.Nothing
    }

-- | The @MeteringRecordId@ is a unique identifier for this metering event.
usageRecordResult_meteringRecordId :: Lens.Lens' UsageRecordResult (Prelude.Maybe Prelude.Text)
usageRecordResult_meteringRecordId = Lens.lens (\UsageRecordResult' {meteringRecordId} -> meteringRecordId) (\s@UsageRecordResult' {} a -> s {meteringRecordId = a} :: UsageRecordResult)

-- | The @UsageRecordResult@ @Status@ indicates the status of an individual
-- @UsageRecord@ processed by @BatchMeterUsage@.
--
-- -   /Success/- The @UsageRecord@ was accepted and honored by
--     @BatchMeterUsage@.
--
-- -   /CustomerNotSubscribed/- The @CustomerIdentifier@ specified is not
--     able to use your product. The @UsageRecord@ was not honored. There
--     are three causes for this result:
--
--     -   The customer identifier is invalid.
--
--     -   The customer identifier provided in the metering record does not
--         have an active agreement or subscription with this product.
--         Future @UsageRecords@ for this customer will fail until the
--         customer subscribes to your product.
--
--     -   The customer\'s AWS account was suspended.
--
-- -   /DuplicateRecord/- Indicates that the @UsageRecord@ was invalid and
--     not honored. A previously metered @UsageRecord@ had the same
--     customer, dimension, and time, but a different quantity.
usageRecordResult_status :: Lens.Lens' UsageRecordResult (Prelude.Maybe UsageRecordResultStatus)
usageRecordResult_status = Lens.lens (\UsageRecordResult' {status} -> status) (\s@UsageRecordResult' {} a -> s {status = a} :: UsageRecordResult)

-- | The @UsageRecord@ that was part of the @BatchMeterUsage@ request.
usageRecordResult_usageRecord :: Lens.Lens' UsageRecordResult (Prelude.Maybe UsageRecord)
usageRecordResult_usageRecord = Lens.lens (\UsageRecordResult' {usageRecord} -> usageRecord) (\s@UsageRecordResult' {} a -> s {usageRecord = a} :: UsageRecordResult)

instance Data.FromJSON UsageRecordResult where
  parseJSON =
    Data.withObject
      "UsageRecordResult"
      ( \x ->
          UsageRecordResult'
            Prelude.<$> (x Data..:? "MeteringRecordId")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "UsageRecord")
      )

instance Prelude.Hashable UsageRecordResult where
  hashWithSalt _salt UsageRecordResult' {..} =
    _salt `Prelude.hashWithSalt` meteringRecordId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` usageRecord

instance Prelude.NFData UsageRecordResult where
  rnf UsageRecordResult' {..} =
    Prelude.rnf meteringRecordId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf usageRecord
