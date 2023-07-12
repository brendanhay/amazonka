{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MarketplaceMetering.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MarketplaceMetering.Lens
  ( -- * Operations

    -- ** BatchMeterUsage
    batchMeterUsage_usageRecords,
    batchMeterUsage_productCode,
    batchMeterUsageResponse_results,
    batchMeterUsageResponse_unprocessedRecords,
    batchMeterUsageResponse_httpStatus,

    -- ** MeterUsage
    meterUsage_dryRun,
    meterUsage_usageAllocations,
    meterUsage_usageQuantity,
    meterUsage_productCode,
    meterUsage_timestamp,
    meterUsage_usageDimension,
    meterUsageResponse_meteringRecordId,
    meterUsageResponse_httpStatus,

    -- ** RegisterUsage
    registerUsage_nonce,
    registerUsage_productCode,
    registerUsage_publicKeyVersion,
    registerUsageResponse_publicKeyRotationTimestamp,
    registerUsageResponse_signature,
    registerUsageResponse_httpStatus,

    -- ** ResolveCustomer
    resolveCustomer_registrationToken,
    resolveCustomerResponse_customerAWSAccountId,
    resolveCustomerResponse_customerIdentifier,
    resolveCustomerResponse_productCode,
    resolveCustomerResponse_httpStatus,

    -- * Types

    -- ** Tag
    tag_key,
    tag_value,

    -- ** UsageAllocation
    usageAllocation_tags,
    usageAllocation_allocatedUsageQuantity,

    -- ** UsageRecord
    usageRecord_quantity,
    usageRecord_usageAllocations,
    usageRecord_timestamp,
    usageRecord_customerIdentifier,
    usageRecord_dimension,

    -- ** UsageRecordResult
    usageRecordResult_meteringRecordId,
    usageRecordResult_status,
    usageRecordResult_usageRecord,
  )
where

import Amazonka.MarketplaceMetering.BatchMeterUsage
import Amazonka.MarketplaceMetering.MeterUsage
import Amazonka.MarketplaceMetering.RegisterUsage
import Amazonka.MarketplaceMetering.ResolveCustomer
import Amazonka.MarketplaceMetering.Types.Tag
import Amazonka.MarketplaceMetering.Types.UsageAllocation
import Amazonka.MarketplaceMetering.Types.UsageRecord
import Amazonka.MarketplaceMetering.Types.UsageRecordResult
