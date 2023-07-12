{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MarketplaceAnalytics.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MarketplaceAnalytics.Lens
  ( -- * Operations

    -- ** GenerateDataSet
    generateDataSet_customerDefinedValues,
    generateDataSet_destinationS3Prefix,
    generateDataSet_dataSetType,
    generateDataSet_dataSetPublicationDate,
    generateDataSet_roleNameArn,
    generateDataSet_destinationS3BucketName,
    generateDataSet_snsTopicArn,
    generateDataSetResponse_dataSetRequestId,
    generateDataSetResponse_httpStatus,

    -- ** StartSupportDataExport
    startSupportDataExport_customerDefinedValues,
    startSupportDataExport_destinationS3Prefix,
    startSupportDataExport_dataSetType,
    startSupportDataExport_fromDate,
    startSupportDataExport_roleNameArn,
    startSupportDataExport_destinationS3BucketName,
    startSupportDataExport_snsTopicArn,
    startSupportDataExportResponse_dataSetRequestId,
    startSupportDataExportResponse_httpStatus,

    -- * Types
  )
where

import Amazonka.MarketplaceAnalytics.GenerateDataSet
import Amazonka.MarketplaceAnalytics.StartSupportDataExport
