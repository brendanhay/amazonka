{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceAnalytics.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceAnalytics.Lens
  ( -- * Operations

    -- ** StartSupportDataExport
    startSupportDataExport_destinationS3Prefix,
    startSupportDataExport_customerDefinedValues,
    startSupportDataExport_dataSetType,
    startSupportDataExport_fromDate,
    startSupportDataExport_roleNameArn,
    startSupportDataExport_destinationS3BucketName,
    startSupportDataExport_snsTopicArn,
    startSupportDataExportResponse_dataSetRequestId,
    startSupportDataExportResponse_httpStatus,

    -- ** GenerateDataSet
    generateDataSet_destinationS3Prefix,
    generateDataSet_customerDefinedValues,
    generateDataSet_dataSetType,
    generateDataSet_dataSetPublicationDate,
    generateDataSet_roleNameArn,
    generateDataSet_destinationS3BucketName,
    generateDataSet_snsTopicArn,
    generateDataSetResponse_dataSetRequestId,
    generateDataSetResponse_httpStatus,

    -- * Types
  )
where

import Network.AWS.MarketplaceAnalytics.GenerateDataSet
import Network.AWS.MarketplaceAnalytics.StartSupportDataExport
