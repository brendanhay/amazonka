{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostAndUsageReport.Lens
  ( -- * Operations

    -- ** ModifyReportDefinition
    modifyReportDefinition_reportName,
    modifyReportDefinition_reportDefinition,
    modifyReportDefinitionResponse_httpStatus,

    -- ** DeleteReportDefinition
    deleteReportDefinition_reportName,
    deleteReportDefinitionResponse_responseMessage,
    deleteReportDefinitionResponse_httpStatus,

    -- ** DescribeReportDefinitions
    describeReportDefinitions_nextToken,
    describeReportDefinitions_maxResults,
    describeReportDefinitionsResponse_nextToken,
    describeReportDefinitionsResponse_reportDefinitions,
    describeReportDefinitionsResponse_httpStatus,

    -- ** PutReportDefinition
    putReportDefinition_reportDefinition,
    putReportDefinitionResponse_httpStatus,

    -- * Types

    -- ** ReportDefinition
    reportDefinition_additionalArtifacts,
    reportDefinition_reportVersioning,
    reportDefinition_refreshClosedReports,
    reportDefinition_reportName,
    reportDefinition_timeUnit,
    reportDefinition_format,
    reportDefinition_compression,
    reportDefinition_additionalSchemaElements,
    reportDefinition_s3Bucket,
    reportDefinition_s3Prefix,
    reportDefinition_s3Region,
  )
where

import Network.AWS.CostAndUsageReport.DeleteReportDefinition
import Network.AWS.CostAndUsageReport.DescribeReportDefinitions
import Network.AWS.CostAndUsageReport.ModifyReportDefinition
import Network.AWS.CostAndUsageReport.PutReportDefinition
import Network.AWS.CostAndUsageReport.Types.ReportDefinition
