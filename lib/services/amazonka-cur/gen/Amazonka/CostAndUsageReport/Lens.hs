{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CostAndUsageReport.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostAndUsageReport.Lens
  ( -- * Operations

    -- ** DeleteReportDefinition
    deleteReportDefinition_reportName,
    deleteReportDefinitionResponse_responseMessage,
    deleteReportDefinitionResponse_httpStatus,

    -- ** DescribeReportDefinitions
    describeReportDefinitions_maxResults,
    describeReportDefinitions_nextToken,
    describeReportDefinitionsResponse_nextToken,
    describeReportDefinitionsResponse_reportDefinitions,
    describeReportDefinitionsResponse_httpStatus,

    -- ** ModifyReportDefinition
    modifyReportDefinition_reportName,
    modifyReportDefinition_reportDefinition,
    modifyReportDefinitionResponse_httpStatus,

    -- ** PutReportDefinition
    putReportDefinition_reportDefinition,
    putReportDefinitionResponse_httpStatus,

    -- * Types

    -- ** ReportDefinition
    reportDefinition_additionalArtifacts,
    reportDefinition_billingViewArn,
    reportDefinition_refreshClosedReports,
    reportDefinition_reportVersioning,
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

import Amazonka.CostAndUsageReport.DeleteReportDefinition
import Amazonka.CostAndUsageReport.DescribeReportDefinitions
import Amazonka.CostAndUsageReport.ModifyReportDefinition
import Amazonka.CostAndUsageReport.PutReportDefinition
import Amazonka.CostAndUsageReport.Types.ReportDefinition
