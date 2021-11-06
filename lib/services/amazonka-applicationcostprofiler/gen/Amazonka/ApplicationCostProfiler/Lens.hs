{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ApplicationCostProfiler.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationCostProfiler.Lens
  ( -- * Operations

    -- ** ImportApplicationUsage
    importApplicationUsage_sourceS3Location,
    importApplicationUsageResponse_httpStatus,
    importApplicationUsageResponse_importId,

    -- ** PutReportDefinition
    putReportDefinition_reportId,
    putReportDefinition_reportDescription,
    putReportDefinition_reportFrequency,
    putReportDefinition_format,
    putReportDefinition_destinationS3Location,
    putReportDefinitionResponse_reportId,
    putReportDefinitionResponse_httpStatus,

    -- ** DeleteReportDefinition
    deleteReportDefinition_reportId,
    deleteReportDefinitionResponse_reportId,
    deleteReportDefinitionResponse_httpStatus,

    -- ** UpdateReportDefinition
    updateReportDefinition_reportId,
    updateReportDefinition_reportDescription,
    updateReportDefinition_reportFrequency,
    updateReportDefinition_format,
    updateReportDefinition_destinationS3Location,
    updateReportDefinitionResponse_reportId,
    updateReportDefinitionResponse_httpStatus,

    -- ** GetReportDefinition
    getReportDefinition_reportId,
    getReportDefinitionResponse_httpStatus,
    getReportDefinitionResponse_reportId,
    getReportDefinitionResponse_reportDescription,
    getReportDefinitionResponse_reportFrequency,
    getReportDefinitionResponse_format,
    getReportDefinitionResponse_destinationS3Location,
    getReportDefinitionResponse_createdAt,
    getReportDefinitionResponse_lastUpdated,

    -- ** ListReportDefinitions
    listReportDefinitions_nextToken,
    listReportDefinitions_maxResults,
    listReportDefinitionsResponse_nextToken,
    listReportDefinitionsResponse_reportDefinitions,
    listReportDefinitionsResponse_httpStatus,

    -- * Types

    -- ** ReportDefinition
    reportDefinition_lastUpdatedAt,
    reportDefinition_createdAt,
    reportDefinition_reportId,
    reportDefinition_format,
    reportDefinition_reportFrequency,
    reportDefinition_reportDescription,
    reportDefinition_destinationS3Location,

    -- ** S3Location
    s3Location_bucket,
    s3Location_prefix,

    -- ** SourceS3Location
    sourceS3Location_region,
    sourceS3Location_bucket,
    sourceS3Location_key,
  )
where

import Amazonka.ApplicationCostProfiler.DeleteReportDefinition
import Amazonka.ApplicationCostProfiler.GetReportDefinition
import Amazonka.ApplicationCostProfiler.ImportApplicationUsage
import Amazonka.ApplicationCostProfiler.ListReportDefinitions
import Amazonka.ApplicationCostProfiler.PutReportDefinition
import Amazonka.ApplicationCostProfiler.Types.ReportDefinition
import Amazonka.ApplicationCostProfiler.Types.S3Location
import Amazonka.ApplicationCostProfiler.Types.SourceS3Location
import Amazonka.ApplicationCostProfiler.UpdateReportDefinition
