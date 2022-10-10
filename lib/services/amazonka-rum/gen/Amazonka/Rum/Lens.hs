{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Rum.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rum.Lens
  ( -- * Operations

    -- ** CreateAppMonitor
    createAppMonitor_tags,
    createAppMonitor_appMonitorConfiguration,
    createAppMonitor_cwLogEnabled,
    createAppMonitor_domain,
    createAppMonitor_name,
    createAppMonitorResponse_id,
    createAppMonitorResponse_httpStatus,

    -- ** DeleteAppMonitor
    deleteAppMonitor_name,
    deleteAppMonitorResponse_httpStatus,

    -- ** GetAppMonitor
    getAppMonitor_name,
    getAppMonitorResponse_appMonitor,
    getAppMonitorResponse_httpStatus,

    -- ** GetAppMonitorData
    getAppMonitorData_nextToken,
    getAppMonitorData_filters,
    getAppMonitorData_maxResults,
    getAppMonitorData_name,
    getAppMonitorData_timeRange,
    getAppMonitorDataResponse_nextToken,
    getAppMonitorDataResponse_events,
    getAppMonitorDataResponse_httpStatus,

    -- ** ListAppMonitors
    listAppMonitors_nextToken,
    listAppMonitors_maxResults,
    listAppMonitorsResponse_nextToken,
    listAppMonitorsResponse_appMonitorSummaries,
    listAppMonitorsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_tags,

    -- ** PutRumEvents
    putRumEvents_appMonitorDetails,
    putRumEvents_batchId,
    putRumEvents_id,
    putRumEvents_rumEvents,
    putRumEvents_userDetails,
    putRumEventsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAppMonitor
    updateAppMonitor_domain,
    updateAppMonitor_appMonitorConfiguration,
    updateAppMonitor_cwLogEnabled,
    updateAppMonitor_name,
    updateAppMonitorResponse_httpStatus,

    -- * Types

    -- ** AppMonitor
    appMonitor_tags,
    appMonitor_name,
    appMonitor_dataStorage,
    appMonitor_created,
    appMonitor_domain,
    appMonitor_state,
    appMonitor_id,
    appMonitor_appMonitorConfiguration,
    appMonitor_lastModified,

    -- ** AppMonitorConfiguration
    appMonitorConfiguration_favoritePages,
    appMonitorConfiguration_guestRoleArn,
    appMonitorConfiguration_sessionSampleRate,
    appMonitorConfiguration_enableXRay,
    appMonitorConfiguration_identityPoolId,
    appMonitorConfiguration_allowCookies,
    appMonitorConfiguration_includedPages,
    appMonitorConfiguration_excludedPages,
    appMonitorConfiguration_telemetries,

    -- ** AppMonitorDetails
    appMonitorDetails_name,
    appMonitorDetails_id,
    appMonitorDetails_version,

    -- ** AppMonitorSummary
    appMonitorSummary_name,
    appMonitorSummary_created,
    appMonitorSummary_state,
    appMonitorSummary_id,
    appMonitorSummary_lastModified,

    -- ** CwLog
    cwLog_cwLogGroup,
    cwLog_cwLogEnabled,

    -- ** DataStorage
    dataStorage_cwLog,

    -- ** QueryFilter
    queryFilter_name,
    queryFilter_values,

    -- ** RumEvent
    rumEvent_metadata,
    rumEvent_details,
    rumEvent_id,
    rumEvent_timestamp,
    rumEvent_type,

    -- ** TimeRange
    timeRange_before,
    timeRange_after,

    -- ** UserDetails
    userDetails_sessionId,
    userDetails_userId,
  )
where

import Amazonka.Rum.CreateAppMonitor
import Amazonka.Rum.DeleteAppMonitor
import Amazonka.Rum.GetAppMonitor
import Amazonka.Rum.GetAppMonitorData
import Amazonka.Rum.ListAppMonitors
import Amazonka.Rum.ListTagsForResource
import Amazonka.Rum.PutRumEvents
import Amazonka.Rum.TagResource
import Amazonka.Rum.Types.AppMonitor
import Amazonka.Rum.Types.AppMonitorConfiguration
import Amazonka.Rum.Types.AppMonitorDetails
import Amazonka.Rum.Types.AppMonitorSummary
import Amazonka.Rum.Types.CwLog
import Amazonka.Rum.Types.DataStorage
import Amazonka.Rum.Types.QueryFilter
import Amazonka.Rum.Types.RumEvent
import Amazonka.Rum.Types.TimeRange
import Amazonka.Rum.Types.UserDetails
import Amazonka.Rum.UntagResource
import Amazonka.Rum.UpdateAppMonitor
