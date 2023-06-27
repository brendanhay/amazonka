{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppIntegrationS.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppIntegrationS.Lens
  ( -- * Operations

    -- ** CreateDataIntegration
    createDataIntegration_clientToken,
    createDataIntegration_description,
    createDataIntegration_fileConfiguration,
    createDataIntegration_objectConfiguration,
    createDataIntegration_tags,
    createDataIntegration_name,
    createDataIntegration_kmsKey,
    createDataIntegration_sourceURI,
    createDataIntegration_scheduleConfig,
    createDataIntegrationResponse_arn,
    createDataIntegrationResponse_clientToken,
    createDataIntegrationResponse_description,
    createDataIntegrationResponse_fileConfiguration,
    createDataIntegrationResponse_id,
    createDataIntegrationResponse_kmsKey,
    createDataIntegrationResponse_name,
    createDataIntegrationResponse_objectConfiguration,
    createDataIntegrationResponse_scheduleConfiguration,
    createDataIntegrationResponse_sourceURI,
    createDataIntegrationResponse_tags,
    createDataIntegrationResponse_httpStatus,

    -- ** CreateEventIntegration
    createEventIntegration_clientToken,
    createEventIntegration_description,
    createEventIntegration_tags,
    createEventIntegration_name,
    createEventIntegration_eventFilter,
    createEventIntegration_eventBridgeBus,
    createEventIntegrationResponse_eventIntegrationArn,
    createEventIntegrationResponse_httpStatus,

    -- ** DeleteDataIntegration
    deleteDataIntegration_dataIntegrationIdentifier,
    deleteDataIntegrationResponse_httpStatus,

    -- ** DeleteEventIntegration
    deleteEventIntegration_name,
    deleteEventIntegrationResponse_httpStatus,

    -- ** GetDataIntegration
    getDataIntegration_identifier,
    getDataIntegrationResponse_arn,
    getDataIntegrationResponse_description,
    getDataIntegrationResponse_fileConfiguration,
    getDataIntegrationResponse_id,
    getDataIntegrationResponse_kmsKey,
    getDataIntegrationResponse_name,
    getDataIntegrationResponse_objectConfiguration,
    getDataIntegrationResponse_scheduleConfiguration,
    getDataIntegrationResponse_sourceURI,
    getDataIntegrationResponse_tags,
    getDataIntegrationResponse_httpStatus,

    -- ** GetEventIntegration
    getEventIntegration_name,
    getEventIntegrationResponse_description,
    getEventIntegrationResponse_eventBridgeBus,
    getEventIntegrationResponse_eventFilter,
    getEventIntegrationResponse_eventIntegrationArn,
    getEventIntegrationResponse_name,
    getEventIntegrationResponse_tags,
    getEventIntegrationResponse_httpStatus,

    -- ** ListDataIntegrationAssociations
    listDataIntegrationAssociations_maxResults,
    listDataIntegrationAssociations_nextToken,
    listDataIntegrationAssociations_dataIntegrationIdentifier,
    listDataIntegrationAssociationsResponse_dataIntegrationAssociations,
    listDataIntegrationAssociationsResponse_nextToken,
    listDataIntegrationAssociationsResponse_httpStatus,

    -- ** ListDataIntegrations
    listDataIntegrations_maxResults,
    listDataIntegrations_nextToken,
    listDataIntegrationsResponse_dataIntegrations,
    listDataIntegrationsResponse_nextToken,
    listDataIntegrationsResponse_httpStatus,

    -- ** ListEventIntegrationAssociations
    listEventIntegrationAssociations_maxResults,
    listEventIntegrationAssociations_nextToken,
    listEventIntegrationAssociations_eventIntegrationName,
    listEventIntegrationAssociationsResponse_eventIntegrationAssociations,
    listEventIntegrationAssociationsResponse_nextToken,
    listEventIntegrationAssociationsResponse_httpStatus,

    -- ** ListEventIntegrations
    listEventIntegrations_maxResults,
    listEventIntegrations_nextToken,
    listEventIntegrationsResponse_eventIntegrations,
    listEventIntegrationsResponse_nextToken,
    listEventIntegrationsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateDataIntegration
    updateDataIntegration_description,
    updateDataIntegration_name,
    updateDataIntegration_identifier,
    updateDataIntegrationResponse_httpStatus,

    -- ** UpdateEventIntegration
    updateEventIntegration_description,
    updateEventIntegration_name,
    updateEventIntegrationResponse_httpStatus,

    -- * Types

    -- ** DataIntegrationAssociationSummary
    dataIntegrationAssociationSummary_clientId,
    dataIntegrationAssociationSummary_dataIntegrationArn,
    dataIntegrationAssociationSummary_dataIntegrationAssociationArn,

    -- ** DataIntegrationSummary
    dataIntegrationSummary_arn,
    dataIntegrationSummary_name,
    dataIntegrationSummary_sourceURI,

    -- ** EventFilter
    eventFilter_source,

    -- ** EventIntegration
    eventIntegration_description,
    eventIntegration_eventBridgeBus,
    eventIntegration_eventFilter,
    eventIntegration_eventIntegrationArn,
    eventIntegration_name,
    eventIntegration_tags,

    -- ** EventIntegrationAssociation
    eventIntegrationAssociation_clientAssociationMetadata,
    eventIntegrationAssociation_clientId,
    eventIntegrationAssociation_eventBridgeRuleName,
    eventIntegrationAssociation_eventIntegrationAssociationArn,
    eventIntegrationAssociation_eventIntegrationAssociationId,
    eventIntegrationAssociation_eventIntegrationName,

    -- ** FileConfiguration
    fileConfiguration_filters,
    fileConfiguration_folders,

    -- ** ScheduleConfiguration
    scheduleConfiguration_firstExecutionFrom,
    scheduleConfiguration_object,
    scheduleConfiguration_scheduleExpression,
  )
where

import Amazonka.AppIntegrationS.CreateDataIntegration
import Amazonka.AppIntegrationS.CreateEventIntegration
import Amazonka.AppIntegrationS.DeleteDataIntegration
import Amazonka.AppIntegrationS.DeleteEventIntegration
import Amazonka.AppIntegrationS.GetDataIntegration
import Amazonka.AppIntegrationS.GetEventIntegration
import Amazonka.AppIntegrationS.ListDataIntegrationAssociations
import Amazonka.AppIntegrationS.ListDataIntegrations
import Amazonka.AppIntegrationS.ListEventIntegrationAssociations
import Amazonka.AppIntegrationS.ListEventIntegrations
import Amazonka.AppIntegrationS.ListTagsForResource
import Amazonka.AppIntegrationS.TagResource
import Amazonka.AppIntegrationS.Types.DataIntegrationAssociationSummary
import Amazonka.AppIntegrationS.Types.DataIntegrationSummary
import Amazonka.AppIntegrationS.Types.EventFilter
import Amazonka.AppIntegrationS.Types.EventIntegration
import Amazonka.AppIntegrationS.Types.EventIntegrationAssociation
import Amazonka.AppIntegrationS.Types.FileConfiguration
import Amazonka.AppIntegrationS.Types.ScheduleConfiguration
import Amazonka.AppIntegrationS.UntagResource
import Amazonka.AppIntegrationS.UpdateDataIntegration
import Amazonka.AppIntegrationS.UpdateEventIntegration
