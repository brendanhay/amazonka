{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppIntegrationS.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppIntegrationS.Lens
  ( -- * Operations

    -- ** GetEventIntegration
    getEventIntegration_name,
    getEventIntegrationResponse_eventBridgeBus,
    getEventIntegrationResponse_eventFilter,
    getEventIntegrationResponse_eventIntegrationArn,
    getEventIntegrationResponse_name,
    getEventIntegrationResponse_description,
    getEventIntegrationResponse_tags,
    getEventIntegrationResponse_httpStatus,

    -- ** ListDataIntegrations
    listDataIntegrations_nextToken,
    listDataIntegrations_maxResults,
    listDataIntegrationsResponse_dataIntegrations,
    listDataIntegrationsResponse_nextToken,
    listDataIntegrationsResponse_httpStatus,

    -- ** CreateDataIntegration
    createDataIntegration_scheduleConfig,
    createDataIntegration_clientToken,
    createDataIntegration_kmsKey,
    createDataIntegration_sourceURI,
    createDataIntegration_description,
    createDataIntegration_tags,
    createDataIntegration_name,
    createDataIntegrationResponse_clientToken,
    createDataIntegrationResponse_kmsKey,
    createDataIntegrationResponse_arn,
    createDataIntegrationResponse_scheduleConfiguration,
    createDataIntegrationResponse_name,
    createDataIntegrationResponse_id,
    createDataIntegrationResponse_sourceURI,
    createDataIntegrationResponse_description,
    createDataIntegrationResponse_tags,
    createDataIntegrationResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListEventIntegrationAssociations
    listEventIntegrationAssociations_nextToken,
    listEventIntegrationAssociations_maxResults,
    listEventIntegrationAssociations_eventIntegrationName,
    listEventIntegrationAssociationsResponse_eventIntegrationAssociations,
    listEventIntegrationAssociationsResponse_nextToken,
    listEventIntegrationAssociationsResponse_httpStatus,

    -- ** GetDataIntegration
    getDataIntegration_identifier,
    getDataIntegrationResponse_kmsKey,
    getDataIntegrationResponse_arn,
    getDataIntegrationResponse_scheduleConfiguration,
    getDataIntegrationResponse_name,
    getDataIntegrationResponse_id,
    getDataIntegrationResponse_sourceURI,
    getDataIntegrationResponse_description,
    getDataIntegrationResponse_tags,
    getDataIntegrationResponse_httpStatus,

    -- ** ListEventIntegrations
    listEventIntegrations_nextToken,
    listEventIntegrations_maxResults,
    listEventIntegrationsResponse_eventIntegrations,
    listEventIntegrationsResponse_nextToken,
    listEventIntegrationsResponse_httpStatus,

    -- ** DeleteEventIntegration
    deleteEventIntegration_name,
    deleteEventIntegrationResponse_httpStatus,

    -- ** UpdateEventIntegration
    updateEventIntegration_description,
    updateEventIntegration_name,
    updateEventIntegrationResponse_httpStatus,

    -- ** DeleteDataIntegration
    deleteDataIntegration_dataIntegrationIdentifier,
    deleteDataIntegrationResponse_httpStatus,

    -- ** UpdateDataIntegration
    updateDataIntegration_name,
    updateDataIntegration_description,
    updateDataIntegration_identifier,
    updateDataIntegrationResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** ListDataIntegrationAssociations
    listDataIntegrationAssociations_nextToken,
    listDataIntegrationAssociations_maxResults,
    listDataIntegrationAssociations_dataIntegrationIdentifier,
    listDataIntegrationAssociationsResponse_dataIntegrationAssociations,
    listDataIntegrationAssociationsResponse_nextToken,
    listDataIntegrationAssociationsResponse_httpStatus,

    -- ** CreateEventIntegration
    createEventIntegration_clientToken,
    createEventIntegration_description,
    createEventIntegration_tags,
    createEventIntegration_name,
    createEventIntegration_eventFilter,
    createEventIntegration_eventBridgeBus,
    createEventIntegrationResponse_eventIntegrationArn,
    createEventIntegrationResponse_httpStatus,

    -- * Types

    -- ** DataIntegrationAssociationSummary
    dataIntegrationAssociationSummary_clientId,
    dataIntegrationAssociationSummary_dataIntegrationAssociationArn,
    dataIntegrationAssociationSummary_dataIntegrationArn,

    -- ** DataIntegrationSummary
    dataIntegrationSummary_arn,
    dataIntegrationSummary_name,
    dataIntegrationSummary_sourceURI,

    -- ** EventFilter
    eventFilter_source,

    -- ** EventIntegration
    eventIntegration_eventBridgeBus,
    eventIntegration_eventFilter,
    eventIntegration_eventIntegrationArn,
    eventIntegration_name,
    eventIntegration_description,
    eventIntegration_tags,

    -- ** EventIntegrationAssociation
    eventIntegrationAssociation_clientId,
    eventIntegrationAssociation_eventIntegrationName,
    eventIntegrationAssociation_clientAssociationMetadata,
    eventIntegrationAssociation_eventIntegrationAssociationId,
    eventIntegrationAssociation_eventIntegrationAssociationArn,
    eventIntegrationAssociation_eventBridgeRuleName,

    -- ** ScheduleConfiguration
    scheduleConfiguration_scheduleExpression,
    scheduleConfiguration_object,
    scheduleConfiguration_firstExecutionFrom,
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
import Amazonka.AppIntegrationS.Types.ScheduleConfiguration
import Amazonka.AppIntegrationS.UntagResource
import Amazonka.AppIntegrationS.UpdateDataIntegration
import Amazonka.AppIntegrationS.UpdateEventIntegration
