{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.AppIntegrationS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-07-29@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The Amazon AppIntegrations service enables you to configure and reuse
-- connections to external applications.
--
-- For information about how you can use external applications with Amazon
-- Connect, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/crm.html Set up pre-built integrations>
-- and
-- <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-wisdom.html Deliver information to agents using Amazon Connect Wisdom>
-- in the /Amazon Connect Administrator Guide/.
module Network.AWS.AppIntegrationS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** DuplicateResourceException
    _DuplicateResourceException,

    -- ** ResourceQuotaExceededException
    _ResourceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServiceError
    _InternalServiceError,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetEventIntegration
    GetEventIntegration (GetEventIntegration'),
    newGetEventIntegration,
    GetEventIntegrationResponse (GetEventIntegrationResponse'),
    newGetEventIntegrationResponse,

    -- ** ListDataIntegrations
    ListDataIntegrations (ListDataIntegrations'),
    newListDataIntegrations,
    ListDataIntegrationsResponse (ListDataIntegrationsResponse'),
    newListDataIntegrationsResponse,

    -- ** CreateDataIntegration
    CreateDataIntegration (CreateDataIntegration'),
    newCreateDataIntegration,
    CreateDataIntegrationResponse (CreateDataIntegrationResponse'),
    newCreateDataIntegrationResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListEventIntegrationAssociations
    ListEventIntegrationAssociations (ListEventIntegrationAssociations'),
    newListEventIntegrationAssociations,
    ListEventIntegrationAssociationsResponse (ListEventIntegrationAssociationsResponse'),
    newListEventIntegrationAssociationsResponse,

    -- ** GetDataIntegration
    GetDataIntegration (GetDataIntegration'),
    newGetDataIntegration,
    GetDataIntegrationResponse (GetDataIntegrationResponse'),
    newGetDataIntegrationResponse,

    -- ** ListEventIntegrations
    ListEventIntegrations (ListEventIntegrations'),
    newListEventIntegrations,
    ListEventIntegrationsResponse (ListEventIntegrationsResponse'),
    newListEventIntegrationsResponse,

    -- ** DeleteEventIntegration
    DeleteEventIntegration (DeleteEventIntegration'),
    newDeleteEventIntegration,
    DeleteEventIntegrationResponse (DeleteEventIntegrationResponse'),
    newDeleteEventIntegrationResponse,

    -- ** UpdateEventIntegration
    UpdateEventIntegration (UpdateEventIntegration'),
    newUpdateEventIntegration,
    UpdateEventIntegrationResponse (UpdateEventIntegrationResponse'),
    newUpdateEventIntegrationResponse,

    -- ** DeleteDataIntegration
    DeleteDataIntegration (DeleteDataIntegration'),
    newDeleteDataIntegration,
    DeleteDataIntegrationResponse (DeleteDataIntegrationResponse'),
    newDeleteDataIntegrationResponse,

    -- ** UpdateDataIntegration
    UpdateDataIntegration (UpdateDataIntegration'),
    newUpdateDataIntegration,
    UpdateDataIntegrationResponse (UpdateDataIntegrationResponse'),
    newUpdateDataIntegrationResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** ListDataIntegrationAssociations
    ListDataIntegrationAssociations (ListDataIntegrationAssociations'),
    newListDataIntegrationAssociations,
    ListDataIntegrationAssociationsResponse (ListDataIntegrationAssociationsResponse'),
    newListDataIntegrationAssociationsResponse,

    -- ** CreateEventIntegration
    CreateEventIntegration (CreateEventIntegration'),
    newCreateEventIntegration,
    CreateEventIntegrationResponse (CreateEventIntegrationResponse'),
    newCreateEventIntegrationResponse,

    -- * Types

    -- ** DataIntegrationAssociationSummary
    DataIntegrationAssociationSummary (DataIntegrationAssociationSummary'),
    newDataIntegrationAssociationSummary,

    -- ** DataIntegrationSummary
    DataIntegrationSummary (DataIntegrationSummary'),
    newDataIntegrationSummary,

    -- ** EventFilter
    EventFilter (EventFilter'),
    newEventFilter,

    -- ** EventIntegration
    EventIntegration (EventIntegration'),
    newEventIntegration,

    -- ** EventIntegrationAssociation
    EventIntegrationAssociation (EventIntegrationAssociation'),
    newEventIntegrationAssociation,

    -- ** ScheduleConfiguration
    ScheduleConfiguration (ScheduleConfiguration'),
    newScheduleConfiguration,
  )
where

import Network.AWS.AppIntegrationS.CreateDataIntegration
import Network.AWS.AppIntegrationS.CreateEventIntegration
import Network.AWS.AppIntegrationS.DeleteDataIntegration
import Network.AWS.AppIntegrationS.DeleteEventIntegration
import Network.AWS.AppIntegrationS.GetDataIntegration
import Network.AWS.AppIntegrationS.GetEventIntegration
import Network.AWS.AppIntegrationS.Lens
import Network.AWS.AppIntegrationS.ListDataIntegrationAssociations
import Network.AWS.AppIntegrationS.ListDataIntegrations
import Network.AWS.AppIntegrationS.ListEventIntegrationAssociations
import Network.AWS.AppIntegrationS.ListEventIntegrations
import Network.AWS.AppIntegrationS.ListTagsForResource
import Network.AWS.AppIntegrationS.TagResource
import Network.AWS.AppIntegrationS.Types
import Network.AWS.AppIntegrationS.UntagResource
import Network.AWS.AppIntegrationS.UpdateDataIntegration
import Network.AWS.AppIntegrationS.UpdateEventIntegration
import Network.AWS.AppIntegrationS.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'AppIntegrationS'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
