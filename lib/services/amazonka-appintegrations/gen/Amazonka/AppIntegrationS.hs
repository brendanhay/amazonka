{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.AppIntegrationS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.AppIntegrationS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** DuplicateResourceException
    _DuplicateResourceException,

    -- ** InternalServiceError
    _InternalServiceError,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourceQuotaExceededException
    _ResourceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateDataIntegration
    CreateDataIntegration (CreateDataIntegration'),
    newCreateDataIntegration,
    CreateDataIntegrationResponse (CreateDataIntegrationResponse'),
    newCreateDataIntegrationResponse,

    -- ** CreateEventIntegration
    CreateEventIntegration (CreateEventIntegration'),
    newCreateEventIntegration,
    CreateEventIntegrationResponse (CreateEventIntegrationResponse'),
    newCreateEventIntegrationResponse,

    -- ** DeleteDataIntegration
    DeleteDataIntegration (DeleteDataIntegration'),
    newDeleteDataIntegration,
    DeleteDataIntegrationResponse (DeleteDataIntegrationResponse'),
    newDeleteDataIntegrationResponse,

    -- ** DeleteEventIntegration
    DeleteEventIntegration (DeleteEventIntegration'),
    newDeleteEventIntegration,
    DeleteEventIntegrationResponse (DeleteEventIntegrationResponse'),
    newDeleteEventIntegrationResponse,

    -- ** GetDataIntegration
    GetDataIntegration (GetDataIntegration'),
    newGetDataIntegration,
    GetDataIntegrationResponse (GetDataIntegrationResponse'),
    newGetDataIntegrationResponse,

    -- ** GetEventIntegration
    GetEventIntegration (GetEventIntegration'),
    newGetEventIntegration,
    GetEventIntegrationResponse (GetEventIntegrationResponse'),
    newGetEventIntegrationResponse,

    -- ** ListDataIntegrationAssociations
    ListDataIntegrationAssociations (ListDataIntegrationAssociations'),
    newListDataIntegrationAssociations,
    ListDataIntegrationAssociationsResponse (ListDataIntegrationAssociationsResponse'),
    newListDataIntegrationAssociationsResponse,

    -- ** ListDataIntegrations
    ListDataIntegrations (ListDataIntegrations'),
    newListDataIntegrations,
    ListDataIntegrationsResponse (ListDataIntegrationsResponse'),
    newListDataIntegrationsResponse,

    -- ** ListEventIntegrationAssociations
    ListEventIntegrationAssociations (ListEventIntegrationAssociations'),
    newListEventIntegrationAssociations,
    ListEventIntegrationAssociationsResponse (ListEventIntegrationAssociationsResponse'),
    newListEventIntegrationAssociationsResponse,

    -- ** ListEventIntegrations
    ListEventIntegrations (ListEventIntegrations'),
    newListEventIntegrations,
    ListEventIntegrationsResponse (ListEventIntegrationsResponse'),
    newListEventIntegrationsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

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

    -- ** UpdateDataIntegration
    UpdateDataIntegration (UpdateDataIntegration'),
    newUpdateDataIntegration,
    UpdateDataIntegrationResponse (UpdateDataIntegrationResponse'),
    newUpdateDataIntegrationResponse,

    -- ** UpdateEventIntegration
    UpdateEventIntegration (UpdateEventIntegration'),
    newUpdateEventIntegration,
    UpdateEventIntegrationResponse (UpdateEventIntegrationResponse'),
    newUpdateEventIntegrationResponse,

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

import Amazonka.AppIntegrationS.CreateDataIntegration
import Amazonka.AppIntegrationS.CreateEventIntegration
import Amazonka.AppIntegrationS.DeleteDataIntegration
import Amazonka.AppIntegrationS.DeleteEventIntegration
import Amazonka.AppIntegrationS.GetDataIntegration
import Amazonka.AppIntegrationS.GetEventIntegration
import Amazonka.AppIntegrationS.Lens
import Amazonka.AppIntegrationS.ListDataIntegrationAssociations
import Amazonka.AppIntegrationS.ListDataIntegrations
import Amazonka.AppIntegrationS.ListEventIntegrationAssociations
import Amazonka.AppIntegrationS.ListEventIntegrations
import Amazonka.AppIntegrationS.ListTagsForResource
import Amazonka.AppIntegrationS.TagResource
import Amazonka.AppIntegrationS.Types
import Amazonka.AppIntegrationS.UntagResource
import Amazonka.AppIntegrationS.UpdateDataIntegration
import Amazonka.AppIntegrationS.UpdateEventIntegration
import Amazonka.AppIntegrationS.Waiters

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
