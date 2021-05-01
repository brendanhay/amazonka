{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS CloudHSM Service
--
-- This is documentation for __AWS CloudHSM Classic__. For more
-- information, see
-- <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs>,
-- the
-- <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference>.
--
-- __For information about the current version of AWS CloudHSM__, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM>, the
-- <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference>.
module Network.AWS.CloudHSM
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** CloudHsmInternalException
    _CloudHsmInternalException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** CloudHsmServiceException
    _CloudHsmServiceException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListHsms (Paginated)
    ListHsms (ListHsms'),
    newListHsms,
    ListHsmsResponse (ListHsmsResponse'),
    newListHsmsResponse,

    -- ** DeleteHsm
    DeleteHsm (DeleteHsm'),
    newDeleteHsm,
    DeleteHsmResponse (DeleteHsmResponse'),
    newDeleteHsmResponse,

    -- ** ModifyLunaClient
    ModifyLunaClient (ModifyLunaClient'),
    newModifyLunaClient,
    ModifyLunaClientResponse (ModifyLunaClientResponse'),
    newModifyLunaClientResponse,

    -- ** DeleteHapg
    DeleteHapg (DeleteHapg'),
    newDeleteHapg,
    DeleteHapgResponse (DeleteHapgResponse'),
    newDeleteHapgResponse,

    -- ** GetConfig
    GetConfig (GetConfig'),
    newGetConfig,
    GetConfigResponse (GetConfigResponse'),
    newGetConfigResponse,

    -- ** DeleteLunaClient
    DeleteLunaClient (DeleteLunaClient'),
    newDeleteLunaClient,
    DeleteLunaClientResponse (DeleteLunaClientResponse'),
    newDeleteLunaClientResponse,

    -- ** ListAvailableZones
    ListAvailableZones (ListAvailableZones'),
    newListAvailableZones,
    ListAvailableZonesResponse (ListAvailableZonesResponse'),
    newListAvailableZonesResponse,

    -- ** ModifyHapg
    ModifyHapg (ModifyHapg'),
    newModifyHapg,
    ModifyHapgResponse (ModifyHapgResponse'),
    newModifyHapgResponse,

    -- ** ListLunaClients (Paginated)
    ListLunaClients (ListLunaClients'),
    newListLunaClients,
    ListLunaClientsResponse (ListLunaClientsResponse'),
    newListLunaClientsResponse,

    -- ** RemoveTagsFromResource
    RemoveTagsFromResource (RemoveTagsFromResource'),
    newRemoveTagsFromResource,
    RemoveTagsFromResourceResponse (RemoveTagsFromResourceResponse'),
    newRemoveTagsFromResourceResponse,

    -- ** DescribeHapg
    DescribeHapg (DescribeHapg'),
    newDescribeHapg,
    DescribeHapgResponse (DescribeHapgResponse'),
    newDescribeHapgResponse,

    -- ** CreateLunaClient
    CreateLunaClient (CreateLunaClient'),
    newCreateLunaClient,
    CreateLunaClientResponse (CreateLunaClientResponse'),
    newCreateLunaClientResponse,

    -- ** DescribeHsm
    DescribeHsm (DescribeHsm'),
    newDescribeHsm,
    DescribeHsmResponse (DescribeHsmResponse'),
    newDescribeHsmResponse,

    -- ** CreateHapg
    CreateHapg (CreateHapg'),
    newCreateHapg,
    CreateHapgResponse (CreateHapgResponse'),
    newCreateHapgResponse,

    -- ** DescribeLunaClient
    DescribeLunaClient (DescribeLunaClient'),
    newDescribeLunaClient,
    DescribeLunaClientResponse (DescribeLunaClientResponse'),
    newDescribeLunaClientResponse,

    -- ** ListHapgs (Paginated)
    ListHapgs (ListHapgs'),
    newListHapgs,
    ListHapgsResponse (ListHapgsResponse'),
    newListHapgsResponse,

    -- ** AddTagsToResource
    AddTagsToResource (AddTagsToResource'),
    newAddTagsToResource,
    AddTagsToResourceResponse (AddTagsToResourceResponse'),
    newAddTagsToResourceResponse,

    -- ** ModifyHsm
    ModifyHsm (ModifyHsm'),
    newModifyHsm,
    ModifyHsmResponse (ModifyHsmResponse'),
    newModifyHsmResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** CreateHsm
    CreateHsm (CreateHsm'),
    newCreateHsm,
    CreateHsmResponse (CreateHsmResponse'),
    newCreateHsmResponse,

    -- * Types

    -- ** ClientVersion
    ClientVersion (..),

    -- ** CloudHsmObjectState
    CloudHsmObjectState (..),

    -- ** HsmStatus
    HsmStatus (..),

    -- ** SubscriptionType
    SubscriptionType (..),

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Network.AWS.CloudHSM.AddTagsToResource
import Network.AWS.CloudHSM.CreateHapg
import Network.AWS.CloudHSM.CreateHsm
import Network.AWS.CloudHSM.CreateLunaClient
import Network.AWS.CloudHSM.DeleteHapg
import Network.AWS.CloudHSM.DeleteHsm
import Network.AWS.CloudHSM.DeleteLunaClient
import Network.AWS.CloudHSM.DescribeHapg
import Network.AWS.CloudHSM.DescribeHsm
import Network.AWS.CloudHSM.DescribeLunaClient
import Network.AWS.CloudHSM.GetConfig
import Network.AWS.CloudHSM.Lens
import Network.AWS.CloudHSM.ListAvailableZones
import Network.AWS.CloudHSM.ListHapgs
import Network.AWS.CloudHSM.ListHsms
import Network.AWS.CloudHSM.ListLunaClients
import Network.AWS.CloudHSM.ListTagsForResource
import Network.AWS.CloudHSM.ModifyHapg
import Network.AWS.CloudHSM.ModifyHsm
import Network.AWS.CloudHSM.ModifyLunaClient
import Network.AWS.CloudHSM.RemoveTagsFromResource
import Network.AWS.CloudHSM.Types
import Network.AWS.CloudHSM.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CloudHSM'.

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
