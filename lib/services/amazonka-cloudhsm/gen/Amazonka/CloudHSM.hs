{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CloudHSM
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2014-05-30@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS CloudHSM Service
--
-- This is documentation for __AWS CloudHSM Classic__. For more
-- information, see
-- <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs>,
-- the
-- <https://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide>,
-- and the
-- <https://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference>.
--
-- __For information about the current version of AWS CloudHSM__, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM>, the
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>,
-- and the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference>.
module Amazonka.CloudHSM
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** CloudHsmInternalException
    _CloudHsmInternalException,

    -- ** CloudHsmServiceException
    _CloudHsmServiceException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AddTagsToResource
    AddTagsToResource (AddTagsToResource'),
    newAddTagsToResource,
    AddTagsToResourceResponse (AddTagsToResourceResponse'),
    newAddTagsToResourceResponse,

    -- ** CreateHapg
    CreateHapg (CreateHapg'),
    newCreateHapg,
    CreateHapgResponse (CreateHapgResponse'),
    newCreateHapgResponse,

    -- ** CreateHsm
    CreateHsm (CreateHsm'),
    newCreateHsm,
    CreateHsmResponse (CreateHsmResponse'),
    newCreateHsmResponse,

    -- ** CreateLunaClient
    CreateLunaClient (CreateLunaClient'),
    newCreateLunaClient,
    CreateLunaClientResponse (CreateLunaClientResponse'),
    newCreateLunaClientResponse,

    -- ** DeleteHapg
    DeleteHapg (DeleteHapg'),
    newDeleteHapg,
    DeleteHapgResponse (DeleteHapgResponse'),
    newDeleteHapgResponse,

    -- ** DeleteHsm
    DeleteHsm (DeleteHsm'),
    newDeleteHsm,
    DeleteHsmResponse (DeleteHsmResponse'),
    newDeleteHsmResponse,

    -- ** DeleteLunaClient
    DeleteLunaClient (DeleteLunaClient'),
    newDeleteLunaClient,
    DeleteLunaClientResponse (DeleteLunaClientResponse'),
    newDeleteLunaClientResponse,

    -- ** DescribeHapg
    DescribeHapg (DescribeHapg'),
    newDescribeHapg,
    DescribeHapgResponse (DescribeHapgResponse'),
    newDescribeHapgResponse,

    -- ** DescribeHsm
    DescribeHsm (DescribeHsm'),
    newDescribeHsm,
    DescribeHsmResponse (DescribeHsmResponse'),
    newDescribeHsmResponse,

    -- ** DescribeLunaClient
    DescribeLunaClient (DescribeLunaClient'),
    newDescribeLunaClient,
    DescribeLunaClientResponse (DescribeLunaClientResponse'),
    newDescribeLunaClientResponse,

    -- ** GetConfig
    GetConfig (GetConfig'),
    newGetConfig,
    GetConfigResponse (GetConfigResponse'),
    newGetConfigResponse,

    -- ** ListAvailableZones
    ListAvailableZones (ListAvailableZones'),
    newListAvailableZones,
    ListAvailableZonesResponse (ListAvailableZonesResponse'),
    newListAvailableZonesResponse,

    -- ** ListHapgs (Paginated)
    ListHapgs (ListHapgs'),
    newListHapgs,
    ListHapgsResponse (ListHapgsResponse'),
    newListHapgsResponse,

    -- ** ListHsms (Paginated)
    ListHsms (ListHsms'),
    newListHsms,
    ListHsmsResponse (ListHsmsResponse'),
    newListHsmsResponse,

    -- ** ListLunaClients (Paginated)
    ListLunaClients (ListLunaClients'),
    newListLunaClients,
    ListLunaClientsResponse (ListLunaClientsResponse'),
    newListLunaClientsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ModifyHapg
    ModifyHapg (ModifyHapg'),
    newModifyHapg,
    ModifyHapgResponse (ModifyHapgResponse'),
    newModifyHapgResponse,

    -- ** ModifyHsm
    ModifyHsm (ModifyHsm'),
    newModifyHsm,
    ModifyHsmResponse (ModifyHsmResponse'),
    newModifyHsmResponse,

    -- ** ModifyLunaClient
    ModifyLunaClient (ModifyLunaClient'),
    newModifyLunaClient,
    ModifyLunaClientResponse (ModifyLunaClientResponse'),
    newModifyLunaClientResponse,

    -- ** RemoveTagsFromResource
    RemoveTagsFromResource (RemoveTagsFromResource'),
    newRemoveTagsFromResource,
    RemoveTagsFromResourceResponse (RemoveTagsFromResourceResponse'),
    newRemoveTagsFromResourceResponse,

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

import Amazonka.CloudHSM.AddTagsToResource
import Amazonka.CloudHSM.CreateHapg
import Amazonka.CloudHSM.CreateHsm
import Amazonka.CloudHSM.CreateLunaClient
import Amazonka.CloudHSM.DeleteHapg
import Amazonka.CloudHSM.DeleteHsm
import Amazonka.CloudHSM.DeleteLunaClient
import Amazonka.CloudHSM.DescribeHapg
import Amazonka.CloudHSM.DescribeHsm
import Amazonka.CloudHSM.DescribeLunaClient
import Amazonka.CloudHSM.GetConfig
import Amazonka.CloudHSM.Lens
import Amazonka.CloudHSM.ListAvailableZones
import Amazonka.CloudHSM.ListHapgs
import Amazonka.CloudHSM.ListHsms
import Amazonka.CloudHSM.ListLunaClients
import Amazonka.CloudHSM.ListTagsForResource
import Amazonka.CloudHSM.ModifyHapg
import Amazonka.CloudHSM.ModifyHsm
import Amazonka.CloudHSM.ModifyLunaClient
import Amazonka.CloudHSM.RemoveTagsFromResource
import Amazonka.CloudHSM.Types
import Amazonka.CloudHSM.Waiters

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
