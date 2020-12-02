{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS CloudHSM Service__
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
--
module Network.AWS.CloudHSM
    (
    -- * Service Configuration
      cloudHSM

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** CloudHSMServiceException
    , _CloudHSMServiceException

    -- ** CloudHSMInternalException
    , _CloudHSMInternalException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteHAPG
    , module Network.AWS.CloudHSM.DeleteHAPG

    -- ** ListHAPGs
    , module Network.AWS.CloudHSM.ListHAPGs

    -- ** ModifyLunaClient
    , module Network.AWS.CloudHSM.ModifyLunaClient

    -- ** ListHSMs
    , module Network.AWS.CloudHSM.ListHSMs

    -- ** DescribeLunaClient
    , module Network.AWS.CloudHSM.DescribeLunaClient

    -- ** ListTagsForResource
    , module Network.AWS.CloudHSM.ListTagsForResource

    -- ** CreateHAPG
    , module Network.AWS.CloudHSM.CreateHAPG

    -- ** CreateHSM
    , module Network.AWS.CloudHSM.CreateHSM

    -- ** RemoveTagsFromResource
    , module Network.AWS.CloudHSM.RemoveTagsFromResource

    -- ** DescribeHAPG
    , module Network.AWS.CloudHSM.DescribeHAPG

    -- ** CreateLunaClient
    , module Network.AWS.CloudHSM.CreateLunaClient

    -- ** ListLunaClients
    , module Network.AWS.CloudHSM.ListLunaClients

    -- ** AddTagsToResource
    , module Network.AWS.CloudHSM.AddTagsToResource

    -- ** GetConfig
    , module Network.AWS.CloudHSM.GetConfig

    -- ** DeleteHSM
    , module Network.AWS.CloudHSM.DeleteHSM

    -- ** DescribeHSM
    , module Network.AWS.CloudHSM.DescribeHSM

    -- ** ModifyHAPG
    , module Network.AWS.CloudHSM.ModifyHAPG

    -- ** DeleteLunaClient
    , module Network.AWS.CloudHSM.DeleteLunaClient

    -- ** ModifyHSM
    , module Network.AWS.CloudHSM.ModifyHSM

    -- ** ListAvailableZones
    , module Network.AWS.CloudHSM.ListAvailableZones

    -- * Types

    -- ** ClientVersion
    , ClientVersion (..)

    -- ** CloudHSMObjectState
    , CloudHSMObjectState (..)

    -- ** HSMStatus
    , HSMStatus (..)

    -- ** SubscriptionType
    , SubscriptionType (..)

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue
    ) where

import Network.AWS.CloudHSM.AddTagsToResource
import Network.AWS.CloudHSM.CreateHAPG
import Network.AWS.CloudHSM.CreateHSM
import Network.AWS.CloudHSM.CreateLunaClient
import Network.AWS.CloudHSM.DeleteHAPG
import Network.AWS.CloudHSM.DeleteHSM
import Network.AWS.CloudHSM.DeleteLunaClient
import Network.AWS.CloudHSM.DescribeHAPG
import Network.AWS.CloudHSM.DescribeHSM
import Network.AWS.CloudHSM.DescribeLunaClient
import Network.AWS.CloudHSM.GetConfig
import Network.AWS.CloudHSM.ListAvailableZones
import Network.AWS.CloudHSM.ListHAPGs
import Network.AWS.CloudHSM.ListHSMs
import Network.AWS.CloudHSM.ListLunaClients
import Network.AWS.CloudHSM.ListTagsForResource
import Network.AWS.CloudHSM.ModifyHAPG
import Network.AWS.CloudHSM.ModifyHSM
import Network.AWS.CloudHSM.ModifyLunaClient
import Network.AWS.CloudHSM.RemoveTagsFromResource
import Network.AWS.CloudHSM.Types
import Network.AWS.CloudHSM.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CloudHSM'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
