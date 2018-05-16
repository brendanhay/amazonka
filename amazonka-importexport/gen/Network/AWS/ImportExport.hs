{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Import/Export Service__
module Network.AWS.ImportExport
    (
    -- * Service Configuration
      importExport

    -- * Errors
    -- $errors

    -- ** InvalidJobIdException
    , _InvalidJobIdException

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** ExpiredJobIdException
    , _ExpiredJobIdException

    -- ** InvalidFileSystemException
    , _InvalidFileSystemException

    -- ** InvalidAccessKeyIdException
    , _InvalidAccessKeyIdException

    -- ** UnableToUpdateJobIdException
    , _UnableToUpdateJobIdException

    -- ** UnableToCancelJobIdException
    , _UnableToCancelJobIdException

    -- ** MultipleRegionsException
    , _MultipleRegionsException

    -- ** InvalidVersionException
    , _InvalidVersionException

    -- ** MalformedManifestException
    , _MalformedManifestException

    -- ** MissingParameterException
    , _MissingParameterException

    -- ** CanceledJobIdException
    , _CanceledJobIdException

    -- ** BucketPermissionException
    , _BucketPermissionException

    -- ** NoSuchBucketException
    , _NoSuchBucketException

    -- ** InvalidAddressException
    , _InvalidAddressException

    -- ** MissingCustomsException
    , _MissingCustomsException

    -- ** InvalidManifestFieldException
    , _InvalidManifestFieldException

    -- ** InvalidCustomsException
    , _InvalidCustomsException

    -- ** MissingManifestFieldException
    , _MissingManifestFieldException

    -- ** CreateJobQuotaExceededException
    , _CreateJobQuotaExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetShippingLabel
    , module Network.AWS.ImportExport.GetShippingLabel

    -- ** CreateJob
    , module Network.AWS.ImportExport.CreateJob

    -- ** ListJobs (Paginated)
    , module Network.AWS.ImportExport.ListJobs

    -- ** UpdateJob
    , module Network.AWS.ImportExport.UpdateJob

    -- ** GetStatus
    , module Network.AWS.ImportExport.GetStatus

    -- ** CancelJob
    , module Network.AWS.ImportExport.CancelJob

    -- * Types

    -- ** JobType
    , JobType (..)

    -- ** Artifact
    , Artifact
    , artifact
    , aURL
    , aDescription

    -- ** Job
    , Job
    , job
    , jobJobType
    , jobJobId
    , jobIsCanceled
    , jobCreationDate
    ) where

import Network.AWS.ImportExport.CancelJob
import Network.AWS.ImportExport.CreateJob
import Network.AWS.ImportExport.GetShippingLabel
import Network.AWS.ImportExport.GetStatus
import Network.AWS.ImportExport.ListJobs
import Network.AWS.ImportExport.Types
import Network.AWS.ImportExport.UpdateJob
import Network.AWS.ImportExport.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'ImportExport'.
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
