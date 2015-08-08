{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- AWS Import\/Export Service AWS Import\/Export accelerates transferring
-- large amounts of data between the AWS cloud and portable storage devices
-- that you mail to us. AWS Import\/Export transfers data directly onto and
-- off of your storage devices using Amazon\'s high-speed internal network
-- and bypassing the Internet. For large data sets, AWS Import\/Export is
-- often faster than Internet transfer and more cost effective than
-- upgrading your connectivity.
--
-- /See:/ <http://docs.aws.amazon.com/AWSImportExport/latest/DG/api-reference.html AWS API Reference>
module Network.AWS.ImportExport
    (
    -- * Service Description
      ImportExport

    -- * Error Matchers
    -- $errors
    , _InvalidJobIdException
    , _InvalidParameterException
    , _ExpiredJobIdException
    , _InvalidFileSystemException
    , _InvalidAccessKeyIdException
    , _UnableToUpdateJobIdException
    , _UnableToCancelJobIdException
    , _InvalidVersionException
    , _MultipleRegionsException
    , _MalformedManifestException
    , _CanceledJobIdException
    , _BucketPermissionException
    , _MissingParameterException
    , _NoSuchBucketException
    , _InvalidAddressException
    , _InvalidManifestFieldException
    , _MissingCustomsException
    , _InvalidCustomsException
    , _MissingManifestFieldException
    , _CreateJobQuotaExceededException

    -- * Operations
    -- $operations

    -- ** GetShippingLabel
    , module Network.AWS.ImportExport.GetShippingLabel

    -- ** CreateJob
    , module Network.AWS.ImportExport.CreateJob

    -- ** ListJobs (Paginated)
    , module Network.AWS.ImportExport.ListJobs
    -- $pager

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

import           Network.AWS.ImportExport.CancelJob
import           Network.AWS.ImportExport.CreateJob
import           Network.AWS.ImportExport.GetShippingLabel
import           Network.AWS.ImportExport.GetStatus
import           Network.AWS.ImportExport.ListJobs
import           Network.AWS.ImportExport.Types
import           Network.AWS.ImportExport.UpdateJob
import           Network.AWS.ImportExport.Waiters

{- $errors
Error matchers are intended to be used with the <http://hackage.haskell.org/package/lens lens>
library functions provided by the "Control.Exception.Lens" module. This allows
the user to catch (and rethrow) service specific errors returned by 'ImportExport'.
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
Waiters poll by repeatedly send a request until some remote success condition
specified by the 'Wait' configuration is fulfilled. The 'Wait' configuration
specifies how many attempts should be made, in addition to delay and retry strategies.
-}

{- $pager
This operation can return paginated results.
-}
