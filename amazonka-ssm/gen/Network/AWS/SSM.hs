{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon EC2 Simple Systems Manager (SSM) enables you to configure and
-- manage your EC2 instances. You can create a configuration document and
-- then associate it with one or more running instances.
--
-- You can use a configuration document to automate the following tasks for
-- your Windows instances:
--
-- -   Join an AWS Directory
--
-- -   Install, repair, or uninstall software using an MSI package
--
-- -   Run PowerShell scripts
--
-- -   Configure CloudWatch Logs to monitor applications and systems
--
-- Note that configuration documents are not supported on Linux instances.
--
-- /See:/ <http://docs.aws.amazon.com/ssm/latest/APIReference/Welcome.html AWS API Reference>
module Network.AWS.SSM
    (
    -- * Service Configuration
      sSM

    -- * Errors
    -- $errors

    -- ** AssociatedInstances
    , _AssociatedInstances

    -- ** InvalidInstanceId
    , _InvalidInstanceId

    -- ** StatusUnchanged
    , _StatusUnchanged

    -- ** InvalidNextToken
    , _InvalidNextToken

    -- ** DuplicateInstanceId
    , _DuplicateInstanceId

    -- ** InvalidDocument
    , _InvalidDocument

    -- ** AssociationAlreadyExists
    , _AssociationAlreadyExists

    -- ** InvalidDocumentContent
    , _InvalidDocumentContent

    -- ** AssociationLimitExceeded
    , _AssociationLimitExceeded

    -- ** AssociationDoesNotExist
    , _AssociationDoesNotExist

    -- ** InternalServerError
    , _InternalServerError

    -- ** TooManyUpdates
    , _TooManyUpdates

    -- ** MaxDocumentSizeExceeded
    , _MaxDocumentSizeExceeded

    -- ** DocumentAlreadyExists
    , _DocumentAlreadyExists

    -- ** DocumentLimitExceeded
    , _DocumentLimitExceeded

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeDocument
    , module Network.AWS.SSM.DescribeDocument

    -- ** CreateAssociation
    , module Network.AWS.SSM.CreateAssociation

    -- ** CreateDocument
    , module Network.AWS.SSM.CreateDocument

    -- ** ListDocuments
    , module Network.AWS.SSM.ListDocuments

    -- ** GetDocument
    , module Network.AWS.SSM.GetDocument

    -- ** DescribeAssociation
    , module Network.AWS.SSM.DescribeAssociation

    -- ** UpdateAssociationStatus
    , module Network.AWS.SSM.UpdateAssociationStatus

    -- ** ListAssociations
    , module Network.AWS.SSM.ListAssociations

    -- ** DeleteAssociation
    , module Network.AWS.SSM.DeleteAssociation

    -- ** DeleteDocument
    , module Network.AWS.SSM.DeleteDocument

    -- ** CreateAssociationBatch
    , module Network.AWS.SSM.CreateAssociationBatch

    -- * Types

    -- ** AssociationFilterKey
    , AssociationFilterKey (..)

    -- ** AssociationStatusName
    , AssociationStatusName (..)

    -- ** DocumentFilterKey
    , DocumentFilterKey (..)

    -- ** DocumentStatus
    , DocumentStatus (..)

    -- ** Fault
    , Fault (..)

    -- ** Association
    , Association
    , association
    , aInstanceId
    , aName

    -- ** AssociationDescription
    , AssociationDescription
    , associationDescription
    , adInstanceId
    , adStatus
    , adDate
    , adName

    -- ** AssociationFilter
    , AssociationFilter
    , associationFilter
    , afKey
    , afValue

    -- ** AssociationStatus
    , AssociationStatus
    , associationStatus
    , asAdditionalInfo
    , asDate
    , asName
    , asMessage

    -- ** CreateAssociationBatchRequestEntry
    , CreateAssociationBatchRequestEntry
    , createAssociationBatchRequestEntry
    , cabreInstanceId
    , cabreName

    -- ** DocumentDescription
    , DocumentDescription
    , documentDescription
    , dStatus
    , dSha1
    , dCreatedDate
    , dName

    -- ** DocumentFilter
    , DocumentFilter
    , documentFilter
    , dfKey
    , dfValue

    -- ** DocumentIdentifier
    , DocumentIdentifier
    , documentIdentifier
    , diName

    -- ** FailedCreateAssociation
    , FailedCreateAssociation
    , failedCreateAssociation
    , fcaEntry
    , fcaFault
    , fcaMessage
    ) where

import           Network.AWS.SSM.CreateAssociation
import           Network.AWS.SSM.CreateAssociationBatch
import           Network.AWS.SSM.CreateDocument
import           Network.AWS.SSM.DeleteAssociation
import           Network.AWS.SSM.DeleteDocument
import           Network.AWS.SSM.DescribeAssociation
import           Network.AWS.SSM.DescribeDocument
import           Network.AWS.SSM.GetDocument
import           Network.AWS.SSM.ListAssociations
import           Network.AWS.SSM.ListDocuments
import           Network.AWS.SSM.Types
import           Network.AWS.SSM.UpdateAssociationStatus
import           Network.AWS.SSM.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'SSM'.
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
