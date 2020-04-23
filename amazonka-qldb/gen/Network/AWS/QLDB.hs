{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.QLDB
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Quantum Ledger Database (Amazon QLDB) is a fully managed ledger database owned by a central trusted authority that provides a transparent, immutable, and cryptographically verifiable transaction log of all of your application changes. Amazon QLDB tracks every application data change and maintains a complete and verifiable history of changes over time.
--
--
-- Ledgers are typically used to record a history of economic and financial activity in an organization. Many organizations build applications with ledger-like functionality because they want to maintain an accurate history of their applications' data. For example, they might want to track the history of credits and debits in banking transactions, verify the data lineage of an insurance claim, or trace the movement of an item in a supply chain network. Ledger applications are often implemented using custom audit tables or audit trails created in relational databases.
--
-- Amazon QLDB is a new class of database that helps eliminate the need to engage in the complex development effort of building your own ledger-like applications. With QLDB, the history of changes to your data is immutable—it cannot be altered, updated, or deleted. And using cryptography, you can easily verify that there have been no unintended changes to your application’s data. QLDB uses an immutable transactional log, known as a <em>journal. The journal tracks each application data change and maintains a complete and verifiable history of changes over time.
--
module Network.AWS.QLDB
    (
    -- * Service Configuration
      qldb

    -- * Errors
    -- $errors

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** ResourcePreconditionNotMetException
    , _ResourcePreconditionNotMetException

    -- ** ResourceAlreadyExistsException
    , _ResourceAlreadyExistsException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** UpdateLedger
    , module Network.AWS.QLDB.UpdateLedger

    -- ** DeleteLedger
    , module Network.AWS.QLDB.DeleteLedger

    -- ** ListTagsForResource
    , module Network.AWS.QLDB.ListTagsForResource

    -- ** GetRevision
    , module Network.AWS.QLDB.GetRevision

    -- ** DescribeLedger
    , module Network.AWS.QLDB.DescribeLedger

    -- ** ExportJournalToS3
    , module Network.AWS.QLDB.ExportJournalToS3

    -- ** CreateLedger
    , module Network.AWS.QLDB.CreateLedger

    -- ** ListLedgers (Paginated)
    , module Network.AWS.QLDB.ListLedgers

    -- ** ListJournalS3Exports (Paginated)
    , module Network.AWS.QLDB.ListJournalS3Exports

    -- ** GetBlock
    , module Network.AWS.QLDB.GetBlock

    -- ** ListJournalS3ExportsForLedger (Paginated)
    , module Network.AWS.QLDB.ListJournalS3ExportsForLedger

    -- ** DescribeJournalS3Export
    , module Network.AWS.QLDB.DescribeJournalS3Export

    -- ** TagResource
    , module Network.AWS.QLDB.TagResource

    -- ** UntagResource
    , module Network.AWS.QLDB.UntagResource

    -- ** GetDigest
    , module Network.AWS.QLDB.GetDigest

    -- * Types

    -- ** ExportStatus
    , ExportStatus (..)

    -- ** LedgerState
    , LedgerState (..)

    -- ** PermissionsMode
    , PermissionsMode (..)

    -- ** S3ObjectEncryptionType
    , S3ObjectEncryptionType (..)

    -- ** JournalS3ExportDescription
    , JournalS3ExportDescription
    , journalS3ExportDescription
    , jsedLedgerName
    , jsedExportId
    , jsedExportCreationTime
    , jsedStatus
    , jsedInclusiveStartTime
    , jsedExclusiveEndTime
    , jsedS3ExportConfiguration
    , jsedRoleARN

    -- ** LedgerSummary
    , LedgerSummary
    , ledgerSummary
    , lsState
    , lsName
    , lsCreationDateTime

    -- ** S3EncryptionConfiguration
    , S3EncryptionConfiguration
    , s3EncryptionConfiguration
    , secKMSKeyARN
    , secObjectEncryptionType

    -- ** S3ExportConfiguration
    , S3ExportConfiguration
    , s3ExportConfiguration
    , secBucket
    , secPrefix
    , secEncryptionConfiguration

    -- ** ValueHolder
    , ValueHolder
    , valueHolder
    , vhIonText
    ) where

import Network.AWS.QLDB.CreateLedger
import Network.AWS.QLDB.DeleteLedger
import Network.AWS.QLDB.DescribeJournalS3Export
import Network.AWS.QLDB.DescribeLedger
import Network.AWS.QLDB.ExportJournalToS3
import Network.AWS.QLDB.GetBlock
import Network.AWS.QLDB.GetDigest
import Network.AWS.QLDB.GetRevision
import Network.AWS.QLDB.ListJournalS3Exports
import Network.AWS.QLDB.ListJournalS3ExportsForLedger
import Network.AWS.QLDB.ListLedgers
import Network.AWS.QLDB.ListTagsForResource
import Network.AWS.QLDB.TagResource
import Network.AWS.QLDB.Types
import Network.AWS.QLDB.UntagResource
import Network.AWS.QLDB.UpdateLedger
import Network.AWS.QLDB.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'QLDB'.
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
