{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.QLDB
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-01-02@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The control plane for Amazon QLDB
module Network.AWS.QLDB
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourcePreconditionNotMetException
    _ResourcePreconditionNotMetException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteLedger
    DeleteLedger (DeleteLedger'),
    newDeleteLedger,
    DeleteLedgerResponse (DeleteLedgerResponse'),
    newDeleteLedgerResponse,

    -- ** UpdateLedger
    UpdateLedger (UpdateLedger'),
    newUpdateLedger,
    UpdateLedgerResponse (UpdateLedgerResponse'),
    newUpdateLedgerResponse,

    -- ** ListLedgers
    ListLedgers (ListLedgers'),
    newListLedgers,
    ListLedgersResponse (ListLedgersResponse'),
    newListLedgersResponse,

    -- ** CreateLedger
    CreateLedger (CreateLedger'),
    newCreateLedger,
    CreateLedgerResponse (CreateLedgerResponse'),
    newCreateLedgerResponse,

    -- ** ExportJournalToS
    ExportJournalToS (ExportJournalToS'),
    newExportJournalToS,
    ExportJournalToSResponse (ExportJournalToSResponse'),
    newExportJournalToSResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** CancelJournalKinesisStream
    CancelJournalKinesisStream (CancelJournalKinesisStream'),
    newCancelJournalKinesisStream,
    CancelJournalKinesisStreamResponse (CancelJournalKinesisStreamResponse'),
    newCancelJournalKinesisStreamResponse,

    -- ** DescribeLedger
    DescribeLedger (DescribeLedger'),
    newDescribeLedger,
    DescribeLedgerResponse (DescribeLedgerResponse'),
    newDescribeLedgerResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DescribeJournalS3Export
    DescribeJournalS3Export (DescribeJournalS3Export'),
    newDescribeJournalS3Export,
    DescribeJournalS3ExportResponse (DescribeJournalS3ExportResponse'),
    newDescribeJournalS3ExportResponse,

    -- ** GetRevision
    GetRevision (GetRevision'),
    newGetRevision,
    GetRevisionResponse (GetRevisionResponse'),
    newGetRevisionResponse,

    -- ** ListJournalKinesisStreamsForLedger
    ListJournalKinesisStreamsForLedger (ListJournalKinesisStreamsForLedger'),
    newListJournalKinesisStreamsForLedger,
    ListJournalKinesisStreamsForLedgerResponse (ListJournalKinesisStreamsForLedgerResponse'),
    newListJournalKinesisStreamsForLedgerResponse,

    -- ** ListJournalS3ExportsForLedger
    ListJournalS3ExportsForLedger (ListJournalS3ExportsForLedger'),
    newListJournalS3ExportsForLedger,
    ListJournalS3ExportsForLedgerResponse (ListJournalS3ExportsForLedgerResponse'),
    newListJournalS3ExportsForLedgerResponse,

    -- ** GetBlock
    GetBlock (GetBlock'),
    newGetBlock,
    GetBlockResponse (GetBlockResponse'),
    newGetBlockResponse,

    -- ** ListJournalS3Exports
    ListJournalS3Exports (ListJournalS3Exports'),
    newListJournalS3Exports,
    ListJournalS3ExportsResponse (ListJournalS3ExportsResponse'),
    newListJournalS3ExportsResponse,

    -- ** StreamJournalToKinesis
    StreamJournalToKinesis (StreamJournalToKinesis'),
    newStreamJournalToKinesis,
    StreamJournalToKinesisResponse (StreamJournalToKinesisResponse'),
    newStreamJournalToKinesisResponse,

    -- ** GetDigest
    GetDigest (GetDigest'),
    newGetDigest,
    GetDigestResponse (GetDigestResponse'),
    newGetDigestResponse,

    -- ** DescribeJournalKinesisStream
    DescribeJournalKinesisStream (DescribeJournalKinesisStream'),
    newDescribeJournalKinesisStream,
    DescribeJournalKinesisStreamResponse (DescribeJournalKinesisStreamResponse'),
    newDescribeJournalKinesisStreamResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- * Types

    -- ** ErrorCause
    ErrorCause (..),

    -- ** ExportStatus
    ExportStatus (..),

    -- ** LedgerState
    LedgerState (..),

    -- ** PermissionsMode
    PermissionsMode (..),

    -- ** S3ObjectEncryptionType
    S3ObjectEncryptionType (..),

    -- ** StreamStatus
    StreamStatus (..),

    -- ** JournalKinesisStreamDescription
    JournalKinesisStreamDescription (JournalKinesisStreamDescription'),
    newJournalKinesisStreamDescription,

    -- ** JournalS3ExportDescription
    JournalS3ExportDescription (JournalS3ExportDescription'),
    newJournalS3ExportDescription,

    -- ** KinesisConfiguration
    KinesisConfiguration (KinesisConfiguration'),
    newKinesisConfiguration,

    -- ** LedgerSummary
    LedgerSummary (LedgerSummary'),
    newLedgerSummary,

    -- ** S3EncryptionConfiguration
    S3EncryptionConfiguration (S3EncryptionConfiguration'),
    newS3EncryptionConfiguration,

    -- ** S3ExportConfiguration
    S3ExportConfiguration (S3ExportConfiguration'),
    newS3ExportConfiguration,

    -- ** ValueHolder
    ValueHolder (ValueHolder'),
    newValueHolder,
  )
where

import Network.AWS.QLDB.CancelJournalKinesisStream
import Network.AWS.QLDB.CreateLedger
import Network.AWS.QLDB.DeleteLedger
import Network.AWS.QLDB.DescribeJournalKinesisStream
import Network.AWS.QLDB.DescribeJournalS3Export
import Network.AWS.QLDB.DescribeLedger
import Network.AWS.QLDB.ExportJournalToS
import Network.AWS.QLDB.GetBlock
import Network.AWS.QLDB.GetDigest
import Network.AWS.QLDB.GetRevision
import Network.AWS.QLDB.Lens
import Network.AWS.QLDB.ListJournalKinesisStreamsForLedger
import Network.AWS.QLDB.ListJournalS3Exports
import Network.AWS.QLDB.ListJournalS3ExportsForLedger
import Network.AWS.QLDB.ListLedgers
import Network.AWS.QLDB.ListTagsForResource
import Network.AWS.QLDB.StreamJournalToKinesis
import Network.AWS.QLDB.TagResource
import Network.AWS.QLDB.Types
import Network.AWS.QLDB.UntagResource
import Network.AWS.QLDB.UpdateLedger
import Network.AWS.QLDB.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'QLDB'.

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
