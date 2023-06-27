{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.QLDB
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-01-02@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The resource management API for Amazon QLDB
module Amazonka.QLDB
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourcePreconditionNotMetException
    _ResourcePreconditionNotMetException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CancelJournalKinesisStream
    CancelJournalKinesisStream (CancelJournalKinesisStream'),
    newCancelJournalKinesisStream,
    CancelJournalKinesisStreamResponse (CancelJournalKinesisStreamResponse'),
    newCancelJournalKinesisStreamResponse,

    -- ** CreateLedger
    CreateLedger (CreateLedger'),
    newCreateLedger,
    CreateLedgerResponse (CreateLedgerResponse'),
    newCreateLedgerResponse,

    -- ** DeleteLedger
    DeleteLedger (DeleteLedger'),
    newDeleteLedger,
    DeleteLedgerResponse (DeleteLedgerResponse'),
    newDeleteLedgerResponse,

    -- ** DescribeJournalKinesisStream
    DescribeJournalKinesisStream (DescribeJournalKinesisStream'),
    newDescribeJournalKinesisStream,
    DescribeJournalKinesisStreamResponse (DescribeJournalKinesisStreamResponse'),
    newDescribeJournalKinesisStreamResponse,

    -- ** DescribeJournalS3Export
    DescribeJournalS3Export (DescribeJournalS3Export'),
    newDescribeJournalS3Export,
    DescribeJournalS3ExportResponse (DescribeJournalS3ExportResponse'),
    newDescribeJournalS3ExportResponse,

    -- ** DescribeLedger
    DescribeLedger (DescribeLedger'),
    newDescribeLedger,
    DescribeLedgerResponse (DescribeLedgerResponse'),
    newDescribeLedgerResponse,

    -- ** ExportJournalToS3
    ExportJournalToS3 (ExportJournalToS3'),
    newExportJournalToS3,
    ExportJournalToS3Response (ExportJournalToS3Response'),
    newExportJournalToS3Response,

    -- ** GetBlock
    GetBlock (GetBlock'),
    newGetBlock,
    GetBlockResponse (GetBlockResponse'),
    newGetBlockResponse,

    -- ** GetDigest
    GetDigest (GetDigest'),
    newGetDigest,
    GetDigestResponse (GetDigestResponse'),
    newGetDigestResponse,

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

    -- ** ListJournalS3Exports
    ListJournalS3Exports (ListJournalS3Exports'),
    newListJournalS3Exports,
    ListJournalS3ExportsResponse (ListJournalS3ExportsResponse'),
    newListJournalS3ExportsResponse,

    -- ** ListJournalS3ExportsForLedger
    ListJournalS3ExportsForLedger (ListJournalS3ExportsForLedger'),
    newListJournalS3ExportsForLedger,
    ListJournalS3ExportsForLedgerResponse (ListJournalS3ExportsForLedgerResponse'),
    newListJournalS3ExportsForLedgerResponse,

    -- ** ListLedgers
    ListLedgers (ListLedgers'),
    newListLedgers,
    ListLedgersResponse (ListLedgersResponse'),
    newListLedgersResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StreamJournalToKinesis
    StreamJournalToKinesis (StreamJournalToKinesis'),
    newStreamJournalToKinesis,
    StreamJournalToKinesisResponse (StreamJournalToKinesisResponse'),
    newStreamJournalToKinesisResponse,

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

    -- ** UpdateLedger
    UpdateLedger (UpdateLedger'),
    newUpdateLedger,
    UpdateLedgerResponse (UpdateLedgerResponse'),
    newUpdateLedgerResponse,

    -- ** UpdateLedgerPermissionsMode
    UpdateLedgerPermissionsMode (UpdateLedgerPermissionsMode'),
    newUpdateLedgerPermissionsMode,
    UpdateLedgerPermissionsModeResponse (UpdateLedgerPermissionsModeResponse'),
    newUpdateLedgerPermissionsModeResponse,

    -- * Types

    -- ** EncryptionStatus
    EncryptionStatus (..),

    -- ** ErrorCause
    ErrorCause (..),

    -- ** ExportStatus
    ExportStatus (..),

    -- ** LedgerState
    LedgerState (..),

    -- ** OutputFormat
    OutputFormat (..),

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

    -- ** LedgerEncryptionDescription
    LedgerEncryptionDescription (LedgerEncryptionDescription'),
    newLedgerEncryptionDescription,

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

import Amazonka.QLDB.CancelJournalKinesisStream
import Amazonka.QLDB.CreateLedger
import Amazonka.QLDB.DeleteLedger
import Amazonka.QLDB.DescribeJournalKinesisStream
import Amazonka.QLDB.DescribeJournalS3Export
import Amazonka.QLDB.DescribeLedger
import Amazonka.QLDB.ExportJournalToS3
import Amazonka.QLDB.GetBlock
import Amazonka.QLDB.GetDigest
import Amazonka.QLDB.GetRevision
import Amazonka.QLDB.Lens
import Amazonka.QLDB.ListJournalKinesisStreamsForLedger
import Amazonka.QLDB.ListJournalS3Exports
import Amazonka.QLDB.ListJournalS3ExportsForLedger
import Amazonka.QLDB.ListLedgers
import Amazonka.QLDB.ListTagsForResource
import Amazonka.QLDB.StreamJournalToKinesis
import Amazonka.QLDB.TagResource
import Amazonka.QLDB.Types
import Amazonka.QLDB.UntagResource
import Amazonka.QLDB.UpdateLedger
import Amazonka.QLDB.UpdateLedgerPermissionsMode
import Amazonka.QLDB.Waiters

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
