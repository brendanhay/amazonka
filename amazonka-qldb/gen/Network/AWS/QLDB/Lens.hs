{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.QLDB.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QLDB.Lens
  ( -- * Operations

    -- ** DeleteLedger
    deleteLedger_name,

    -- ** UpdateLedger
    updateLedger_deletionProtection,
    updateLedger_name,
    updateLedgerResponse_deletionProtection,
    updateLedgerResponse_arn,
    updateLedgerResponse_state,
    updateLedgerResponse_name,
    updateLedgerResponse_creationDateTime,
    updateLedgerResponse_httpStatus,

    -- ** ListLedgers
    listLedgers_nextToken,
    listLedgers_maxResults,
    listLedgersResponse_nextToken,
    listLedgersResponse_ledgers,
    listLedgersResponse_httpStatus,

    -- ** CreateLedger
    createLedger_deletionProtection,
    createLedger_tags,
    createLedger_name,
    createLedger_permissionsMode,
    createLedgerResponse_deletionProtection,
    createLedgerResponse_arn,
    createLedgerResponse_state,
    createLedgerResponse_name,
    createLedgerResponse_creationDateTime,
    createLedgerResponse_httpStatus,

    -- ** ExportJournalToS
    exportJournalToS_name,
    exportJournalToS_inclusiveStartTime,
    exportJournalToS_exclusiveEndTime,
    exportJournalToS_s3ExportConfiguration,
    exportJournalToS_roleArn,
    exportJournalToSResponse_httpStatus,
    exportJournalToSResponse_exportId,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** CancelJournalKinesisStream
    cancelJournalKinesisStream_ledgerName,
    cancelJournalKinesisStream_streamId,
    cancelJournalKinesisStreamResponse_streamId,
    cancelJournalKinesisStreamResponse_httpStatus,

    -- ** DescribeLedger
    describeLedger_name,
    describeLedgerResponse_deletionProtection,
    describeLedgerResponse_arn,
    describeLedgerResponse_state,
    describeLedgerResponse_name,
    describeLedgerResponse_creationDateTime,
    describeLedgerResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** DescribeJournalS3Export
    describeJournalS3Export_name,
    describeJournalS3Export_exportId,
    describeJournalS3ExportResponse_httpStatus,
    describeJournalS3ExportResponse_exportDescription,

    -- ** GetRevision
    getRevision_digestTipAddress,
    getRevision_name,
    getRevision_blockAddress,
    getRevision_documentId,
    getRevisionResponse_proof,
    getRevisionResponse_httpStatus,
    getRevisionResponse_revision,

    -- ** ListJournalKinesisStreamsForLedger
    listJournalKinesisStreamsForLedger_nextToken,
    listJournalKinesisStreamsForLedger_maxResults,
    listJournalKinesisStreamsForLedger_ledgerName,
    listJournalKinesisStreamsForLedgerResponse_streams,
    listJournalKinesisStreamsForLedgerResponse_nextToken,
    listJournalKinesisStreamsForLedgerResponse_httpStatus,

    -- ** ListJournalS3ExportsForLedger
    listJournalS3ExportsForLedger_nextToken,
    listJournalS3ExportsForLedger_maxResults,
    listJournalS3ExportsForLedger_name,
    listJournalS3ExportsForLedgerResponse_nextToken,
    listJournalS3ExportsForLedgerResponse_journalS3Exports,
    listJournalS3ExportsForLedgerResponse_httpStatus,

    -- ** GetBlock
    getBlock_digestTipAddress,
    getBlock_name,
    getBlock_blockAddress,
    getBlockResponse_proof,
    getBlockResponse_httpStatus,
    getBlockResponse_block,

    -- ** ListJournalS3Exports
    listJournalS3Exports_nextToken,
    listJournalS3Exports_maxResults,
    listJournalS3ExportsResponse_nextToken,
    listJournalS3ExportsResponse_journalS3Exports,
    listJournalS3ExportsResponse_httpStatus,

    -- ** StreamJournalToKinesis
    streamJournalToKinesis_exclusiveEndTime,
    streamJournalToKinesis_tags,
    streamJournalToKinesis_ledgerName,
    streamJournalToKinesis_roleArn,
    streamJournalToKinesis_inclusiveStartTime,
    streamJournalToKinesis_kinesisConfiguration,
    streamJournalToKinesis_streamName,
    streamJournalToKinesisResponse_streamId,
    streamJournalToKinesisResponse_httpStatus,

    -- ** GetDigest
    getDigest_name,
    getDigestResponse_httpStatus,
    getDigestResponse_digest,
    getDigestResponse_digestTipAddress,

    -- ** DescribeJournalKinesisStream
    describeJournalKinesisStream_ledgerName,
    describeJournalKinesisStream_streamId,
    describeJournalKinesisStreamResponse_stream,
    describeJournalKinesisStreamResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- * Types

    -- ** JournalKinesisStreamDescription
    journalKinesisStreamDescription_creationTime,
    journalKinesisStreamDescription_inclusiveStartTime,
    journalKinesisStreamDescription_errorCause,
    journalKinesisStreamDescription_arn,
    journalKinesisStreamDescription_exclusiveEndTime,
    journalKinesisStreamDescription_ledgerName,
    journalKinesisStreamDescription_roleArn,
    journalKinesisStreamDescription_streamId,
    journalKinesisStreamDescription_status,
    journalKinesisStreamDescription_kinesisConfiguration,
    journalKinesisStreamDescription_streamName,

    -- ** JournalS3ExportDescription
    journalS3ExportDescription_ledgerName,
    journalS3ExportDescription_exportId,
    journalS3ExportDescription_exportCreationTime,
    journalS3ExportDescription_status,
    journalS3ExportDescription_inclusiveStartTime,
    journalS3ExportDescription_exclusiveEndTime,
    journalS3ExportDescription_s3ExportConfiguration,
    journalS3ExportDescription_roleArn,

    -- ** KinesisConfiguration
    kinesisConfiguration_aggregationEnabled,
    kinesisConfiguration_streamArn,

    -- ** LedgerSummary
    ledgerSummary_state,
    ledgerSummary_name,
    ledgerSummary_creationDateTime,

    -- ** S3EncryptionConfiguration
    s3EncryptionConfiguration_kmsKeyArn,
    s3EncryptionConfiguration_objectEncryptionType,

    -- ** S3ExportConfiguration
    s3ExportConfiguration_bucket,
    s3ExportConfiguration_prefix,
    s3ExportConfiguration_encryptionConfiguration,

    -- ** ValueHolder
    valueHolder_ionText,
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
import Network.AWS.QLDB.ListJournalKinesisStreamsForLedger
import Network.AWS.QLDB.ListJournalS3Exports
import Network.AWS.QLDB.ListJournalS3ExportsForLedger
import Network.AWS.QLDB.ListLedgers
import Network.AWS.QLDB.ListTagsForResource
import Network.AWS.QLDB.StreamJournalToKinesis
import Network.AWS.QLDB.TagResource
import Network.AWS.QLDB.Types.JournalKinesisStreamDescription
import Network.AWS.QLDB.Types.JournalS3ExportDescription
import Network.AWS.QLDB.Types.KinesisConfiguration
import Network.AWS.QLDB.Types.LedgerSummary
import Network.AWS.QLDB.Types.S3EncryptionConfiguration
import Network.AWS.QLDB.Types.S3ExportConfiguration
import Network.AWS.QLDB.Types.ValueHolder
import Network.AWS.QLDB.UntagResource
import Network.AWS.QLDB.UpdateLedger
