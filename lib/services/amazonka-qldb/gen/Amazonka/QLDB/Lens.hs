{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.QLDB.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDB.Lens
  ( -- * Operations

    -- ** CancelJournalKinesisStream
    cancelJournalKinesisStream_ledgerName,
    cancelJournalKinesisStream_streamId,
    cancelJournalKinesisStreamResponse_streamId,
    cancelJournalKinesisStreamResponse_httpStatus,

    -- ** CreateLedger
    createLedger_deletionProtection,
    createLedger_kmsKey,
    createLedger_tags,
    createLedger_name,
    createLedger_permissionsMode,
    createLedgerResponse_arn,
    createLedgerResponse_creationDateTime,
    createLedgerResponse_deletionProtection,
    createLedgerResponse_kmsKeyArn,
    createLedgerResponse_name,
    createLedgerResponse_permissionsMode,
    createLedgerResponse_state,
    createLedgerResponse_httpStatus,

    -- ** DeleteLedger
    deleteLedger_name,

    -- ** DescribeJournalKinesisStream
    describeJournalKinesisStream_ledgerName,
    describeJournalKinesisStream_streamId,
    describeJournalKinesisStreamResponse_stream,
    describeJournalKinesisStreamResponse_httpStatus,

    -- ** DescribeJournalS3Export
    describeJournalS3Export_name,
    describeJournalS3Export_exportId,
    describeJournalS3ExportResponse_httpStatus,
    describeJournalS3ExportResponse_exportDescription,

    -- ** DescribeLedger
    describeLedger_name,
    describeLedgerResponse_arn,
    describeLedgerResponse_creationDateTime,
    describeLedgerResponse_deletionProtection,
    describeLedgerResponse_encryptionDescription,
    describeLedgerResponse_name,
    describeLedgerResponse_permissionsMode,
    describeLedgerResponse_state,
    describeLedgerResponse_httpStatus,

    -- ** ExportJournalToS3
    exportJournalToS3_outputFormat,
    exportJournalToS3_name,
    exportJournalToS3_inclusiveStartTime,
    exportJournalToS3_exclusiveEndTime,
    exportJournalToS3_s3ExportConfiguration,
    exportJournalToS3_roleArn,
    exportJournalToS3Response_httpStatus,
    exportJournalToS3Response_exportId,

    -- ** GetBlock
    getBlock_digestTipAddress,
    getBlock_name,
    getBlock_blockAddress,
    getBlockResponse_proof,
    getBlockResponse_httpStatus,
    getBlockResponse_block,

    -- ** GetDigest
    getDigest_name,
    getDigestResponse_httpStatus,
    getDigestResponse_digest,
    getDigestResponse_digestTipAddress,

    -- ** GetRevision
    getRevision_digestTipAddress,
    getRevision_name,
    getRevision_blockAddress,
    getRevision_documentId,
    getRevisionResponse_proof,
    getRevisionResponse_httpStatus,
    getRevisionResponse_revision,

    -- ** ListJournalKinesisStreamsForLedger
    listJournalKinesisStreamsForLedger_maxResults,
    listJournalKinesisStreamsForLedger_nextToken,
    listJournalKinesisStreamsForLedger_ledgerName,
    listJournalKinesisStreamsForLedgerResponse_nextToken,
    listJournalKinesisStreamsForLedgerResponse_streams,
    listJournalKinesisStreamsForLedgerResponse_httpStatus,

    -- ** ListJournalS3Exports
    listJournalS3Exports_maxResults,
    listJournalS3Exports_nextToken,
    listJournalS3ExportsResponse_journalS3Exports,
    listJournalS3ExportsResponse_nextToken,
    listJournalS3ExportsResponse_httpStatus,

    -- ** ListJournalS3ExportsForLedger
    listJournalS3ExportsForLedger_maxResults,
    listJournalS3ExportsForLedger_nextToken,
    listJournalS3ExportsForLedger_name,
    listJournalS3ExportsForLedgerResponse_journalS3Exports,
    listJournalS3ExportsForLedgerResponse_nextToken,
    listJournalS3ExportsForLedgerResponse_httpStatus,

    -- ** ListLedgers
    listLedgers_maxResults,
    listLedgers_nextToken,
    listLedgersResponse_ledgers,
    listLedgersResponse_nextToken,
    listLedgersResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

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

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateLedger
    updateLedger_deletionProtection,
    updateLedger_kmsKey,
    updateLedger_name,
    updateLedgerResponse_arn,
    updateLedgerResponse_creationDateTime,
    updateLedgerResponse_deletionProtection,
    updateLedgerResponse_encryptionDescription,
    updateLedgerResponse_name,
    updateLedgerResponse_state,
    updateLedgerResponse_httpStatus,

    -- ** UpdateLedgerPermissionsMode
    updateLedgerPermissionsMode_name,
    updateLedgerPermissionsMode_permissionsMode,
    updateLedgerPermissionsModeResponse_arn,
    updateLedgerPermissionsModeResponse_name,
    updateLedgerPermissionsModeResponse_permissionsMode,
    updateLedgerPermissionsModeResponse_httpStatus,

    -- * Types

    -- ** JournalKinesisStreamDescription
    journalKinesisStreamDescription_arn,
    journalKinesisStreamDescription_creationTime,
    journalKinesisStreamDescription_errorCause,
    journalKinesisStreamDescription_exclusiveEndTime,
    journalKinesisStreamDescription_inclusiveStartTime,
    journalKinesisStreamDescription_ledgerName,
    journalKinesisStreamDescription_roleArn,
    journalKinesisStreamDescription_streamId,
    journalKinesisStreamDescription_status,
    journalKinesisStreamDescription_kinesisConfiguration,
    journalKinesisStreamDescription_streamName,

    -- ** JournalS3ExportDescription
    journalS3ExportDescription_outputFormat,
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

    -- ** LedgerEncryptionDescription
    ledgerEncryptionDescription_inaccessibleKmsKeyDateTime,
    ledgerEncryptionDescription_kmsKeyArn,
    ledgerEncryptionDescription_encryptionStatus,

    -- ** LedgerSummary
    ledgerSummary_creationDateTime,
    ledgerSummary_name,
    ledgerSummary_state,

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
import Amazonka.QLDB.ListJournalKinesisStreamsForLedger
import Amazonka.QLDB.ListJournalS3Exports
import Amazonka.QLDB.ListJournalS3ExportsForLedger
import Amazonka.QLDB.ListLedgers
import Amazonka.QLDB.ListTagsForResource
import Amazonka.QLDB.StreamJournalToKinesis
import Amazonka.QLDB.TagResource
import Amazonka.QLDB.Types.JournalKinesisStreamDescription
import Amazonka.QLDB.Types.JournalS3ExportDescription
import Amazonka.QLDB.Types.KinesisConfiguration
import Amazonka.QLDB.Types.LedgerEncryptionDescription
import Amazonka.QLDB.Types.LedgerSummary
import Amazonka.QLDB.Types.S3EncryptionConfiguration
import Amazonka.QLDB.Types.S3ExportConfiguration
import Amazonka.QLDB.Types.ValueHolder
import Amazonka.QLDB.UntagResource
import Amazonka.QLDB.UpdateLedger
import Amazonka.QLDB.UpdateLedgerPermissionsMode
