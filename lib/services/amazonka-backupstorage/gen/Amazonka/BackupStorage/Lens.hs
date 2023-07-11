{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.BackupStorage.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BackupStorage.Lens
  ( -- * Operations

    -- ** DeleteObject
    deleteObject_backupJobId,
    deleteObject_objectName,

    -- ** GetChunk
    getChunk_storageJobId,
    getChunk_chunkToken,
    getChunkResponse_httpStatus,
    getChunkResponse_length,
    getChunkResponse_checksum,
    getChunkResponse_checksumAlgorithm,
    getChunkResponse_data,

    -- ** GetObjectMetadata
    getObjectMetadata_storageJobId,
    getObjectMetadata_objectToken,
    getObjectMetadataResponse_metadataBlobChecksum,
    getObjectMetadataResponse_metadataBlobChecksumAlgorithm,
    getObjectMetadataResponse_metadataBlobLength,
    getObjectMetadataResponse_metadataString,
    getObjectMetadataResponse_httpStatus,
    getObjectMetadataResponse_metadataBlob,

    -- ** ListChunks
    listChunks_maxResults,
    listChunks_nextToken,
    listChunks_storageJobId,
    listChunks_objectToken,
    listChunksResponse_nextToken,
    listChunksResponse_httpStatus,
    listChunksResponse_chunkList,

    -- ** ListObjects
    listObjects_createdAfter,
    listObjects_createdBefore,
    listObjects_maxResults,
    listObjects_nextToken,
    listObjects_startingObjectName,
    listObjects_startingObjectPrefix,
    listObjects_storageJobId,
    listObjectsResponse_nextToken,
    listObjectsResponse_httpStatus,
    listObjectsResponse_objectList,

    -- ** NotifyObjectComplete
    notifyObjectComplete_metadataBlobChecksum,
    notifyObjectComplete_metadataBlobChecksumAlgorithm,
    notifyObjectComplete_metadataBlobLength,
    notifyObjectComplete_metadataString,
    notifyObjectComplete_backupJobId,
    notifyObjectComplete_uploadId,
    notifyObjectComplete_objectChecksum,
    notifyObjectComplete_objectChecksumAlgorithm,
    notifyObjectComplete_metadataBlob,
    notifyObjectCompleteResponse_httpStatus,
    notifyObjectCompleteResponse_objectChecksum,
    notifyObjectCompleteResponse_objectChecksumAlgorithm,

    -- ** PutChunk
    putChunk_backupJobId,
    putChunk_uploadId,
    putChunk_chunkIndex,
    putChunk_length,
    putChunk_checksum,
    putChunk_checksumAlgorithm,
    putChunk_data,
    putChunkResponse_httpStatus,
    putChunkResponse_chunkChecksum,
    putChunkResponse_chunkChecksumAlgorithm,

    -- ** PutObject
    putObject_inlineChunkChecksum,
    putObject_inlineChunkChecksumAlgorithm,
    putObject_inlineChunkLength,
    putObject_metadataString,
    putObject_objectChecksum,
    putObject_objectChecksumAlgorithm,
    putObject_throwOnDuplicate,
    putObject_backupJobId,
    putObject_objectName,
    putObject_inlineChunk,
    putObjectResponse_httpStatus,
    putObjectResponse_inlineChunkChecksum,
    putObjectResponse_inlineChunkChecksumAlgorithm,
    putObjectResponse_objectChecksum,
    putObjectResponse_objectChecksumAlgorithm,

    -- ** StartObject
    startObject_throwOnDuplicate,
    startObject_backupJobId,
    startObject_objectName,
    startObjectResponse_httpStatus,
    startObjectResponse_uploadId,

    -- * Types

    -- ** BackupObject
    backupObject_chunksCount,
    backupObject_metadataString,
    backupObject_name,
    backupObject_objectChecksum,
    backupObject_objectChecksumAlgorithm,
    backupObject_objectToken,

    -- ** Chunk
    chunk_index,
    chunk_length,
    chunk_checksum,
    chunk_checksumAlgorithm,
    chunk_chunkToken,
  )
where

import Amazonka.BackupStorage.DeleteObject
import Amazonka.BackupStorage.GetChunk
import Amazonka.BackupStorage.GetObjectMetadata
import Amazonka.BackupStorage.ListChunks
import Amazonka.BackupStorage.ListObjects
import Amazonka.BackupStorage.NotifyObjectComplete
import Amazonka.BackupStorage.PutChunk
import Amazonka.BackupStorage.PutObject
import Amazonka.BackupStorage.StartObject
import Amazonka.BackupStorage.Types.BackupObject
import Amazonka.BackupStorage.Types.Chunk
