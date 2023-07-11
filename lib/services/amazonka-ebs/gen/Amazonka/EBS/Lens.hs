{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EBS.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EBS.Lens
  ( -- * Operations

    -- ** CompleteSnapshot
    completeSnapshot_checksum,
    completeSnapshot_checksumAggregationMethod,
    completeSnapshot_checksumAlgorithm,
    completeSnapshot_snapshotId,
    completeSnapshot_changedBlocksCount,
    completeSnapshotResponse_status,
    completeSnapshotResponse_httpStatus,

    -- ** GetSnapshotBlock
    getSnapshotBlock_snapshotId,
    getSnapshotBlock_blockIndex,
    getSnapshotBlock_blockToken,
    getSnapshotBlockResponse_checksum,
    getSnapshotBlockResponse_checksumAlgorithm,
    getSnapshotBlockResponse_dataLength,
    getSnapshotBlockResponse_httpStatus,
    getSnapshotBlockResponse_blockData,

    -- ** ListChangedBlocks
    listChangedBlocks_firstSnapshotId,
    listChangedBlocks_maxResults,
    listChangedBlocks_nextToken,
    listChangedBlocks_startingBlockIndex,
    listChangedBlocks_secondSnapshotId,
    listChangedBlocksResponse_blockSize,
    listChangedBlocksResponse_changedBlocks,
    listChangedBlocksResponse_expiryTime,
    listChangedBlocksResponse_nextToken,
    listChangedBlocksResponse_volumeSize,
    listChangedBlocksResponse_httpStatus,

    -- ** ListSnapshotBlocks
    listSnapshotBlocks_maxResults,
    listSnapshotBlocks_nextToken,
    listSnapshotBlocks_startingBlockIndex,
    listSnapshotBlocks_snapshotId,
    listSnapshotBlocksResponse_blockSize,
    listSnapshotBlocksResponse_blocks,
    listSnapshotBlocksResponse_expiryTime,
    listSnapshotBlocksResponse_nextToken,
    listSnapshotBlocksResponse_volumeSize,
    listSnapshotBlocksResponse_httpStatus,

    -- ** PutSnapshotBlock
    putSnapshotBlock_progress,
    putSnapshotBlock_snapshotId,
    putSnapshotBlock_blockIndex,
    putSnapshotBlock_dataLength,
    putSnapshotBlock_checksum,
    putSnapshotBlock_checksumAlgorithm,
    putSnapshotBlock_blockData,
    putSnapshotBlockResponse_checksum,
    putSnapshotBlockResponse_checksumAlgorithm,
    putSnapshotBlockResponse_httpStatus,

    -- ** StartSnapshot
    startSnapshot_clientToken,
    startSnapshot_description,
    startSnapshot_encrypted,
    startSnapshot_kmsKeyArn,
    startSnapshot_parentSnapshotId,
    startSnapshot_tags,
    startSnapshot_timeout,
    startSnapshot_volumeSize,
    startSnapshotResponse_blockSize,
    startSnapshotResponse_description,
    startSnapshotResponse_kmsKeyArn,
    startSnapshotResponse_ownerId,
    startSnapshotResponse_parentSnapshotId,
    startSnapshotResponse_snapshotId,
    startSnapshotResponse_startTime,
    startSnapshotResponse_status,
    startSnapshotResponse_tags,
    startSnapshotResponse_volumeSize,
    startSnapshotResponse_httpStatus,

    -- * Types

    -- ** Block
    block_blockIndex,
    block_blockToken,

    -- ** ChangedBlock
    changedBlock_blockIndex,
    changedBlock_firstBlockToken,
    changedBlock_secondBlockToken,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.EBS.CompleteSnapshot
import Amazonka.EBS.GetSnapshotBlock
import Amazonka.EBS.ListChangedBlocks
import Amazonka.EBS.ListSnapshotBlocks
import Amazonka.EBS.PutSnapshotBlock
import Amazonka.EBS.StartSnapshot
import Amazonka.EBS.Types.Block
import Amazonka.EBS.Types.ChangedBlock
import Amazonka.EBS.Types.Tag
