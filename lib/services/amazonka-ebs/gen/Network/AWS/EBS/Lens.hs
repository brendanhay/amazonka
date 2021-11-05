{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EBS.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EBS.Lens
  ( -- * Operations

    -- ** StartSnapshot
    startSnapshot_kmsKeyArn,
    startSnapshot_clientToken,
    startSnapshot_encrypted,
    startSnapshot_parentSnapshotId,
    startSnapshot_timeout,
    startSnapshot_description,
    startSnapshot_tags,
    startSnapshot_volumeSize,
    startSnapshotResponse_blockSize,
    startSnapshotResponse_status,
    startSnapshotResponse_kmsKeyArn,
    startSnapshotResponse_startTime,
    startSnapshotResponse_volumeSize,
    startSnapshotResponse_ownerId,
    startSnapshotResponse_parentSnapshotId,
    startSnapshotResponse_description,
    startSnapshotResponse_tags,
    startSnapshotResponse_snapshotId,
    startSnapshotResponse_httpStatus,

    -- ** ListSnapshotBlocks
    listSnapshotBlocks_nextToken,
    listSnapshotBlocks_startingBlockIndex,
    listSnapshotBlocks_maxResults,
    listSnapshotBlocks_snapshotId,
    listSnapshotBlocksResponse_blockSize,
    listSnapshotBlocksResponse_blocks,
    listSnapshotBlocksResponse_expiryTime,
    listSnapshotBlocksResponse_volumeSize,
    listSnapshotBlocksResponse_nextToken,
    listSnapshotBlocksResponse_httpStatus,

    -- ** PutSnapshotBlock
    putSnapshotBlock_progress,
    putSnapshotBlock_snapshotId,
    putSnapshotBlock_blockIndex,
    putSnapshotBlock_dataLength,
    putSnapshotBlock_checksum,
    putSnapshotBlock_checksumAlgorithm,
    putSnapshotBlock_blockData,
    putSnapshotBlockResponse_checksumAlgorithm,
    putSnapshotBlockResponse_checksum,
    putSnapshotBlockResponse_httpStatus,

    -- ** ListChangedBlocks
    listChangedBlocks_nextToken,
    listChangedBlocks_startingBlockIndex,
    listChangedBlocks_firstSnapshotId,
    listChangedBlocks_maxResults,
    listChangedBlocks_secondSnapshotId,
    listChangedBlocksResponse_blockSize,
    listChangedBlocksResponse_expiryTime,
    listChangedBlocksResponse_volumeSize,
    listChangedBlocksResponse_changedBlocks,
    listChangedBlocksResponse_nextToken,
    listChangedBlocksResponse_httpStatus,

    -- ** CompleteSnapshot
    completeSnapshot_checksumAlgorithm,
    completeSnapshot_checksum,
    completeSnapshot_checksumAggregationMethod,
    completeSnapshot_snapshotId,
    completeSnapshot_changedBlocksCount,
    completeSnapshotResponse_status,
    completeSnapshotResponse_httpStatus,

    -- ** GetSnapshotBlock
    getSnapshotBlock_snapshotId,
    getSnapshotBlock_blockIndex,
    getSnapshotBlock_blockToken,
    getSnapshotBlockResponse_dataLength,
    getSnapshotBlockResponse_checksumAlgorithm,
    getSnapshotBlockResponse_checksum,
    getSnapshotBlockResponse_httpStatus,
    getSnapshotBlockResponse_blockData,

    -- * Types

    -- ** Block
    block_blockIndex,
    block_blockToken,

    -- ** ChangedBlock
    changedBlock_blockIndex,
    changedBlock_secondBlockToken,
    changedBlock_firstBlockToken,

    -- ** Tag
    tag_value,
    tag_key,
  )
where

import Network.AWS.EBS.CompleteSnapshot
import Network.AWS.EBS.GetSnapshotBlock
import Network.AWS.EBS.ListChangedBlocks
import Network.AWS.EBS.ListSnapshotBlocks
import Network.AWS.EBS.PutSnapshotBlock
import Network.AWS.EBS.StartSnapshot
import Network.AWS.EBS.Types.Block
import Network.AWS.EBS.Types.ChangedBlock
import Network.AWS.EBS.Types.Tag
