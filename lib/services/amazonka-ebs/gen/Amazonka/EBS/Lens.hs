{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EBS.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EBS.Lens
  ( -- * Operations

    -- ** CompleteSnapshot
    completeSnapshot_checksumAlgorithm,
    completeSnapshot_checksumAggregationMethod,
    completeSnapshot_checksum,
    completeSnapshot_snapshotId,
    completeSnapshot_changedBlocksCount,
    completeSnapshotResponse_status,
    completeSnapshotResponse_httpStatus,

    -- ** GetSnapshotBlock
    getSnapshotBlock_snapshotId,
    getSnapshotBlock_blockIndex,
    getSnapshotBlock_blockToken,
    getSnapshotBlockResponse_checksumAlgorithm,
    getSnapshotBlockResponse_checksum,
    getSnapshotBlockResponse_dataLength,
    getSnapshotBlockResponse_httpStatus,
    getSnapshotBlockResponse_blockData,

    -- ** ListChangedBlocks
    listChangedBlocks_nextToken,
    listChangedBlocks_startingBlockIndex,
    listChangedBlocks_maxResults,
    listChangedBlocks_firstSnapshotId,
    listChangedBlocks_secondSnapshotId,
    listChangedBlocksResponse_nextToken,
    listChangedBlocksResponse_expiryTime,
    listChangedBlocksResponse_volumeSize,
    listChangedBlocksResponse_changedBlocks,
    listChangedBlocksResponse_blockSize,
    listChangedBlocksResponse_httpStatus,

    -- ** ListSnapshotBlocks
    listSnapshotBlocks_nextToken,
    listSnapshotBlocks_startingBlockIndex,
    listSnapshotBlocks_maxResults,
    listSnapshotBlocks_snapshotId,
    listSnapshotBlocksResponse_nextToken,
    listSnapshotBlocksResponse_expiryTime,
    listSnapshotBlocksResponse_volumeSize,
    listSnapshotBlocksResponse_blockSize,
    listSnapshotBlocksResponse_blocks,
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

    -- ** StartSnapshot
    startSnapshot_tags,
    startSnapshot_timeout,
    startSnapshot_clientToken,
    startSnapshot_description,
    startSnapshot_kmsKeyArn,
    startSnapshot_parentSnapshotId,
    startSnapshot_encrypted,
    startSnapshot_volumeSize,
    startSnapshotResponse_tags,
    startSnapshotResponse_ownerId,
    startSnapshotResponse_snapshotId,
    startSnapshotResponse_status,
    startSnapshotResponse_volumeSize,
    startSnapshotResponse_description,
    startSnapshotResponse_kmsKeyArn,
    startSnapshotResponse_parentSnapshotId,
    startSnapshotResponse_blockSize,
    startSnapshotResponse_startTime,
    startSnapshotResponse_httpStatus,

    -- * Types

    -- ** Block
    block_blockToken,
    block_blockIndex,

    -- ** ChangedBlock
    changedBlock_secondBlockToken,
    changedBlock_firstBlockToken,
    changedBlock_blockIndex,

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
