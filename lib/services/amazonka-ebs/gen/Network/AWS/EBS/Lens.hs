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

import Amazonka.EBS.CompleteSnapshot
import Amazonka.EBS.GetSnapshotBlock
import Amazonka.EBS.ListChangedBlocks
import Amazonka.EBS.ListSnapshotBlocks
import Amazonka.EBS.PutSnapshotBlock
import Amazonka.EBS.StartSnapshot
import Amazonka.EBS.Types.Block
import Amazonka.EBS.Types.ChangedBlock
import Amazonka.EBS.Types.Tag
