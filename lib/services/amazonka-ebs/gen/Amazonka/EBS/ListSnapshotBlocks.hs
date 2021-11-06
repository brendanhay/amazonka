{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EBS.ListSnapshotBlocks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the blocks in an Amazon Elastic Block Store
-- snapshot.
module Amazonka.EBS.ListSnapshotBlocks
  ( -- * Creating a Request
    ListSnapshotBlocks (..),
    newListSnapshotBlocks,

    -- * Request Lenses
    listSnapshotBlocks_nextToken,
    listSnapshotBlocks_startingBlockIndex,
    listSnapshotBlocks_maxResults,
    listSnapshotBlocks_snapshotId,

    -- * Destructuring the Response
    ListSnapshotBlocksResponse (..),
    newListSnapshotBlocksResponse,

    -- * Response Lenses
    listSnapshotBlocksResponse_blockSize,
    listSnapshotBlocksResponse_blocks,
    listSnapshotBlocksResponse_expiryTime,
    listSnapshotBlocksResponse_volumeSize,
    listSnapshotBlocksResponse_nextToken,
    listSnapshotBlocksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EBS.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSnapshotBlocks' smart constructor.
data ListSnapshotBlocks = ListSnapshotBlocks'
  { -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The block index from which the list should start. The list in the
    -- response will start from this block index or the next valid block index
    -- in the snapshot.
    startingBlockIndex :: Prelude.Maybe Prelude.Natural,
    -- | The number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the snapshot from which to get block indexes and block tokens.
    snapshotId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSnapshotBlocks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSnapshotBlocks_nextToken' - The token to request the next page of results.
--
-- 'startingBlockIndex', 'listSnapshotBlocks_startingBlockIndex' - The block index from which the list should start. The list in the
-- response will start from this block index or the next valid block index
-- in the snapshot.
--
-- 'maxResults', 'listSnapshotBlocks_maxResults' - The number of results to return.
--
-- 'snapshotId', 'listSnapshotBlocks_snapshotId' - The ID of the snapshot from which to get block indexes and block tokens.
newListSnapshotBlocks ::
  -- | 'snapshotId'
  Prelude.Text ->
  ListSnapshotBlocks
newListSnapshotBlocks pSnapshotId_ =
  ListSnapshotBlocks'
    { nextToken = Prelude.Nothing,
      startingBlockIndex = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      snapshotId = pSnapshotId_
    }

-- | The token to request the next page of results.
listSnapshotBlocks_nextToken :: Lens.Lens' ListSnapshotBlocks (Prelude.Maybe Prelude.Text)
listSnapshotBlocks_nextToken = Lens.lens (\ListSnapshotBlocks' {nextToken} -> nextToken) (\s@ListSnapshotBlocks' {} a -> s {nextToken = a} :: ListSnapshotBlocks)

-- | The block index from which the list should start. The list in the
-- response will start from this block index or the next valid block index
-- in the snapshot.
listSnapshotBlocks_startingBlockIndex :: Lens.Lens' ListSnapshotBlocks (Prelude.Maybe Prelude.Natural)
listSnapshotBlocks_startingBlockIndex = Lens.lens (\ListSnapshotBlocks' {startingBlockIndex} -> startingBlockIndex) (\s@ListSnapshotBlocks' {} a -> s {startingBlockIndex = a} :: ListSnapshotBlocks)

-- | The number of results to return.
listSnapshotBlocks_maxResults :: Lens.Lens' ListSnapshotBlocks (Prelude.Maybe Prelude.Natural)
listSnapshotBlocks_maxResults = Lens.lens (\ListSnapshotBlocks' {maxResults} -> maxResults) (\s@ListSnapshotBlocks' {} a -> s {maxResults = a} :: ListSnapshotBlocks)

-- | The ID of the snapshot from which to get block indexes and block tokens.
listSnapshotBlocks_snapshotId :: Lens.Lens' ListSnapshotBlocks Prelude.Text
listSnapshotBlocks_snapshotId = Lens.lens (\ListSnapshotBlocks' {snapshotId} -> snapshotId) (\s@ListSnapshotBlocks' {} a -> s {snapshotId = a} :: ListSnapshotBlocks)

instance Core.AWSRequest ListSnapshotBlocks where
  type
    AWSResponse ListSnapshotBlocks =
      ListSnapshotBlocksResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSnapshotBlocksResponse'
            Prelude.<$> (x Core..?> "BlockSize")
            Prelude.<*> (x Core..?> "Blocks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "ExpiryTime")
            Prelude.<*> (x Core..?> "VolumeSize")
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSnapshotBlocks

instance Prelude.NFData ListSnapshotBlocks

instance Core.ToHeaders ListSnapshotBlocks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListSnapshotBlocks where
  toPath ListSnapshotBlocks' {..} =
    Prelude.mconcat
      ["/snapshots/", Core.toBS snapshotId, "/blocks"]

instance Core.ToQuery ListSnapshotBlocks where
  toQuery ListSnapshotBlocks' {..} =
    Prelude.mconcat
      [ "pageToken" Core.=: nextToken,
        "startingBlockIndex" Core.=: startingBlockIndex,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListSnapshotBlocksResponse' smart constructor.
data ListSnapshotBlocksResponse = ListSnapshotBlocksResponse'
  { -- | The size of the blocks in the snapshot, in bytes.
    blockSize :: Prelude.Maybe Prelude.Int,
    -- | An array of objects containing information about the blocks.
    blocks :: Prelude.Maybe (Core.Sensitive [Block]),
    -- | The time when the @BlockToken@ expires.
    expiryTime :: Prelude.Maybe Core.POSIX,
    -- | The size of the volume in GB.
    volumeSize :: Prelude.Maybe Prelude.Natural,
    -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSnapshotBlocksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockSize', 'listSnapshotBlocksResponse_blockSize' - The size of the blocks in the snapshot, in bytes.
--
-- 'blocks', 'listSnapshotBlocksResponse_blocks' - An array of objects containing information about the blocks.
--
-- 'expiryTime', 'listSnapshotBlocksResponse_expiryTime' - The time when the @BlockToken@ expires.
--
-- 'volumeSize', 'listSnapshotBlocksResponse_volumeSize' - The size of the volume in GB.
--
-- 'nextToken', 'listSnapshotBlocksResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'httpStatus', 'listSnapshotBlocksResponse_httpStatus' - The response's http status code.
newListSnapshotBlocksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSnapshotBlocksResponse
newListSnapshotBlocksResponse pHttpStatus_ =
  ListSnapshotBlocksResponse'
    { blockSize =
        Prelude.Nothing,
      blocks = Prelude.Nothing,
      expiryTime = Prelude.Nothing,
      volumeSize = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The size of the blocks in the snapshot, in bytes.
listSnapshotBlocksResponse_blockSize :: Lens.Lens' ListSnapshotBlocksResponse (Prelude.Maybe Prelude.Int)
listSnapshotBlocksResponse_blockSize = Lens.lens (\ListSnapshotBlocksResponse' {blockSize} -> blockSize) (\s@ListSnapshotBlocksResponse' {} a -> s {blockSize = a} :: ListSnapshotBlocksResponse)

-- | An array of objects containing information about the blocks.
listSnapshotBlocksResponse_blocks :: Lens.Lens' ListSnapshotBlocksResponse (Prelude.Maybe [Block])
listSnapshotBlocksResponse_blocks = Lens.lens (\ListSnapshotBlocksResponse' {blocks} -> blocks) (\s@ListSnapshotBlocksResponse' {} a -> s {blocks = a} :: ListSnapshotBlocksResponse) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The time when the @BlockToken@ expires.
listSnapshotBlocksResponse_expiryTime :: Lens.Lens' ListSnapshotBlocksResponse (Prelude.Maybe Prelude.UTCTime)
listSnapshotBlocksResponse_expiryTime = Lens.lens (\ListSnapshotBlocksResponse' {expiryTime} -> expiryTime) (\s@ListSnapshotBlocksResponse' {} a -> s {expiryTime = a} :: ListSnapshotBlocksResponse) Prelude.. Lens.mapping Core._Time

-- | The size of the volume in GB.
listSnapshotBlocksResponse_volumeSize :: Lens.Lens' ListSnapshotBlocksResponse (Prelude.Maybe Prelude.Natural)
listSnapshotBlocksResponse_volumeSize = Lens.lens (\ListSnapshotBlocksResponse' {volumeSize} -> volumeSize) (\s@ListSnapshotBlocksResponse' {} a -> s {volumeSize = a} :: ListSnapshotBlocksResponse)

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
listSnapshotBlocksResponse_nextToken :: Lens.Lens' ListSnapshotBlocksResponse (Prelude.Maybe Prelude.Text)
listSnapshotBlocksResponse_nextToken = Lens.lens (\ListSnapshotBlocksResponse' {nextToken} -> nextToken) (\s@ListSnapshotBlocksResponse' {} a -> s {nextToken = a} :: ListSnapshotBlocksResponse)

-- | The response's http status code.
listSnapshotBlocksResponse_httpStatus :: Lens.Lens' ListSnapshotBlocksResponse Prelude.Int
listSnapshotBlocksResponse_httpStatus = Lens.lens (\ListSnapshotBlocksResponse' {httpStatus} -> httpStatus) (\s@ListSnapshotBlocksResponse' {} a -> s {httpStatus = a} :: ListSnapshotBlocksResponse)

instance Prelude.NFData ListSnapshotBlocksResponse
