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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    listSnapshotBlocksResponse_nextToken,
    listSnapshotBlocksResponse_expiryTime,
    listSnapshotBlocksResponse_volumeSize,
    listSnapshotBlocksResponse_blockSize,
    listSnapshotBlocksResponse_blocks,
    listSnapshotBlocksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EBS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSnapshotBlocks' smart constructor.
data ListSnapshotBlocks = ListSnapshotBlocks'
  { -- | The token to request the next page of results.
    --
    -- If you specify __NextToken__, then __StartingBlockIndex__ is ignored.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The block index from which the list should start. The list in the
    -- response will start from this block index or the next valid block index
    -- in the snapshot.
    --
    -- If you specify __NextToken__, then __StartingBlockIndex__ is ignored.
    startingBlockIndex :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of blocks to be returned by the request.
    --
    -- Even if additional blocks can be retrieved from the snapshot, the
    -- request can return less blocks than __MaxResults__ or an empty array of
    -- blocks.
    --
    -- To retrieve the next set of blocks from the snapshot, make another
    -- request with the returned __NextToken__ value. The value of
    -- __NextToken__ is @null@ when there are no more blocks to return.
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
-- If you specify __NextToken__, then __StartingBlockIndex__ is ignored.
--
-- 'startingBlockIndex', 'listSnapshotBlocks_startingBlockIndex' - The block index from which the list should start. The list in the
-- response will start from this block index or the next valid block index
-- in the snapshot.
--
-- If you specify __NextToken__, then __StartingBlockIndex__ is ignored.
--
-- 'maxResults', 'listSnapshotBlocks_maxResults' - The maximum number of blocks to be returned by the request.
--
-- Even if additional blocks can be retrieved from the snapshot, the
-- request can return less blocks than __MaxResults__ or an empty array of
-- blocks.
--
-- To retrieve the next set of blocks from the snapshot, make another
-- request with the returned __NextToken__ value. The value of
-- __NextToken__ is @null@ when there are no more blocks to return.
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
--
-- If you specify __NextToken__, then __StartingBlockIndex__ is ignored.
listSnapshotBlocks_nextToken :: Lens.Lens' ListSnapshotBlocks (Prelude.Maybe Prelude.Text)
listSnapshotBlocks_nextToken = Lens.lens (\ListSnapshotBlocks' {nextToken} -> nextToken) (\s@ListSnapshotBlocks' {} a -> s {nextToken = a} :: ListSnapshotBlocks)

-- | The block index from which the list should start. The list in the
-- response will start from this block index or the next valid block index
-- in the snapshot.
--
-- If you specify __NextToken__, then __StartingBlockIndex__ is ignored.
listSnapshotBlocks_startingBlockIndex :: Lens.Lens' ListSnapshotBlocks (Prelude.Maybe Prelude.Natural)
listSnapshotBlocks_startingBlockIndex = Lens.lens (\ListSnapshotBlocks' {startingBlockIndex} -> startingBlockIndex) (\s@ListSnapshotBlocks' {} a -> s {startingBlockIndex = a} :: ListSnapshotBlocks)

-- | The maximum number of blocks to be returned by the request.
--
-- Even if additional blocks can be retrieved from the snapshot, the
-- request can return less blocks than __MaxResults__ or an empty array of
-- blocks.
--
-- To retrieve the next set of blocks from the snapshot, make another
-- request with the returned __NextToken__ value. The value of
-- __NextToken__ is @null@ when there are no more blocks to return.
listSnapshotBlocks_maxResults :: Lens.Lens' ListSnapshotBlocks (Prelude.Maybe Prelude.Natural)
listSnapshotBlocks_maxResults = Lens.lens (\ListSnapshotBlocks' {maxResults} -> maxResults) (\s@ListSnapshotBlocks' {} a -> s {maxResults = a} :: ListSnapshotBlocks)

-- | The ID of the snapshot from which to get block indexes and block tokens.
listSnapshotBlocks_snapshotId :: Lens.Lens' ListSnapshotBlocks Prelude.Text
listSnapshotBlocks_snapshotId = Lens.lens (\ListSnapshotBlocks' {snapshotId} -> snapshotId) (\s@ListSnapshotBlocks' {} a -> s {snapshotId = a} :: ListSnapshotBlocks)

instance Core.AWSRequest ListSnapshotBlocks where
  type
    AWSResponse ListSnapshotBlocks =
      ListSnapshotBlocksResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSnapshotBlocksResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "ExpiryTime")
            Prelude.<*> (x Data..?> "VolumeSize")
            Prelude.<*> (x Data..?> "BlockSize")
            Prelude.<*> (x Data..?> "Blocks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSnapshotBlocks where
  hashWithSalt _salt ListSnapshotBlocks' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` startingBlockIndex
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` snapshotId

instance Prelude.NFData ListSnapshotBlocks where
  rnf ListSnapshotBlocks' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf startingBlockIndex
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf snapshotId

instance Data.ToHeaders ListSnapshotBlocks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSnapshotBlocks where
  toPath ListSnapshotBlocks' {..} =
    Prelude.mconcat
      ["/snapshots/", Data.toBS snapshotId, "/blocks"]

instance Data.ToQuery ListSnapshotBlocks where
  toQuery ListSnapshotBlocks' {..} =
    Prelude.mconcat
      [ "pageToken" Data.=: nextToken,
        "startingBlockIndex" Data.=: startingBlockIndex,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListSnapshotBlocksResponse' smart constructor.
data ListSnapshotBlocksResponse = ListSnapshotBlocksResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The time when the @BlockToken@ expires.
    expiryTime :: Prelude.Maybe Data.POSIX,
    -- | The size of the volume in GB.
    volumeSize :: Prelude.Maybe Prelude.Natural,
    -- | The size of the blocks in the snapshot, in bytes.
    blockSize :: Prelude.Maybe Prelude.Int,
    -- | An array of objects containing information about the blocks.
    blocks :: Prelude.Maybe (Data.Sensitive [Block]),
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
-- 'nextToken', 'listSnapshotBlocksResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'expiryTime', 'listSnapshotBlocksResponse_expiryTime' - The time when the @BlockToken@ expires.
--
-- 'volumeSize', 'listSnapshotBlocksResponse_volumeSize' - The size of the volume in GB.
--
-- 'blockSize', 'listSnapshotBlocksResponse_blockSize' - The size of the blocks in the snapshot, in bytes.
--
-- 'blocks', 'listSnapshotBlocksResponse_blocks' - An array of objects containing information about the blocks.
--
-- 'httpStatus', 'listSnapshotBlocksResponse_httpStatus' - The response's http status code.
newListSnapshotBlocksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSnapshotBlocksResponse
newListSnapshotBlocksResponse pHttpStatus_ =
  ListSnapshotBlocksResponse'
    { nextToken =
        Prelude.Nothing,
      expiryTime = Prelude.Nothing,
      volumeSize = Prelude.Nothing,
      blockSize = Prelude.Nothing,
      blocks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
listSnapshotBlocksResponse_nextToken :: Lens.Lens' ListSnapshotBlocksResponse (Prelude.Maybe Prelude.Text)
listSnapshotBlocksResponse_nextToken = Lens.lens (\ListSnapshotBlocksResponse' {nextToken} -> nextToken) (\s@ListSnapshotBlocksResponse' {} a -> s {nextToken = a} :: ListSnapshotBlocksResponse)

-- | The time when the @BlockToken@ expires.
listSnapshotBlocksResponse_expiryTime :: Lens.Lens' ListSnapshotBlocksResponse (Prelude.Maybe Prelude.UTCTime)
listSnapshotBlocksResponse_expiryTime = Lens.lens (\ListSnapshotBlocksResponse' {expiryTime} -> expiryTime) (\s@ListSnapshotBlocksResponse' {} a -> s {expiryTime = a} :: ListSnapshotBlocksResponse) Prelude.. Lens.mapping Data._Time

-- | The size of the volume in GB.
listSnapshotBlocksResponse_volumeSize :: Lens.Lens' ListSnapshotBlocksResponse (Prelude.Maybe Prelude.Natural)
listSnapshotBlocksResponse_volumeSize = Lens.lens (\ListSnapshotBlocksResponse' {volumeSize} -> volumeSize) (\s@ListSnapshotBlocksResponse' {} a -> s {volumeSize = a} :: ListSnapshotBlocksResponse)

-- | The size of the blocks in the snapshot, in bytes.
listSnapshotBlocksResponse_blockSize :: Lens.Lens' ListSnapshotBlocksResponse (Prelude.Maybe Prelude.Int)
listSnapshotBlocksResponse_blockSize = Lens.lens (\ListSnapshotBlocksResponse' {blockSize} -> blockSize) (\s@ListSnapshotBlocksResponse' {} a -> s {blockSize = a} :: ListSnapshotBlocksResponse)

-- | An array of objects containing information about the blocks.
listSnapshotBlocksResponse_blocks :: Lens.Lens' ListSnapshotBlocksResponse (Prelude.Maybe [Block])
listSnapshotBlocksResponse_blocks = Lens.lens (\ListSnapshotBlocksResponse' {blocks} -> blocks) (\s@ListSnapshotBlocksResponse' {} a -> s {blocks = a} :: ListSnapshotBlocksResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
listSnapshotBlocksResponse_httpStatus :: Lens.Lens' ListSnapshotBlocksResponse Prelude.Int
listSnapshotBlocksResponse_httpStatus = Lens.lens (\ListSnapshotBlocksResponse' {httpStatus} -> httpStatus) (\s@ListSnapshotBlocksResponse' {} a -> s {httpStatus = a} :: ListSnapshotBlocksResponse)

instance Prelude.NFData ListSnapshotBlocksResponse where
  rnf ListSnapshotBlocksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf expiryTime
      `Prelude.seq` Prelude.rnf volumeSize
      `Prelude.seq` Prelude.rnf blockSize
      `Prelude.seq` Prelude.rnf blocks
      `Prelude.seq` Prelude.rnf httpStatus
