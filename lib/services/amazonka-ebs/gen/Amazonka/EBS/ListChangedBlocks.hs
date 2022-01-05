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
-- Module      : Amazonka.EBS.ListChangedBlocks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the blocks that are different between two
-- Amazon Elastic Block Store snapshots of the same volume\/snapshot
-- lineage.
module Amazonka.EBS.ListChangedBlocks
  ( -- * Creating a Request
    ListChangedBlocks (..),
    newListChangedBlocks,

    -- * Request Lenses
    listChangedBlocks_nextToken,
    listChangedBlocks_startingBlockIndex,
    listChangedBlocks_firstSnapshotId,
    listChangedBlocks_maxResults,
    listChangedBlocks_secondSnapshotId,

    -- * Destructuring the Response
    ListChangedBlocksResponse (..),
    newListChangedBlocksResponse,

    -- * Response Lenses
    listChangedBlocksResponse_blockSize,
    listChangedBlocksResponse_expiryTime,
    listChangedBlocksResponse_volumeSize,
    listChangedBlocksResponse_changedBlocks,
    listChangedBlocksResponse_nextToken,
    listChangedBlocksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EBS.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListChangedBlocks' smart constructor.
data ListChangedBlocks = ListChangedBlocks'
  { -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The block index from which the comparison should start.
    --
    -- The list in the response will start from this block index or the next
    -- valid block index in the snapshots.
    startingBlockIndex :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the first snapshot to use for the comparison.
    --
    -- The @FirstSnapshotID@ parameter must be specified with a
    -- @SecondSnapshotId@ parameter; otherwise, an error occurs.
    firstSnapshotId :: Prelude.Maybe Prelude.Text,
    -- | The number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the second snapshot to use for the comparison.
    --
    -- The @SecondSnapshotId@ parameter must be specified with a
    -- @FirstSnapshotID@ parameter; otherwise, an error occurs.
    secondSnapshotId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChangedBlocks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listChangedBlocks_nextToken' - The token to request the next page of results.
--
-- 'startingBlockIndex', 'listChangedBlocks_startingBlockIndex' - The block index from which the comparison should start.
--
-- The list in the response will start from this block index or the next
-- valid block index in the snapshots.
--
-- 'firstSnapshotId', 'listChangedBlocks_firstSnapshotId' - The ID of the first snapshot to use for the comparison.
--
-- The @FirstSnapshotID@ parameter must be specified with a
-- @SecondSnapshotId@ parameter; otherwise, an error occurs.
--
-- 'maxResults', 'listChangedBlocks_maxResults' - The number of results to return.
--
-- 'secondSnapshotId', 'listChangedBlocks_secondSnapshotId' - The ID of the second snapshot to use for the comparison.
--
-- The @SecondSnapshotId@ parameter must be specified with a
-- @FirstSnapshotID@ parameter; otherwise, an error occurs.
newListChangedBlocks ::
  -- | 'secondSnapshotId'
  Prelude.Text ->
  ListChangedBlocks
newListChangedBlocks pSecondSnapshotId_ =
  ListChangedBlocks'
    { nextToken = Prelude.Nothing,
      startingBlockIndex = Prelude.Nothing,
      firstSnapshotId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      secondSnapshotId = pSecondSnapshotId_
    }

-- | The token to request the next page of results.
listChangedBlocks_nextToken :: Lens.Lens' ListChangedBlocks (Prelude.Maybe Prelude.Text)
listChangedBlocks_nextToken = Lens.lens (\ListChangedBlocks' {nextToken} -> nextToken) (\s@ListChangedBlocks' {} a -> s {nextToken = a} :: ListChangedBlocks)

-- | The block index from which the comparison should start.
--
-- The list in the response will start from this block index or the next
-- valid block index in the snapshots.
listChangedBlocks_startingBlockIndex :: Lens.Lens' ListChangedBlocks (Prelude.Maybe Prelude.Natural)
listChangedBlocks_startingBlockIndex = Lens.lens (\ListChangedBlocks' {startingBlockIndex} -> startingBlockIndex) (\s@ListChangedBlocks' {} a -> s {startingBlockIndex = a} :: ListChangedBlocks)

-- | The ID of the first snapshot to use for the comparison.
--
-- The @FirstSnapshotID@ parameter must be specified with a
-- @SecondSnapshotId@ parameter; otherwise, an error occurs.
listChangedBlocks_firstSnapshotId :: Lens.Lens' ListChangedBlocks (Prelude.Maybe Prelude.Text)
listChangedBlocks_firstSnapshotId = Lens.lens (\ListChangedBlocks' {firstSnapshotId} -> firstSnapshotId) (\s@ListChangedBlocks' {} a -> s {firstSnapshotId = a} :: ListChangedBlocks)

-- | The number of results to return.
listChangedBlocks_maxResults :: Lens.Lens' ListChangedBlocks (Prelude.Maybe Prelude.Natural)
listChangedBlocks_maxResults = Lens.lens (\ListChangedBlocks' {maxResults} -> maxResults) (\s@ListChangedBlocks' {} a -> s {maxResults = a} :: ListChangedBlocks)

-- | The ID of the second snapshot to use for the comparison.
--
-- The @SecondSnapshotId@ parameter must be specified with a
-- @FirstSnapshotID@ parameter; otherwise, an error occurs.
listChangedBlocks_secondSnapshotId :: Lens.Lens' ListChangedBlocks Prelude.Text
listChangedBlocks_secondSnapshotId = Lens.lens (\ListChangedBlocks' {secondSnapshotId} -> secondSnapshotId) (\s@ListChangedBlocks' {} a -> s {secondSnapshotId = a} :: ListChangedBlocks)

instance Core.AWSRequest ListChangedBlocks where
  type
    AWSResponse ListChangedBlocks =
      ListChangedBlocksResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListChangedBlocksResponse'
            Prelude.<$> (x Core..?> "BlockSize")
            Prelude.<*> (x Core..?> "ExpiryTime")
            Prelude.<*> (x Core..?> "VolumeSize")
            Prelude.<*> (x Core..?> "ChangedBlocks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListChangedBlocks where
  hashWithSalt _salt ListChangedBlocks' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` startingBlockIndex
      `Prelude.hashWithSalt` firstSnapshotId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` secondSnapshotId

instance Prelude.NFData ListChangedBlocks where
  rnf ListChangedBlocks' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf startingBlockIndex
      `Prelude.seq` Prelude.rnf firstSnapshotId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf secondSnapshotId

instance Core.ToHeaders ListChangedBlocks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListChangedBlocks where
  toPath ListChangedBlocks' {..} =
    Prelude.mconcat
      [ "/snapshots/",
        Core.toBS secondSnapshotId,
        "/changedblocks"
      ]

instance Core.ToQuery ListChangedBlocks where
  toQuery ListChangedBlocks' {..} =
    Prelude.mconcat
      [ "pageToken" Core.=: nextToken,
        "startingBlockIndex" Core.=: startingBlockIndex,
        "firstSnapshotId" Core.=: firstSnapshotId,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListChangedBlocksResponse' smart constructor.
data ListChangedBlocksResponse = ListChangedBlocksResponse'
  { -- | The size of the blocks in the snapshot, in bytes.
    blockSize :: Prelude.Maybe Prelude.Int,
    -- | The time when the @BlockToken@ expires.
    expiryTime :: Prelude.Maybe Core.POSIX,
    -- | The size of the volume in GB.
    volumeSize :: Prelude.Maybe Prelude.Natural,
    -- | An array of objects containing information about the changed blocks.
    changedBlocks :: Prelude.Maybe [Core.Sensitive ChangedBlock],
    -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChangedBlocksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockSize', 'listChangedBlocksResponse_blockSize' - The size of the blocks in the snapshot, in bytes.
--
-- 'expiryTime', 'listChangedBlocksResponse_expiryTime' - The time when the @BlockToken@ expires.
--
-- 'volumeSize', 'listChangedBlocksResponse_volumeSize' - The size of the volume in GB.
--
-- 'changedBlocks', 'listChangedBlocksResponse_changedBlocks' - An array of objects containing information about the changed blocks.
--
-- 'nextToken', 'listChangedBlocksResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'httpStatus', 'listChangedBlocksResponse_httpStatus' - The response's http status code.
newListChangedBlocksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListChangedBlocksResponse
newListChangedBlocksResponse pHttpStatus_ =
  ListChangedBlocksResponse'
    { blockSize =
        Prelude.Nothing,
      expiryTime = Prelude.Nothing,
      volumeSize = Prelude.Nothing,
      changedBlocks = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The size of the blocks in the snapshot, in bytes.
listChangedBlocksResponse_blockSize :: Lens.Lens' ListChangedBlocksResponse (Prelude.Maybe Prelude.Int)
listChangedBlocksResponse_blockSize = Lens.lens (\ListChangedBlocksResponse' {blockSize} -> blockSize) (\s@ListChangedBlocksResponse' {} a -> s {blockSize = a} :: ListChangedBlocksResponse)

-- | The time when the @BlockToken@ expires.
listChangedBlocksResponse_expiryTime :: Lens.Lens' ListChangedBlocksResponse (Prelude.Maybe Prelude.UTCTime)
listChangedBlocksResponse_expiryTime = Lens.lens (\ListChangedBlocksResponse' {expiryTime} -> expiryTime) (\s@ListChangedBlocksResponse' {} a -> s {expiryTime = a} :: ListChangedBlocksResponse) Prelude.. Lens.mapping Core._Time

-- | The size of the volume in GB.
listChangedBlocksResponse_volumeSize :: Lens.Lens' ListChangedBlocksResponse (Prelude.Maybe Prelude.Natural)
listChangedBlocksResponse_volumeSize = Lens.lens (\ListChangedBlocksResponse' {volumeSize} -> volumeSize) (\s@ListChangedBlocksResponse' {} a -> s {volumeSize = a} :: ListChangedBlocksResponse)

-- | An array of objects containing information about the changed blocks.
listChangedBlocksResponse_changedBlocks :: Lens.Lens' ListChangedBlocksResponse (Prelude.Maybe [ChangedBlock])
listChangedBlocksResponse_changedBlocks = Lens.lens (\ListChangedBlocksResponse' {changedBlocks} -> changedBlocks) (\s@ListChangedBlocksResponse' {} a -> s {changedBlocks = a} :: ListChangedBlocksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
listChangedBlocksResponse_nextToken :: Lens.Lens' ListChangedBlocksResponse (Prelude.Maybe Prelude.Text)
listChangedBlocksResponse_nextToken = Lens.lens (\ListChangedBlocksResponse' {nextToken} -> nextToken) (\s@ListChangedBlocksResponse' {} a -> s {nextToken = a} :: ListChangedBlocksResponse)

-- | The response's http status code.
listChangedBlocksResponse_httpStatus :: Lens.Lens' ListChangedBlocksResponse Prelude.Int
listChangedBlocksResponse_httpStatus = Lens.lens (\ListChangedBlocksResponse' {httpStatus} -> httpStatus) (\s@ListChangedBlocksResponse' {} a -> s {httpStatus = a} :: ListChangedBlocksResponse)

instance Prelude.NFData ListChangedBlocksResponse where
  rnf ListChangedBlocksResponse' {..} =
    Prelude.rnf blockSize
      `Prelude.seq` Prelude.rnf expiryTime
      `Prelude.seq` Prelude.rnf volumeSize
      `Prelude.seq` Prelude.rnf changedBlocks
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
