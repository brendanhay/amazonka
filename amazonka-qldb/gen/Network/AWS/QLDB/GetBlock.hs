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
-- Module      : Network.AWS.QLDB.GetBlock
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a block object at a specified address in a journal. Also returns
-- a proof of the specified block for verification if @DigestTipAddress@ is
-- provided.
--
-- For information about the data contents in a block, see
-- <https://docs.aws.amazon.com/qldb/latest/developerguide/journal-contents.html Journal contents>
-- in the /Amazon QLDB Developer Guide/.
--
-- If the specified ledger doesn\'t exist or is in @DELETING@ status, then
-- throws @ResourceNotFoundException@.
--
-- If the specified ledger is in @CREATING@ status, then throws
-- @ResourcePreconditionNotMetException@.
--
-- If no block exists with the specified address, then throws
-- @InvalidParameterException@.
module Network.AWS.QLDB.GetBlock
  ( -- * Creating a Request
    GetBlock (..),
    newGetBlock,

    -- * Request Lenses
    getBlock_digestTipAddress,
    getBlock_name,
    getBlock_blockAddress,

    -- * Destructuring the Response
    GetBlockResponse (..),
    newGetBlockResponse,

    -- * Response Lenses
    getBlockResponse_proof,
    getBlockResponse_httpStatus,
    getBlockResponse_block,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QLDB.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetBlock' smart constructor.
data GetBlock = GetBlock'
  { -- | The latest block location covered by the digest for which to request a
    -- proof. An address is an Amazon Ion structure that has two fields:
    -- @strandId@ and @sequenceNo@.
    --
    -- For example: @{strandId:\"BlFTjlSXze9BIh1KOszcE3\",sequenceNo:49}@
    digestTipAddress :: Prelude.Maybe (Core.Sensitive ValueHolder),
    -- | The name of the ledger.
    name :: Prelude.Text,
    -- | The location of the block that you want to request. An address is an
    -- Amazon Ion structure that has two fields: @strandId@ and @sequenceNo@.
    --
    -- For example: @{strandId:\"BlFTjlSXze9BIh1KOszcE3\",sequenceNo:14}@
    blockAddress :: Core.Sensitive ValueHolder
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'digestTipAddress', 'getBlock_digestTipAddress' - The latest block location covered by the digest for which to request a
-- proof. An address is an Amazon Ion structure that has two fields:
-- @strandId@ and @sequenceNo@.
--
-- For example: @{strandId:\"BlFTjlSXze9BIh1KOszcE3\",sequenceNo:49}@
--
-- 'name', 'getBlock_name' - The name of the ledger.
--
-- 'blockAddress', 'getBlock_blockAddress' - The location of the block that you want to request. An address is an
-- Amazon Ion structure that has two fields: @strandId@ and @sequenceNo@.
--
-- For example: @{strandId:\"BlFTjlSXze9BIh1KOszcE3\",sequenceNo:14}@
newGetBlock ::
  -- | 'name'
  Prelude.Text ->
  -- | 'blockAddress'
  ValueHolder ->
  GetBlock
newGetBlock pName_ pBlockAddress_ =
  GetBlock'
    { digestTipAddress = Prelude.Nothing,
      name = pName_,
      blockAddress = Core._Sensitive Lens.# pBlockAddress_
    }

-- | The latest block location covered by the digest for which to request a
-- proof. An address is an Amazon Ion structure that has two fields:
-- @strandId@ and @sequenceNo@.
--
-- For example: @{strandId:\"BlFTjlSXze9BIh1KOszcE3\",sequenceNo:49}@
getBlock_digestTipAddress :: Lens.Lens' GetBlock (Prelude.Maybe ValueHolder)
getBlock_digestTipAddress = Lens.lens (\GetBlock' {digestTipAddress} -> digestTipAddress) (\s@GetBlock' {} a -> s {digestTipAddress = a} :: GetBlock) Prelude.. Lens.mapping Core._Sensitive

-- | The name of the ledger.
getBlock_name :: Lens.Lens' GetBlock Prelude.Text
getBlock_name = Lens.lens (\GetBlock' {name} -> name) (\s@GetBlock' {} a -> s {name = a} :: GetBlock)

-- | The location of the block that you want to request. An address is an
-- Amazon Ion structure that has two fields: @strandId@ and @sequenceNo@.
--
-- For example: @{strandId:\"BlFTjlSXze9BIh1KOszcE3\",sequenceNo:14}@
getBlock_blockAddress :: Lens.Lens' GetBlock ValueHolder
getBlock_blockAddress = Lens.lens (\GetBlock' {blockAddress} -> blockAddress) (\s@GetBlock' {} a -> s {blockAddress = a} :: GetBlock) Prelude.. Core._Sensitive

instance Core.AWSRequest GetBlock where
  type AWSResponse GetBlock = GetBlockResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBlockResponse'
            Prelude.<$> (x Core..?> "Proof")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Block")
      )

instance Prelude.Hashable GetBlock

instance Prelude.NFData GetBlock

instance Core.ToHeaders GetBlock where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetBlock where
  toJSON GetBlock' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DigestTipAddress" Core..=)
              Prelude.<$> digestTipAddress,
            Prelude.Just ("BlockAddress" Core..= blockAddress)
          ]
      )

instance Core.ToPath GetBlock where
  toPath GetBlock' {..} =
    Prelude.mconcat
      ["/ledgers/", Core.toBS name, "/block"]

instance Core.ToQuery GetBlock where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBlockResponse' smart constructor.
data GetBlockResponse = GetBlockResponse'
  { -- | The proof object in Amazon Ion format returned by a @GetBlock@ request.
    -- A proof contains the list of hash values required to recalculate the
    -- specified digest using a Merkle tree, starting with the specified block.
    proof :: Prelude.Maybe (Core.Sensitive ValueHolder),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The block data object in Amazon Ion format.
    block :: Core.Sensitive ValueHolder
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBlockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'proof', 'getBlockResponse_proof' - The proof object in Amazon Ion format returned by a @GetBlock@ request.
-- A proof contains the list of hash values required to recalculate the
-- specified digest using a Merkle tree, starting with the specified block.
--
-- 'httpStatus', 'getBlockResponse_httpStatus' - The response's http status code.
--
-- 'block', 'getBlockResponse_block' - The block data object in Amazon Ion format.
newGetBlockResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'block'
  ValueHolder ->
  GetBlockResponse
newGetBlockResponse pHttpStatus_ pBlock_ =
  GetBlockResponse'
    { proof = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      block = Core._Sensitive Lens.# pBlock_
    }

-- | The proof object in Amazon Ion format returned by a @GetBlock@ request.
-- A proof contains the list of hash values required to recalculate the
-- specified digest using a Merkle tree, starting with the specified block.
getBlockResponse_proof :: Lens.Lens' GetBlockResponse (Prelude.Maybe ValueHolder)
getBlockResponse_proof = Lens.lens (\GetBlockResponse' {proof} -> proof) (\s@GetBlockResponse' {} a -> s {proof = a} :: GetBlockResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The response's http status code.
getBlockResponse_httpStatus :: Lens.Lens' GetBlockResponse Prelude.Int
getBlockResponse_httpStatus = Lens.lens (\GetBlockResponse' {httpStatus} -> httpStatus) (\s@GetBlockResponse' {} a -> s {httpStatus = a} :: GetBlockResponse)

-- | The block data object in Amazon Ion format.
getBlockResponse_block :: Lens.Lens' GetBlockResponse ValueHolder
getBlockResponse_block = Lens.lens (\GetBlockResponse' {block} -> block) (\s@GetBlockResponse' {} a -> s {block = a} :: GetBlockResponse) Prelude.. Core._Sensitive

instance Prelude.NFData GetBlockResponse
