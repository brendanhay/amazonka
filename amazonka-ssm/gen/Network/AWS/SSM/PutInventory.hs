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
-- Module      : Network.AWS.SSM.PutInventory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Bulk update custom inventory items on one more instance. The request
-- adds an inventory item, if it doesn\'t already exist, or updates an
-- inventory item, if it does exist.
module Network.AWS.SSM.PutInventory
  ( -- * Creating a Request
    PutInventory (..),
    newPutInventory,

    -- * Request Lenses
    putInventory_instanceId,
    putInventory_items,

    -- * Destructuring the Response
    PutInventoryResponse (..),
    newPutInventoryResponse,

    -- * Response Lenses
    putInventoryResponse_message,
    putInventoryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newPutInventory' smart constructor.
data PutInventory = PutInventory'
  { -- | An instance ID where you want to add or update inventory items.
    instanceId :: Core.Text,
    -- | The inventory items that you want to add or update on instances.
    items :: Core.NonEmpty InventoryItem
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutInventory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'putInventory_instanceId' - An instance ID where you want to add or update inventory items.
--
-- 'items', 'putInventory_items' - The inventory items that you want to add or update on instances.
newPutInventory ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'items'
  Core.NonEmpty InventoryItem ->
  PutInventory
newPutInventory pInstanceId_ pItems_ =
  PutInventory'
    { instanceId = pInstanceId_,
      items = Lens._Coerce Lens.# pItems_
    }

-- | An instance ID where you want to add or update inventory items.
putInventory_instanceId :: Lens.Lens' PutInventory Core.Text
putInventory_instanceId = Lens.lens (\PutInventory' {instanceId} -> instanceId) (\s@PutInventory' {} a -> s {instanceId = a} :: PutInventory)

-- | The inventory items that you want to add or update on instances.
putInventory_items :: Lens.Lens' PutInventory (Core.NonEmpty InventoryItem)
putInventory_items = Lens.lens (\PutInventory' {items} -> items) (\s@PutInventory' {} a -> s {items = a} :: PutInventory) Core.. Lens._Coerce

instance Core.AWSRequest PutInventory where
  type AWSResponse PutInventory = PutInventoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutInventoryResponse'
            Core.<$> (x Core..?> "Message")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutInventory

instance Core.NFData PutInventory

instance Core.ToHeaders PutInventory where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.PutInventory" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutInventory where
  toJSON PutInventory' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InstanceId" Core..= instanceId),
            Core.Just ("Items" Core..= items)
          ]
      )

instance Core.ToPath PutInventory where
  toPath = Core.const "/"

instance Core.ToQuery PutInventory where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutInventoryResponse' smart constructor.
data PutInventoryResponse = PutInventoryResponse'
  { -- | Information about the request.
    message :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutInventoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'putInventoryResponse_message' - Information about the request.
--
-- 'httpStatus', 'putInventoryResponse_httpStatus' - The response's http status code.
newPutInventoryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutInventoryResponse
newPutInventoryResponse pHttpStatus_ =
  PutInventoryResponse'
    { message = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the request.
putInventoryResponse_message :: Lens.Lens' PutInventoryResponse (Core.Maybe Core.Text)
putInventoryResponse_message = Lens.lens (\PutInventoryResponse' {message} -> message) (\s@PutInventoryResponse' {} a -> s {message = a} :: PutInventoryResponse)

-- | The response's http status code.
putInventoryResponse_httpStatus :: Lens.Lens' PutInventoryResponse Core.Int
putInventoryResponse_httpStatus = Lens.lens (\PutInventoryResponse' {httpStatus} -> httpStatus) (\s@PutInventoryResponse' {} a -> s {httpStatus = a} :: PutInventoryResponse)

instance Core.NFData PutInventoryResponse
