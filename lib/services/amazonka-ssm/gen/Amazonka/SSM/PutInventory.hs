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
-- Module      : Amazonka.SSM.PutInventory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Bulk update custom inventory items on one or more managed nodes. The
-- request adds an inventory item, if it doesn\'t already exist, or updates
-- an inventory item, if it does exist.
module Amazonka.SSM.PutInventory
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newPutInventory' smart constructor.
data PutInventory = PutInventory'
  { -- | An managed node ID where you want to add or update inventory items.
    instanceId :: Prelude.Text,
    -- | The inventory items that you want to add or update on managed nodes.
    items :: Prelude.NonEmpty InventoryItem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutInventory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'putInventory_instanceId' - An managed node ID where you want to add or update inventory items.
--
-- 'items', 'putInventory_items' - The inventory items that you want to add or update on managed nodes.
newPutInventory ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'items'
  Prelude.NonEmpty InventoryItem ->
  PutInventory
newPutInventory pInstanceId_ pItems_ =
  PutInventory'
    { instanceId = pInstanceId_,
      items = Lens.coerced Lens.# pItems_
    }

-- | An managed node ID where you want to add or update inventory items.
putInventory_instanceId :: Lens.Lens' PutInventory Prelude.Text
putInventory_instanceId = Lens.lens (\PutInventory' {instanceId} -> instanceId) (\s@PutInventory' {} a -> s {instanceId = a} :: PutInventory)

-- | The inventory items that you want to add or update on managed nodes.
putInventory_items :: Lens.Lens' PutInventory (Prelude.NonEmpty InventoryItem)
putInventory_items = Lens.lens (\PutInventory' {items} -> items) (\s@PutInventory' {} a -> s {items = a} :: PutInventory) Prelude.. Lens.coerced

instance Core.AWSRequest PutInventory where
  type AWSResponse PutInventory = PutInventoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutInventoryResponse'
            Prelude.<$> (x Data..?> "Message")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutInventory where
  hashWithSalt _salt PutInventory' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` items

instance Prelude.NFData PutInventory where
  rnf PutInventory' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf items

instance Data.ToHeaders PutInventory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.PutInventory" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutInventory where
  toJSON PutInventory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceId" Data..= instanceId),
            Prelude.Just ("Items" Data..= items)
          ]
      )

instance Data.ToPath PutInventory where
  toPath = Prelude.const "/"

instance Data.ToQuery PutInventory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutInventoryResponse' smart constructor.
data PutInventoryResponse = PutInventoryResponse'
  { -- | Information about the request.
    message :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PutInventoryResponse
newPutInventoryResponse pHttpStatus_ =
  PutInventoryResponse'
    { message = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the request.
putInventoryResponse_message :: Lens.Lens' PutInventoryResponse (Prelude.Maybe Prelude.Text)
putInventoryResponse_message = Lens.lens (\PutInventoryResponse' {message} -> message) (\s@PutInventoryResponse' {} a -> s {message = a} :: PutInventoryResponse)

-- | The response's http status code.
putInventoryResponse_httpStatus :: Lens.Lens' PutInventoryResponse Prelude.Int
putInventoryResponse_httpStatus = Lens.lens (\PutInventoryResponse' {httpStatus} -> httpStatus) (\s@PutInventoryResponse' {} a -> s {httpStatus = a} :: PutInventoryResponse)

instance Prelude.NFData PutInventoryResponse where
  rnf PutInventoryResponse' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf httpStatus
