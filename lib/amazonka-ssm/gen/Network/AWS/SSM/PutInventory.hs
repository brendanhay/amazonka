{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.PutInventory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Bulk update custom inventory items on one more instance. The request adds an inventory item, if it doesn't already exist, or updates an inventory item, if it does exist.
module Network.AWS.SSM.PutInventory
  ( -- * Creating a request
    PutInventory (..),
    mkPutInventory,

    -- ** Request lenses
    piInstanceId,
    piItems,

    -- * Destructuring the response
    PutInventoryResponse (..),
    mkPutInventoryResponse,

    -- ** Response lenses
    pirsMessage,
    pirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkPutInventory' smart constructor.
data PutInventory = PutInventory'
  { -- | An instance ID where you want to add or update inventory items.
    instanceId :: Lude.Text,
    -- | The inventory items that you want to add or update on instances.
    items :: Lude.NonEmpty InventoryItem
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutInventory' with the minimum fields required to make a request.
--
-- * 'instanceId' - An instance ID where you want to add or update inventory items.
-- * 'items' - The inventory items that you want to add or update on instances.
mkPutInventory ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'items'
  Lude.NonEmpty InventoryItem ->
  PutInventory
mkPutInventory pInstanceId_ pItems_ =
  PutInventory' {instanceId = pInstanceId_, items = pItems_}

-- | An instance ID where you want to add or update inventory items.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piInstanceId :: Lens.Lens' PutInventory Lude.Text
piInstanceId = Lens.lens (instanceId :: PutInventory -> Lude.Text) (\s a -> s {instanceId = a} :: PutInventory)
{-# DEPRECATED piInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The inventory items that you want to add or update on instances.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piItems :: Lens.Lens' PutInventory (Lude.NonEmpty InventoryItem)
piItems = Lens.lens (items :: PutInventory -> Lude.NonEmpty InventoryItem) (\s a -> s {items = a} :: PutInventory)
{-# DEPRECATED piItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Lude.AWSRequest PutInventory where
  type Rs PutInventory = PutInventoryResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutInventoryResponse'
            Lude.<$> (x Lude..?> "Message") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutInventory where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.PutInventory" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutInventory where
  toJSON PutInventory' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InstanceId" Lude..= instanceId),
            Lude.Just ("Items" Lude..= items)
          ]
      )

instance Lude.ToPath PutInventory where
  toPath = Lude.const "/"

instance Lude.ToQuery PutInventory where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutInventoryResponse' smart constructor.
data PutInventoryResponse = PutInventoryResponse'
  { -- | Information about the request.
    message :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutInventoryResponse' with the minimum fields required to make a request.
--
-- * 'message' - Information about the request.
-- * 'responseStatus' - The response status code.
mkPutInventoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutInventoryResponse
mkPutInventoryResponse pResponseStatus_ =
  PutInventoryResponse'
    { message = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the request.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsMessage :: Lens.Lens' PutInventoryResponse (Lude.Maybe Lude.Text)
pirsMessage = Lens.lens (message :: PutInventoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: PutInventoryResponse)
{-# DEPRECATED pirsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsResponseStatus :: Lens.Lens' PutInventoryResponse Lude.Int
pirsResponseStatus = Lens.lens (responseStatus :: PutInventoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutInventoryResponse)
{-# DEPRECATED pirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
