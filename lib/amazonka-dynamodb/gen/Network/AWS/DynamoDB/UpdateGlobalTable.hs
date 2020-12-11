{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.UpdateGlobalTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or removes replicas in the specified global table. The global table must already exist to be able to use this operation. Any replica to be added must be empty, have the same name as the global table, have the same key schema, have DynamoDB Streams enabled, and have the same provisioned and maximum write capacity units.
--
-- If global secondary indexes are specified, then the following conditions must also be met:
--
--     * The global secondary indexes must have the same name.
--
--
--     * The global secondary indexes must have the same hash key and sort key (if present).
--
--
--     * The global secondary indexes must have the same provisioned and maximum write capacity units.
module Network.AWS.DynamoDB.UpdateGlobalTable
  ( -- * Creating a request
    UpdateGlobalTable (..),
    mkUpdateGlobalTable,

    -- ** Request lenses
    ugtGlobalTableName,
    ugtReplicaUpdates,

    -- * Destructuring the response
    UpdateGlobalTableResponse (..),
    mkUpdateGlobalTableResponse,

    -- ** Response lenses
    ugtrsGlobalTableDescription,
    ugtrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateGlobalTable' smart constructor.
data UpdateGlobalTable = UpdateGlobalTable'
  { globalTableName ::
      Lude.Text,
    replicaUpdates :: [ReplicaUpdate]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGlobalTable' with the minimum fields required to make a request.
--
-- * 'globalTableName' - The global table name.
-- * 'replicaUpdates' - A list of Regions that should be added or removed from the global table.
mkUpdateGlobalTable ::
  -- | 'globalTableName'
  Lude.Text ->
  UpdateGlobalTable
mkUpdateGlobalTable pGlobalTableName_ =
  UpdateGlobalTable'
    { globalTableName = pGlobalTableName_,
      replicaUpdates = Lude.mempty
    }

-- | The global table name.
--
-- /Note:/ Consider using 'globalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtGlobalTableName :: Lens.Lens' UpdateGlobalTable Lude.Text
ugtGlobalTableName = Lens.lens (globalTableName :: UpdateGlobalTable -> Lude.Text) (\s a -> s {globalTableName = a} :: UpdateGlobalTable)
{-# DEPRECATED ugtGlobalTableName "Use generic-lens or generic-optics with 'globalTableName' instead." #-}

-- | A list of Regions that should be added or removed from the global table.
--
-- /Note:/ Consider using 'replicaUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtReplicaUpdates :: Lens.Lens' UpdateGlobalTable [ReplicaUpdate]
ugtReplicaUpdates = Lens.lens (replicaUpdates :: UpdateGlobalTable -> [ReplicaUpdate]) (\s a -> s {replicaUpdates = a} :: UpdateGlobalTable)
{-# DEPRECATED ugtReplicaUpdates "Use generic-lens or generic-optics with 'replicaUpdates' instead." #-}

instance Lude.AWSRequest UpdateGlobalTable where
  type Rs UpdateGlobalTable = UpdateGlobalTableResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateGlobalTableResponse'
            Lude.<$> (x Lude..?> "GlobalTableDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateGlobalTable where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.UpdateGlobalTable" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateGlobalTable where
  toJSON UpdateGlobalTable' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GlobalTableName" Lude..= globalTableName),
            Lude.Just ("ReplicaUpdates" Lude..= replicaUpdates)
          ]
      )

instance Lude.ToPath UpdateGlobalTable where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateGlobalTable where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateGlobalTableResponse' smart constructor.
data UpdateGlobalTableResponse = UpdateGlobalTableResponse'
  { globalTableDescription ::
      Lude.Maybe GlobalTableDescription,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGlobalTableResponse' with the minimum fields required to make a request.
--
-- * 'globalTableDescription' - Contains the details of the global table.
-- * 'responseStatus' - The response status code.
mkUpdateGlobalTableResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateGlobalTableResponse
mkUpdateGlobalTableResponse pResponseStatus_ =
  UpdateGlobalTableResponse'
    { globalTableDescription = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains the details of the global table.
--
-- /Note:/ Consider using 'globalTableDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtrsGlobalTableDescription :: Lens.Lens' UpdateGlobalTableResponse (Lude.Maybe GlobalTableDescription)
ugtrsGlobalTableDescription = Lens.lens (globalTableDescription :: UpdateGlobalTableResponse -> Lude.Maybe GlobalTableDescription) (\s a -> s {globalTableDescription = a} :: UpdateGlobalTableResponse)
{-# DEPRECATED ugtrsGlobalTableDescription "Use generic-lens or generic-optics with 'globalTableDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtrsResponseStatus :: Lens.Lens' UpdateGlobalTableResponse Lude.Int
ugtrsResponseStatus = Lens.lens (responseStatus :: UpdateGlobalTableResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateGlobalTableResponse)
{-# DEPRECATED ugtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
