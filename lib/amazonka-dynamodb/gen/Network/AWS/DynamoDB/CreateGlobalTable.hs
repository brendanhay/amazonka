{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.CreateGlobalTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a global table from an existing table. A global table creates a replication relationship between two or more DynamoDB tables with the same table name in the provided Regions.
--
-- If you want to add a new replica table to a global table, each of the following conditions must be true:
--
--     * The table must have the same primary key as all of the other replicas.
--
--
--     * The table must have the same name as all of the other replicas.
--
--
--     * The table must have DynamoDB Streams enabled, with the stream containing both the new and the old images of the item.
--
--
--     * None of the replica tables in the global table can contain any data.
--
--
-- If global secondary indexes are specified, then the following conditions must also be met:
--
--     * The global secondary indexes must have the same name.
--
--
--     * The global secondary indexes must have the same hash key and sort key (if present).
--
--
-- If local secondary indexes are specified, then the following conditions must also be met:
--
--     * The local secondary indexes must have the same name.
--
--
--     * The local secondary indexes must have the same hash key and sort key (if present).
--
--
-- /Important:/ Write capacity settings should be set consistently across your replica tables and secondary indexes. DynamoDB strongly recommends enabling auto scaling to manage the write capacity settings for all of your global tables replicas and indexes.
-- If you prefer to manage write capacity settings manually, you should provision equal replicated write capacity units to your replica tables. You should also provision equal replicated write capacity units to matching secondary indexes across your global table.
module Network.AWS.DynamoDB.CreateGlobalTable
  ( -- * Creating a request
    CreateGlobalTable (..),
    mkCreateGlobalTable,

    -- ** Request lenses
    cgtGlobalTableName,
    cgtReplicationGroup,

    -- * Destructuring the response
    CreateGlobalTableResponse (..),
    mkCreateGlobalTableResponse,

    -- ** Response lenses
    cgtrsGlobalTableDescription,
    cgtrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateGlobalTable' smart constructor.
data CreateGlobalTable = CreateGlobalTable'
  { -- | The global table name.
    globalTableName :: Lude.Text,
    -- | The Regions where the global table needs to be created.
    replicationGroup :: [Replica]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGlobalTable' with the minimum fields required to make a request.
--
-- * 'globalTableName' - The global table name.
-- * 'replicationGroup' - The Regions where the global table needs to be created.
mkCreateGlobalTable ::
  -- | 'globalTableName'
  Lude.Text ->
  CreateGlobalTable
mkCreateGlobalTable pGlobalTableName_ =
  CreateGlobalTable'
    { globalTableName = pGlobalTableName_,
      replicationGroup = Lude.mempty
    }

-- | The global table name.
--
-- /Note:/ Consider using 'globalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgtGlobalTableName :: Lens.Lens' CreateGlobalTable Lude.Text
cgtGlobalTableName = Lens.lens (globalTableName :: CreateGlobalTable -> Lude.Text) (\s a -> s {globalTableName = a} :: CreateGlobalTable)
{-# DEPRECATED cgtGlobalTableName "Use generic-lens or generic-optics with 'globalTableName' instead." #-}

-- | The Regions where the global table needs to be created.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgtReplicationGroup :: Lens.Lens' CreateGlobalTable [Replica]
cgtReplicationGroup = Lens.lens (replicationGroup :: CreateGlobalTable -> [Replica]) (\s a -> s {replicationGroup = a} :: CreateGlobalTable)
{-# DEPRECATED cgtReplicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead." #-}

instance Lude.AWSRequest CreateGlobalTable where
  type Rs CreateGlobalTable = CreateGlobalTableResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateGlobalTableResponse'
            Lude.<$> (x Lude..?> "GlobalTableDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateGlobalTable where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.CreateGlobalTable" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateGlobalTable where
  toJSON CreateGlobalTable' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GlobalTableName" Lude..= globalTableName),
            Lude.Just ("ReplicationGroup" Lude..= replicationGroup)
          ]
      )

instance Lude.ToPath CreateGlobalTable where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateGlobalTable where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateGlobalTableResponse' smart constructor.
data CreateGlobalTableResponse = CreateGlobalTableResponse'
  { -- | Contains the details of the global table.
    globalTableDescription :: Lude.Maybe GlobalTableDescription,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGlobalTableResponse' with the minimum fields required to make a request.
--
-- * 'globalTableDescription' - Contains the details of the global table.
-- * 'responseStatus' - The response status code.
mkCreateGlobalTableResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateGlobalTableResponse
mkCreateGlobalTableResponse pResponseStatus_ =
  CreateGlobalTableResponse'
    { globalTableDescription = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains the details of the global table.
--
-- /Note:/ Consider using 'globalTableDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgtrsGlobalTableDescription :: Lens.Lens' CreateGlobalTableResponse (Lude.Maybe GlobalTableDescription)
cgtrsGlobalTableDescription = Lens.lens (globalTableDescription :: CreateGlobalTableResponse -> Lude.Maybe GlobalTableDescription) (\s a -> s {globalTableDescription = a} :: CreateGlobalTableResponse)
{-# DEPRECATED cgtrsGlobalTableDescription "Use generic-lens or generic-optics with 'globalTableDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgtrsResponseStatus :: Lens.Lens' CreateGlobalTableResponse Lude.Int
cgtrsResponseStatus = Lens.lens (responseStatus :: CreateGlobalTableResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateGlobalTableResponse)
{-# DEPRECATED cgtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
