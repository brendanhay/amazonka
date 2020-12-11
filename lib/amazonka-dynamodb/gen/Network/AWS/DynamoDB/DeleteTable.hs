{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DeleteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteTable@ operation deletes a table and all of its items. After a @DeleteTable@ request, the specified table is in the @DELETING@ state until DynamoDB completes the deletion. If the table is in the @ACTIVE@ state, you can delete it. If a table is in @CREATING@ or @UPDATING@ states, then DynamoDB returns a @ResourceInUseException@ . If the specified table does not exist, DynamoDB returns a @ResourceNotFoundException@ . If table is already in the @DELETING@ state, no error is returned.
--
-- When you delete a table, any indexes on that table are also deleted.
-- If you have DynamoDB Streams enabled on the table, then the corresponding stream on that table goes into the @DISABLED@ state, and the stream is automatically deleted after 24 hours.
-- Use the @DescribeTable@ action to check the status of the table.
module Network.AWS.DynamoDB.DeleteTable
  ( -- * Creating a request
    DeleteTable (..),
    mkDeleteTable,

    -- ** Request lenses
    dtTableName,

    -- * Destructuring the response
    DeleteTableResponse (..),
    mkDeleteTableResponse,

    -- ** Response lenses
    dtrsTableDescription,
    dtrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DeleteTable@ operation.
--
-- /See:/ 'mkDeleteTable' smart constructor.
newtype DeleteTable = DeleteTable' {tableName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTable' with the minimum fields required to make a request.
--
-- * 'tableName' - The name of the table to delete.
mkDeleteTable ::
  -- | 'tableName'
  Lude.Text ->
  DeleteTable
mkDeleteTable pTableName_ = DeleteTable' {tableName = pTableName_}

-- | The name of the table to delete.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTableName :: Lens.Lens' DeleteTable Lude.Text
dtTableName = Lens.lens (tableName :: DeleteTable -> Lude.Text) (\s a -> s {tableName = a} :: DeleteTable)
{-# DEPRECATED dtTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest DeleteTable where
  type Rs DeleteTable = DeleteTableResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteTableResponse'
            Lude.<$> (x Lude..?> "TableDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTable where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.DeleteTable" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteTable where
  toJSON DeleteTable' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TableName" Lude..= tableName)])

instance Lude.ToPath DeleteTable where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTable where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @DeleteTable@ operation.
--
-- /See:/ 'mkDeleteTableResponse' smart constructor.
data DeleteTableResponse = DeleteTableResponse'
  { tableDescription ::
      Lude.Maybe TableDescription,
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

-- | Creates a value of 'DeleteTableResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'tableDescription' - Represents the properties of a table.
mkDeleteTableResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTableResponse
mkDeleteTableResponse pResponseStatus_ =
  DeleteTableResponse'
    { tableDescription = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Represents the properties of a table.
--
-- /Note:/ Consider using 'tableDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsTableDescription :: Lens.Lens' DeleteTableResponse (Lude.Maybe TableDescription)
dtrsTableDescription = Lens.lens (tableDescription :: DeleteTableResponse -> Lude.Maybe TableDescription) (\s a -> s {tableDescription = a} :: DeleteTableResponse)
{-# DEPRECATED dtrsTableDescription "Use generic-lens or generic-optics with 'tableDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResponseStatus :: Lens.Lens' DeleteTableResponse Lude.Int
dtrsResponseStatus = Lens.lens (responseStatus :: DeleteTableResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTableResponse)
{-# DEPRECATED dtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
