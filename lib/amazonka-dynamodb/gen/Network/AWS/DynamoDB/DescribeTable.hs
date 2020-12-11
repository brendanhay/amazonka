{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the table, including the current status of the table, when it was created, the primary key schema, and any indexes on the table.
module Network.AWS.DynamoDB.DescribeTable
  ( -- * Creating a request
    DescribeTable (..),
    mkDescribeTable,

    -- ** Request lenses
    desTableName,

    -- * Destructuring the response
    DescribeTableResponse (..),
    mkDescribeTableResponse,

    -- ** Response lenses
    drsTable,
    drsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DescribeTable@ operation.
--
-- /See:/ 'mkDescribeTable' smart constructor.
newtype DescribeTable = DescribeTable' {tableName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTable' with the minimum fields required to make a request.
--
-- * 'tableName' - The name of the table to describe.
mkDescribeTable ::
  -- | 'tableName'
  Lude.Text ->
  DescribeTable
mkDescribeTable pTableName_ =
  DescribeTable' {tableName = pTableName_}

-- | The name of the table to describe.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desTableName :: Lens.Lens' DescribeTable Lude.Text
desTableName = Lens.lens (tableName :: DescribeTable -> Lude.Text) (\s a -> s {tableName = a} :: DescribeTable)
{-# DEPRECATED desTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest DescribeTable where
  type Rs DescribeTable = DescribeTableResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTableResponse'
            Lude.<$> (x Lude..?> "Table") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTable where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.DescribeTable" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTable where
  toJSON DescribeTable' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TableName" Lude..= tableName)])

instance Lude.ToPath DescribeTable where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTable where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @DescribeTable@ operation.
--
-- /See:/ 'mkDescribeTableResponse' smart constructor.
data DescribeTableResponse = DescribeTableResponse'
  { table ::
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

-- | Creates a value of 'DescribeTableResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'table' - The properties of the table.
mkDescribeTableResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTableResponse
mkDescribeTableResponse pResponseStatus_ =
  DescribeTableResponse'
    { table = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The properties of the table.
--
-- /Note:/ Consider using 'table' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsTable :: Lens.Lens' DescribeTableResponse (Lude.Maybe TableDescription)
drsTable = Lens.lens (table :: DescribeTableResponse -> Lude.Maybe TableDescription) (\s a -> s {table = a} :: DescribeTableResponse)
{-# DEPRECATED drsTable "Use generic-lens or generic-optics with 'table' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeTableResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeTableResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTableResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
