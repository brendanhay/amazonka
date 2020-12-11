{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.ListTables
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of table names associated with the current account and endpoint. The output from @ListTables@ is paginated, with each page returning a maximum of 100 table names.
--
-- This operation returns paginated results.
module Network.AWS.DynamoDB.ListTables
  ( -- * Creating a request
    ListTables (..),
    mkListTables,

    -- ** Request lenses
    ltExclusiveStartTableName,
    ltLimit,

    -- * Destructuring the response
    ListTablesResponse (..),
    mkListTablesResponse,

    -- ** Response lenses
    ltrsLastEvaluatedTableName,
    ltrsTableNames,
    ltrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @ListTables@ operation.
--
-- /See:/ 'mkListTables' smart constructor.
data ListTables = ListTables'
  { exclusiveStartTableName ::
      Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTables' with the minimum fields required to make a request.
--
-- * 'exclusiveStartTableName' - The first table name that this operation will evaluate. Use the value that was returned for @LastEvaluatedTableName@ in a previous operation, so that you can obtain the next page of results.
-- * 'limit' - A maximum number of table names to return. If this parameter is not specified, the limit is 100.
mkListTables ::
  ListTables
mkListTables =
  ListTables'
    { exclusiveStartTableName = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The first table name that this operation will evaluate. Use the value that was returned for @LastEvaluatedTableName@ in a previous operation, so that you can obtain the next page of results.
--
-- /Note:/ Consider using 'exclusiveStartTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltExclusiveStartTableName :: Lens.Lens' ListTables (Lude.Maybe Lude.Text)
ltExclusiveStartTableName = Lens.lens (exclusiveStartTableName :: ListTables -> Lude.Maybe Lude.Text) (\s a -> s {exclusiveStartTableName = a} :: ListTables)
{-# DEPRECATED ltExclusiveStartTableName "Use generic-lens or generic-optics with 'exclusiveStartTableName' instead." #-}

-- | A maximum number of table names to return. If this parameter is not specified, the limit is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLimit :: Lens.Lens' ListTables (Lude.Maybe Lude.Natural)
ltLimit = Lens.lens (limit :: ListTables -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListTables)
{-# DEPRECATED ltLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListTables where
  page rq rs
    | Page.stop (rs Lens.^. ltrsLastEvaluatedTableName) = Lude.Nothing
    | Page.stop (rs Lens.^. ltrsTableNames) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltExclusiveStartTableName
          Lens..~ rs Lens.^. ltrsLastEvaluatedTableName

instance Lude.AWSRequest ListTables where
  type Rs ListTables = ListTablesResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTablesResponse'
            Lude.<$> (x Lude..?> "LastEvaluatedTableName")
            Lude.<*> (x Lude..?> "TableNames" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTables where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.ListTables" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTables where
  toJSON ListTables' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ExclusiveStartTableName" Lude..=)
              Lude.<$> exclusiveStartTableName,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListTables where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTables where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @ListTables@ operation.
--
-- /See:/ 'mkListTablesResponse' smart constructor.
data ListTablesResponse = ListTablesResponse'
  { lastEvaluatedTableName ::
      Lude.Maybe Lude.Text,
    tableNames :: Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'ListTablesResponse' with the minimum fields required to make a request.
--
-- * 'lastEvaluatedTableName' - The name of the last table in the current page of results. Use this value as the @ExclusiveStartTableName@ in a new request to obtain the next page of results, until all the table names are returned.
--
-- If you do not receive a @LastEvaluatedTableName@ value in the response, this means that there are no more table names to be retrieved.
-- * 'responseStatus' - The response status code.
-- * 'tableNames' - The names of the tables associated with the current account at the current endpoint. The maximum size of this array is 100.
--
-- If @LastEvaluatedTableName@ also appears in the output, you can use this value as the @ExclusiveStartTableName@ parameter in a subsequent @ListTables@ request and obtain the next page of results.
mkListTablesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTablesResponse
mkListTablesResponse pResponseStatus_ =
  ListTablesResponse'
    { lastEvaluatedTableName = Lude.Nothing,
      tableNames = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the last table in the current page of results. Use this value as the @ExclusiveStartTableName@ in a new request to obtain the next page of results, until all the table names are returned.
--
-- If you do not receive a @LastEvaluatedTableName@ value in the response, this means that there are no more table names to be retrieved.
--
-- /Note:/ Consider using 'lastEvaluatedTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsLastEvaluatedTableName :: Lens.Lens' ListTablesResponse (Lude.Maybe Lude.Text)
ltrsLastEvaluatedTableName = Lens.lens (lastEvaluatedTableName :: ListTablesResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastEvaluatedTableName = a} :: ListTablesResponse)
{-# DEPRECATED ltrsLastEvaluatedTableName "Use generic-lens or generic-optics with 'lastEvaluatedTableName' instead." #-}

-- | The names of the tables associated with the current account at the current endpoint. The maximum size of this array is 100.
--
-- If @LastEvaluatedTableName@ also appears in the output, you can use this value as the @ExclusiveStartTableName@ parameter in a subsequent @ListTables@ request and obtain the next page of results.
--
-- /Note:/ Consider using 'tableNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsTableNames :: Lens.Lens' ListTablesResponse (Lude.Maybe [Lude.Text])
ltrsTableNames = Lens.lens (tableNames :: ListTablesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {tableNames = a} :: ListTablesResponse)
{-# DEPRECATED ltrsTableNames "Use generic-lens or generic-optics with 'tableNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsResponseStatus :: Lens.Lens' ListTablesResponse Lude.Int
ltrsResponseStatus = Lens.lens (responseStatus :: ListTablesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTablesResponse)
{-# DEPRECATED ltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
