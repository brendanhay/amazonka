{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.ListGlobalTables
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all global tables that have a replica in the specified Region.
module Network.AWS.DynamoDB.ListGlobalTables
  ( -- * Creating a request
    ListGlobalTables (..),
    mkListGlobalTables,

    -- ** Request lenses
    lgtRegionName,
    lgtExclusiveStartGlobalTableName,
    lgtLimit,

    -- * Destructuring the response
    ListGlobalTablesResponse (..),
    mkListGlobalTablesResponse,

    -- ** Response lenses
    lgtrsLastEvaluatedGlobalTableName,
    lgtrsGlobalTables,
    lgtrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListGlobalTables' smart constructor.
data ListGlobalTables = ListGlobalTables'
  { -- | Lists the global tables in a specific Region.
    regionName :: Lude.Maybe Lude.Text,
    -- | The first global table name that this operation will evaluate.
    exclusiveStartGlobalTableName :: Lude.Maybe Lude.Text,
    -- | The maximum number of table names to return, if the parameter is not specified DynamoDB defaults to 100.
    --
    -- If the number of global tables DynamoDB finds reaches this limit, it stops the operation and returns the table names collected up to that point, with a table name in the @LastEvaluatedGlobalTableName@ to apply in a subsequent operation to the @ExclusiveStartGlobalTableName@ parameter.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGlobalTables' with the minimum fields required to make a request.
--
-- * 'regionName' - Lists the global tables in a specific Region.
-- * 'exclusiveStartGlobalTableName' - The first global table name that this operation will evaluate.
-- * 'limit' - The maximum number of table names to return, if the parameter is not specified DynamoDB defaults to 100.
--
-- If the number of global tables DynamoDB finds reaches this limit, it stops the operation and returns the table names collected up to that point, with a table name in the @LastEvaluatedGlobalTableName@ to apply in a subsequent operation to the @ExclusiveStartGlobalTableName@ parameter.
mkListGlobalTables ::
  ListGlobalTables
mkListGlobalTables =
  ListGlobalTables'
    { regionName = Lude.Nothing,
      exclusiveStartGlobalTableName = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | Lists the global tables in a specific Region.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgtRegionName :: Lens.Lens' ListGlobalTables (Lude.Maybe Lude.Text)
lgtRegionName = Lens.lens (regionName :: ListGlobalTables -> Lude.Maybe Lude.Text) (\s a -> s {regionName = a} :: ListGlobalTables)
{-# DEPRECATED lgtRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

-- | The first global table name that this operation will evaluate.
--
-- /Note:/ Consider using 'exclusiveStartGlobalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgtExclusiveStartGlobalTableName :: Lens.Lens' ListGlobalTables (Lude.Maybe Lude.Text)
lgtExclusiveStartGlobalTableName = Lens.lens (exclusiveStartGlobalTableName :: ListGlobalTables -> Lude.Maybe Lude.Text) (\s a -> s {exclusiveStartGlobalTableName = a} :: ListGlobalTables)
{-# DEPRECATED lgtExclusiveStartGlobalTableName "Use generic-lens or generic-optics with 'exclusiveStartGlobalTableName' instead." #-}

-- | The maximum number of table names to return, if the parameter is not specified DynamoDB defaults to 100.
--
-- If the number of global tables DynamoDB finds reaches this limit, it stops the operation and returns the table names collected up to that point, with a table name in the @LastEvaluatedGlobalTableName@ to apply in a subsequent operation to the @ExclusiveStartGlobalTableName@ parameter.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgtLimit :: Lens.Lens' ListGlobalTables (Lude.Maybe Lude.Natural)
lgtLimit = Lens.lens (limit :: ListGlobalTables -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListGlobalTables)
{-# DEPRECATED lgtLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest ListGlobalTables where
  type Rs ListGlobalTables = ListGlobalTablesResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListGlobalTablesResponse'
            Lude.<$> (x Lude..?> "LastEvaluatedGlobalTableName")
            Lude.<*> (x Lude..?> "GlobalTables" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListGlobalTables where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.ListGlobalTables" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListGlobalTables where
  toJSON ListGlobalTables' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RegionName" Lude..=) Lude.<$> regionName,
            ("ExclusiveStartGlobalTableName" Lude..=)
              Lude.<$> exclusiveStartGlobalTableName,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListGlobalTables where
  toPath = Lude.const "/"

instance Lude.ToQuery ListGlobalTables where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListGlobalTablesResponse' smart constructor.
data ListGlobalTablesResponse = ListGlobalTablesResponse'
  { -- | Last evaluated global table name.
    lastEvaluatedGlobalTableName :: Lude.Maybe Lude.Text,
    -- | List of global table names.
    globalTables :: Lude.Maybe [GlobalTable],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGlobalTablesResponse' with the minimum fields required to make a request.
--
-- * 'lastEvaluatedGlobalTableName' - Last evaluated global table name.
-- * 'globalTables' - List of global table names.
-- * 'responseStatus' - The response status code.
mkListGlobalTablesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListGlobalTablesResponse
mkListGlobalTablesResponse pResponseStatus_ =
  ListGlobalTablesResponse'
    { lastEvaluatedGlobalTableName =
        Lude.Nothing,
      globalTables = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Last evaluated global table name.
--
-- /Note:/ Consider using 'lastEvaluatedGlobalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgtrsLastEvaluatedGlobalTableName :: Lens.Lens' ListGlobalTablesResponse (Lude.Maybe Lude.Text)
lgtrsLastEvaluatedGlobalTableName = Lens.lens (lastEvaluatedGlobalTableName :: ListGlobalTablesResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastEvaluatedGlobalTableName = a} :: ListGlobalTablesResponse)
{-# DEPRECATED lgtrsLastEvaluatedGlobalTableName "Use generic-lens or generic-optics with 'lastEvaluatedGlobalTableName' instead." #-}

-- | List of global table names.
--
-- /Note:/ Consider using 'globalTables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgtrsGlobalTables :: Lens.Lens' ListGlobalTablesResponse (Lude.Maybe [GlobalTable])
lgtrsGlobalTables = Lens.lens (globalTables :: ListGlobalTablesResponse -> Lude.Maybe [GlobalTable]) (\s a -> s {globalTables = a} :: ListGlobalTablesResponse)
{-# DEPRECATED lgtrsGlobalTables "Use generic-lens or generic-optics with 'globalTables' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgtrsResponseStatus :: Lens.Lens' ListGlobalTablesResponse Lude.Int
lgtrsResponseStatus = Lens.lens (responseStatus :: ListGlobalTablesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListGlobalTablesResponse)
{-# DEPRECATED lgtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
