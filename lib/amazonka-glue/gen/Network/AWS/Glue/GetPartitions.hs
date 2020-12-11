{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetPartitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the partitions in a table.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetPartitions
  ( -- * Creating a request
    GetPartitions (..),
    mkGetPartitions,

    -- ** Request lenses
    gpsCatalogId,
    gpsNextToken,
    gpsExpression,
    gpsSegment,
    gpsMaxResults,
    gpsDatabaseName,
    gpsTableName,

    -- * Destructuring the response
    GetPartitionsResponse (..),
    mkGetPartitionsResponse,

    -- ** Response lenses
    gpsrsPartitions,
    gpsrsNextToken,
    gpsrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetPartitions' smart constructor.
data GetPartitions = GetPartitions'
  { catalogId ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    expression :: Lude.Maybe Lude.Text,
    segment :: Lude.Maybe Segment,
    maxResults :: Lude.Maybe Lude.Natural,
    databaseName :: Lude.Text,
    tableName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPartitions' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the partitions in question reside. If none is provided, the AWS account ID is used by default.
-- * 'databaseName' - The name of the catalog database where the partitions reside.
-- * 'expression' - An expression that filters the partitions to be returned.
--
-- The expression uses SQL syntax similar to the SQL @WHERE@ filter clause. The SQL statement parser <http://jsqlparser.sourceforge.net/home.php JSQLParser> parses the expression.
-- /Operators/ : The following are the operators that you can use in the @Expression@ API call:
--
--     * =
--
--     * Checks whether the values of the two operands are equal; if yes, then the condition becomes true.
-- Example: Assume 'variable a' holds 10 and 'variable b' holds 20.
-- (a = b) is not true.
--
--
--     * < >
--
--     * Checks whether the values of two operands are equal; if the values are not equal, then the condition becomes true.
-- Example: (a < > b) is true.
--
--
--     * >
--
--     * Checks whether the value of the left operand is greater than the value of the right operand; if yes, then the condition becomes true.
-- Example: (a > b) is not true.
--
--
--     * <
--
--     * Checks whether the value of the left operand is less than the value of the right operand; if yes, then the condition becomes true.
-- Example: (a < b) is true.
--
--
--     * >=
--
--     * Checks whether the value of the left operand is greater than or equal to the value of the right operand; if yes, then the condition becomes true.
-- Example: (a >= b) is not true.
--
--
--     * <=
--
--     * Checks whether the value of the left operand is less than or equal to the value of the right operand; if yes, then the condition becomes true.
-- Example: (a <= b) is true.
--
--
--     * AND, OR, IN, BETWEEN, LIKE, NOT, IS NULL
--
--     * Logical operators.
--
--
-- /Supported Partition Key Types/ : The following are the supported partition keys.
--
--     * @string@
--
--
--     * @date@
--
--
--     * @timestamp@
--
--
--     * @int@
--
--
--     * @bigint@
--
--
--     * @long@
--
--
--     * @tinyint@
--
--
--     * @smallint@
--
--
--     * @decimal@
--
--
-- If an invalid type is encountered, an exception is thrown.
-- The following list shows the valid operators on each type. When you define a crawler, the @partitionKey@ type is created as a @STRING@ , to be compatible with the catalog partitions.
-- /Sample API Call/ :
-- * 'maxResults' - The maximum number of partitions to return in a single response.
-- * 'nextToken' - A continuation token, if this is not the first call to retrieve these partitions.
-- * 'segment' - The segment of the table's partitions to scan in this request.
-- * 'tableName' - The name of the partitions' table.
mkGetPartitions ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  GetPartitions
mkGetPartitions pDatabaseName_ pTableName_ =
  GetPartitions'
    { catalogId = Lude.Nothing,
      nextToken = Lude.Nothing,
      expression = Lude.Nothing,
      segment = Lude.Nothing,
      maxResults = Lude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | The ID of the Data Catalog where the partitions in question reside. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsCatalogId :: Lens.Lens' GetPartitions (Lude.Maybe Lude.Text)
gpsCatalogId = Lens.lens (catalogId :: GetPartitions -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: GetPartitions)
{-# DEPRECATED gpsCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | A continuation token, if this is not the first call to retrieve these partitions.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsNextToken :: Lens.Lens' GetPartitions (Lude.Maybe Lude.Text)
gpsNextToken = Lens.lens (nextToken :: GetPartitions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetPartitions)
{-# DEPRECATED gpsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An expression that filters the partitions to be returned.
--
-- The expression uses SQL syntax similar to the SQL @WHERE@ filter clause. The SQL statement parser <http://jsqlparser.sourceforge.net/home.php JSQLParser> parses the expression.
-- /Operators/ : The following are the operators that you can use in the @Expression@ API call:
--
--     * =
--
--     * Checks whether the values of the two operands are equal; if yes, then the condition becomes true.
-- Example: Assume 'variable a' holds 10 and 'variable b' holds 20.
-- (a = b) is not true.
--
--
--     * < >
--
--     * Checks whether the values of two operands are equal; if the values are not equal, then the condition becomes true.
-- Example: (a < > b) is true.
--
--
--     * >
--
--     * Checks whether the value of the left operand is greater than the value of the right operand; if yes, then the condition becomes true.
-- Example: (a > b) is not true.
--
--
--     * <
--
--     * Checks whether the value of the left operand is less than the value of the right operand; if yes, then the condition becomes true.
-- Example: (a < b) is true.
--
--
--     * >=
--
--     * Checks whether the value of the left operand is greater than or equal to the value of the right operand; if yes, then the condition becomes true.
-- Example: (a >= b) is not true.
--
--
--     * <=
--
--     * Checks whether the value of the left operand is less than or equal to the value of the right operand; if yes, then the condition becomes true.
-- Example: (a <= b) is true.
--
--
--     * AND, OR, IN, BETWEEN, LIKE, NOT, IS NULL
--
--     * Logical operators.
--
--
-- /Supported Partition Key Types/ : The following are the supported partition keys.
--
--     * @string@
--
--
--     * @date@
--
--
--     * @timestamp@
--
--
--     * @int@
--
--
--     * @bigint@
--
--
--     * @long@
--
--
--     * @tinyint@
--
--
--     * @smallint@
--
--
--     * @decimal@
--
--
-- If an invalid type is encountered, an exception is thrown.
-- The following list shows the valid operators on each type. When you define a crawler, the @partitionKey@ type is created as a @STRING@ , to be compatible with the catalog partitions.
-- /Sample API Call/ :
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsExpression :: Lens.Lens' GetPartitions (Lude.Maybe Lude.Text)
gpsExpression = Lens.lens (expression :: GetPartitions -> Lude.Maybe Lude.Text) (\s a -> s {expression = a} :: GetPartitions)
{-# DEPRECATED gpsExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

-- | The segment of the table's partitions to scan in this request.
--
-- /Note:/ Consider using 'segment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsSegment :: Lens.Lens' GetPartitions (Lude.Maybe Segment)
gpsSegment = Lens.lens (segment :: GetPartitions -> Lude.Maybe Segment) (\s a -> s {segment = a} :: GetPartitions)
{-# DEPRECATED gpsSegment "Use generic-lens or generic-optics with 'segment' instead." #-}

-- | The maximum number of partitions to return in a single response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsMaxResults :: Lens.Lens' GetPartitions (Lude.Maybe Lude.Natural)
gpsMaxResults = Lens.lens (maxResults :: GetPartitions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetPartitions)
{-# DEPRECATED gpsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the catalog database where the partitions reside.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsDatabaseName :: Lens.Lens' GetPartitions Lude.Text
gpsDatabaseName = Lens.lens (databaseName :: GetPartitions -> Lude.Text) (\s a -> s {databaseName = a} :: GetPartitions)
{-# DEPRECATED gpsDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the partitions' table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsTableName :: Lens.Lens' GetPartitions Lude.Text
gpsTableName = Lens.lens (tableName :: GetPartitions -> Lude.Text) (\s a -> s {tableName = a} :: GetPartitions)
{-# DEPRECATED gpsTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Page.AWSPager GetPartitions where
  page rq rs
    | Page.stop (rs Lens.^. gpsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gpsrsPartitions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gpsNextToken Lens..~ rs Lens.^. gpsrsNextToken

instance Lude.AWSRequest GetPartitions where
  type Rs GetPartitions = GetPartitionsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPartitionsResponse'
            Lude.<$> (x Lude..?> "Partitions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPartitions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetPartitions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetPartitions where
  toJSON GetPartitions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Expression" Lude..=) Lude.<$> expression,
            ("Segment" Lude..=) Lude.<$> segment,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

instance Lude.ToPath GetPartitions where
  toPath = Lude.const "/"

instance Lude.ToQuery GetPartitions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetPartitionsResponse' smart constructor.
data GetPartitionsResponse = GetPartitionsResponse'
  { partitions ::
      Lude.Maybe [Partition],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetPartitionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, if the returned list of partitions does not include the last one.
-- * 'partitions' - A list of requested partitions.
-- * 'responseStatus' - The response status code.
mkGetPartitionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPartitionsResponse
mkGetPartitionsResponse pResponseStatus_ =
  GetPartitionsResponse'
    { partitions = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of requested partitions.
--
-- /Note:/ Consider using 'partitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrsPartitions :: Lens.Lens' GetPartitionsResponse (Lude.Maybe [Partition])
gpsrsPartitions = Lens.lens (partitions :: GetPartitionsResponse -> Lude.Maybe [Partition]) (\s a -> s {partitions = a} :: GetPartitionsResponse)
{-# DEPRECATED gpsrsPartitions "Use generic-lens or generic-optics with 'partitions' instead." #-}

-- | A continuation token, if the returned list of partitions does not include the last one.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrsNextToken :: Lens.Lens' GetPartitionsResponse (Lude.Maybe Lude.Text)
gpsrsNextToken = Lens.lens (nextToken :: GetPartitionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetPartitionsResponse)
{-# DEPRECATED gpsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrsResponseStatus :: Lens.Lens' GetPartitionsResponse Lude.Int
gpsrsResponseStatus = Lens.lens (responseStatus :: GetPartitionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPartitionsResponse)
{-# DEPRECATED gpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
