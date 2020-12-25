{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gpsDatabaseName,
    gpsTableName,
    gpsCatalogId,
    gpsExpression,
    gpsMaxResults,
    gpsNextToken,
    gpsSegment,

    -- * Destructuring the response
    GetPartitionsResponse (..),
    mkGetPartitionsResponse,

    -- ** Response lenses
    gprgrsNextToken,
    gprgrsPartitions,
    gprgrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetPartitions' smart constructor.
data GetPartitions = GetPartitions'
  { -- | The name of the catalog database where the partitions reside.
    databaseName :: Types.DatabaseName,
    -- | The name of the partitions' table.
    tableName :: Types.TableName,
    -- | The ID of the Data Catalog where the partitions in question reside. If none is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogId,
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
    expression :: Core.Maybe Types.PredicateString,
    -- | The maximum number of partitions to return in a single response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A continuation token, if this is not the first call to retrieve these partitions.
    nextToken :: Core.Maybe Types.Token,
    -- | The segment of the table's partitions to scan in this request.
    segment :: Core.Maybe Types.Segment
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPartitions' value with any optional fields omitted.
mkGetPartitions ::
  -- | 'databaseName'
  Types.DatabaseName ->
  -- | 'tableName'
  Types.TableName ->
  GetPartitions
mkGetPartitions databaseName tableName =
  GetPartitions'
    { databaseName,
      tableName,
      catalogId = Core.Nothing,
      expression = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      segment = Core.Nothing
    }

-- | The name of the catalog database where the partitions reside.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsDatabaseName :: Lens.Lens' GetPartitions Types.DatabaseName
gpsDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED gpsDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the partitions' table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsTableName :: Lens.Lens' GetPartitions Types.TableName
gpsTableName = Lens.field @"tableName"
{-# DEPRECATED gpsTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The ID of the Data Catalog where the partitions in question reside. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsCatalogId :: Lens.Lens' GetPartitions (Core.Maybe Types.CatalogId)
gpsCatalogId = Lens.field @"catalogId"
{-# DEPRECATED gpsCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

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
gpsExpression :: Lens.Lens' GetPartitions (Core.Maybe Types.PredicateString)
gpsExpression = Lens.field @"expression"
{-# DEPRECATED gpsExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

-- | The maximum number of partitions to return in a single response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsMaxResults :: Lens.Lens' GetPartitions (Core.Maybe Core.Natural)
gpsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gpsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A continuation token, if this is not the first call to retrieve these partitions.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsNextToken :: Lens.Lens' GetPartitions (Core.Maybe Types.Token)
gpsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gpsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The segment of the table's partitions to scan in this request.
--
-- /Note:/ Consider using 'segment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsSegment :: Lens.Lens' GetPartitions (Core.Maybe Types.Segment)
gpsSegment = Lens.field @"segment"
{-# DEPRECATED gpsSegment "Use generic-lens or generic-optics with 'segment' instead." #-}

instance Core.FromJSON GetPartitions where
  toJSON GetPartitions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            ("CatalogId" Core..=) Core.<$> catalogId,
            ("Expression" Core..=) Core.<$> expression,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("Segment" Core..=) Core.<$> segment
          ]
      )

instance Core.AWSRequest GetPartitions where
  type Rs GetPartitions = GetPartitionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetPartitions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPartitionsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Partitions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetPartitions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"partitions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetPartitionsResponse' smart constructor.
data GetPartitionsResponse = GetPartitionsResponse'
  { -- | A continuation token, if the returned list of partitions does not include the last one.
    nextToken :: Core.Maybe Types.Token,
    -- | A list of requested partitions.
    partitions :: Core.Maybe [Types.Partition],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetPartitionsResponse' value with any optional fields omitted.
mkGetPartitionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetPartitionsResponse
mkGetPartitionsResponse responseStatus =
  GetPartitionsResponse'
    { nextToken = Core.Nothing,
      partitions = Core.Nothing,
      responseStatus
    }

-- | A continuation token, if the returned list of partitions does not include the last one.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprgrsNextToken :: Lens.Lens' GetPartitionsResponse (Core.Maybe Types.Token)
gprgrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gprgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of requested partitions.
--
-- /Note:/ Consider using 'partitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprgrsPartitions :: Lens.Lens' GetPartitionsResponse (Core.Maybe [Types.Partition])
gprgrsPartitions = Lens.field @"partitions"
{-# DEPRECATED gprgrsPartitions "Use generic-lens or generic-optics with 'partitions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprgrsResponseStatus :: Lens.Lens' GetPartitionsResponse Core.Int
gprgrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gprgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
