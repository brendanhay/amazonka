{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dtfTableName,

    -- * Destructuring the response
    DescribeTableResponse (..),
    mkDescribeTableResponse,

    -- ** Response lenses
    drsTable,
    drsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeTable@ operation.
--
-- /See:/ 'mkDescribeTable' smart constructor.
newtype DescribeTable = DescribeTable'
  { -- | The name of the table to describe.
    tableName :: Types.TableName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTable' value with any optional fields omitted.
mkDescribeTable ::
  -- | 'tableName'
  Types.TableName ->
  DescribeTable
mkDescribeTable tableName = DescribeTable' {tableName}

-- | The name of the table to describe.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfTableName :: Lens.Lens' DescribeTable Types.TableName
dtfTableName = Lens.field @"tableName"
{-# DEPRECATED dtfTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Core.FromJSON DescribeTable where
  toJSON DescribeTable {..} =
    Core.object
      (Core.catMaybes [Core.Just ("TableName" Core..= tableName)])

instance Core.AWSRequest DescribeTable where
  type Rs DescribeTable = DescribeTableResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DynamoDB_20120810.DescribeTable")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTableResponse'
            Core.<$> (x Core..:? "Table") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @DescribeTable@ operation.
--
-- /See:/ 'mkDescribeTableResponse' smart constructor.
data DescribeTableResponse = DescribeTableResponse'
  { -- | The properties of the table.
    table :: Core.Maybe Types.TableDescription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeTableResponse' value with any optional fields omitted.
mkDescribeTableResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTableResponse
mkDescribeTableResponse responseStatus =
  DescribeTableResponse' {table = Core.Nothing, responseStatus}

-- | The properties of the table.
--
-- /Note:/ Consider using 'table' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsTable :: Lens.Lens' DescribeTableResponse (Core.Maybe Types.TableDescription)
drsTable = Lens.field @"table"
{-# DEPRECATED drsTable "Use generic-lens or generic-optics with 'table' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeTableResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
