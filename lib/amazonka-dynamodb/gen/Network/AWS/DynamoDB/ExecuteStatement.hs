{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.ExecuteStatement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation allows you to perform reads and singleton writes on data stored in DynamoDB, using PartiQL.
module Network.AWS.DynamoDB.ExecuteStatement
  ( -- * Creating a request
    ExecuteStatement (..),
    mkExecuteStatement,

    -- ** Request lenses
    esStatement,
    esConsistentRead,
    esNextToken,
    esParameters,

    -- * Destructuring the response
    ExecuteStatementResponse (..),
    mkExecuteStatementResponse,

    -- ** Response lenses
    esrrsItems,
    esrrsNextToken,
    esrrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkExecuteStatement' smart constructor.
data ExecuteStatement = ExecuteStatement'
  { -- | The PartiQL statement representing the operation to run.
    statement :: Types.Statement,
    -- | The consistency of a read operation. If set to @true@ , then a strongly consistent read is used; otherwise, an eventually consistent read is used.
    consistentRead :: Core.Maybe Core.Bool,
    -- | Set this value to get remaining results, if @NextToken@ was returned in the statement response.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The parameters for the PartiQL statement, if any.
    parameters :: Core.Maybe (Core.NonEmpty Types.AttributeValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExecuteStatement' value with any optional fields omitted.
mkExecuteStatement ::
  -- | 'statement'
  Types.Statement ->
  ExecuteStatement
mkExecuteStatement statement =
  ExecuteStatement'
    { statement,
      consistentRead = Core.Nothing,
      nextToken = Core.Nothing,
      parameters = Core.Nothing
    }

-- | The PartiQL statement representing the operation to run.
--
-- /Note:/ Consider using 'statement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esStatement :: Lens.Lens' ExecuteStatement Types.Statement
esStatement = Lens.field @"statement"
{-# DEPRECATED esStatement "Use generic-lens or generic-optics with 'statement' instead." #-}

-- | The consistency of a read operation. If set to @true@ , then a strongly consistent read is used; otherwise, an eventually consistent read is used.
--
-- /Note:/ Consider using 'consistentRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esConsistentRead :: Lens.Lens' ExecuteStatement (Core.Maybe Core.Bool)
esConsistentRead = Lens.field @"consistentRead"
{-# DEPRECATED esConsistentRead "Use generic-lens or generic-optics with 'consistentRead' instead." #-}

-- | Set this value to get remaining results, if @NextToken@ was returned in the statement response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esNextToken :: Lens.Lens' ExecuteStatement (Core.Maybe Types.NextToken)
esNextToken = Lens.field @"nextToken"
{-# DEPRECATED esNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The parameters for the PartiQL statement, if any.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esParameters :: Lens.Lens' ExecuteStatement (Core.Maybe (Core.NonEmpty Types.AttributeValue))
esParameters = Lens.field @"parameters"
{-# DEPRECATED esParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Core.FromJSON ExecuteStatement where
  toJSON ExecuteStatement {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Statement" Core..= statement),
            ("ConsistentRead" Core..=) Core.<$> consistentRead,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("Parameters" Core..=) Core.<$> parameters
          ]
      )

instance Core.AWSRequest ExecuteStatement where
  type Rs ExecuteStatement = ExecuteStatementResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DynamoDB_20120810.ExecuteStatement")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ExecuteStatementResponse'
            Core.<$> (x Core..:? "Items")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkExecuteStatementResponse' smart constructor.
data ExecuteStatementResponse = ExecuteStatementResponse'
  { -- | If a read operation was used, this property will contain the result of the reade operation; a map of attribute names and their values. For the write operations this value will be empty.
    items :: Core.Maybe [Core.HashMap Types.AttributeName Types.AttributeValue],
    -- | If the response of a read request exceeds the response payload limit DynamoDB will set this value in the response. If set, you can use that this value in the subsequent request to get the remaining results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExecuteStatementResponse' value with any optional fields omitted.
mkExecuteStatementResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ExecuteStatementResponse
mkExecuteStatementResponse responseStatus =
  ExecuteStatementResponse'
    { items = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | If a read operation was used, this property will contain the result of the reade operation; a map of attribute names and their values. For the write operations this value will be empty.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrrsItems :: Lens.Lens' ExecuteStatementResponse (Core.Maybe [Core.HashMap Types.AttributeName Types.AttributeValue])
esrrsItems = Lens.field @"items"
{-# DEPRECATED esrrsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | If the response of a read request exceeds the response payload limit DynamoDB will set this value in the response. If set, you can use that this value in the subsequent request to get the remaining results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrrsNextToken :: Lens.Lens' ExecuteStatementResponse (Core.Maybe Types.NextToken)
esrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED esrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrrsResponseStatus :: Lens.Lens' ExecuteStatementResponse Core.Int
esrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED esrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
