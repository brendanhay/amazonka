{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.BatchExecuteStatement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation allows you to perform batch reads and writes on data stored in DynamoDB, using PartiQL.
module Network.AWS.DynamoDB.BatchExecuteStatement
  ( -- * Creating a request
    BatchExecuteStatement (..),
    mkBatchExecuteStatement,

    -- ** Request lenses
    besStatements,

    -- * Destructuring the response
    BatchExecuteStatementResponse (..),
    mkBatchExecuteStatementResponse,

    -- ** Response lenses
    besrrsResponses,
    besrrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchExecuteStatement' smart constructor.
newtype BatchExecuteStatement = BatchExecuteStatement'
  { -- | The list of PartiQL statements representing the batch to run.
    statements :: Core.NonEmpty Types.BatchStatementRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchExecuteStatement' value with any optional fields omitted.
mkBatchExecuteStatement ::
  -- | 'statements'
  Core.NonEmpty Types.BatchStatementRequest ->
  BatchExecuteStatement
mkBatchExecuteStatement statements =
  BatchExecuteStatement' {statements}

-- | The list of PartiQL statements representing the batch to run.
--
-- /Note:/ Consider using 'statements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
besStatements :: Lens.Lens' BatchExecuteStatement (Core.NonEmpty Types.BatchStatementRequest)
besStatements = Lens.field @"statements"
{-# DEPRECATED besStatements "Use generic-lens or generic-optics with 'statements' instead." #-}

instance Core.FromJSON BatchExecuteStatement where
  toJSON BatchExecuteStatement {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Statements" Core..= statements)])

instance Core.AWSRequest BatchExecuteStatement where
  type Rs BatchExecuteStatement = BatchExecuteStatementResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DynamoDB_20120810.BatchExecuteStatement")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchExecuteStatementResponse'
            Core.<$> (x Core..:? "Responses") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchExecuteStatementResponse' smart constructor.
data BatchExecuteStatementResponse = BatchExecuteStatementResponse'
  { -- | The response to each PartiQL statement in the batch.
    responses :: Core.Maybe [Types.BatchStatementResponse],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchExecuteStatementResponse' value with any optional fields omitted.
mkBatchExecuteStatementResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchExecuteStatementResponse
mkBatchExecuteStatementResponse responseStatus =
  BatchExecuteStatementResponse'
    { responses = Core.Nothing,
      responseStatus
    }

-- | The response to each PartiQL statement in the batch.
--
-- /Note:/ Consider using 'responses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
besrrsResponses :: Lens.Lens' BatchExecuteStatementResponse (Core.Maybe [Types.BatchStatementResponse])
besrrsResponses = Lens.field @"responses"
{-# DEPRECATED besrrsResponses "Use generic-lens or generic-optics with 'responses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
besrrsResponseStatus :: Lens.Lens' BatchExecuteStatementResponse Core.Int
besrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED besrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
