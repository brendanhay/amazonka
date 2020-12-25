{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.StartQueryExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs the SQL query statements contained in the @Query@ . Requires you to have access to the workgroup in which the query ran. Running queries against an external catalog requires 'GetDataCatalog' permission to the catalog. For code samples using the AWS SDK for Java, see <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples> in the /Amazon Athena User Guide/ .
module Network.AWS.Athena.StartQueryExecution
  ( -- * Creating a request
    StartQueryExecution (..),
    mkStartQueryExecution,

    -- ** Request lenses
    sqeQueryString,
    sqeClientRequestToken,
    sqeQueryExecutionContext,
    sqeResultConfiguration,
    sqeWorkGroup,

    -- * Destructuring the response
    StartQueryExecutionResponse (..),
    mkStartQueryExecutionResponse,

    -- ** Response lenses
    sqerrsQueryExecutionId,
    sqerrsResponseStatus,
  )
where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartQueryExecution' smart constructor.
data StartQueryExecution = StartQueryExecution'
  { -- | The SQL query statements to be executed.
    queryString :: Types.QueryString,
    -- | A unique case-sensitive string used to ensure the request to create the query is idempotent (executes only once). If another @StartQueryExecution@ request is received, the same response is returned and another query is not created. If a parameter has changed, for example, the @QueryString@ , an error is returned.
    --
    -- /Important:/ This token is listed as not required because AWS SDKs (for example the AWS SDK for Java) auto-generate the token for users. If you are not using the AWS SDK or the AWS CLI, you must provide this token or the action will fail.
    clientRequestToken :: Core.Maybe Types.IdempotencyToken,
    -- | The database within which the query executes.
    queryExecutionContext :: Core.Maybe Types.QueryExecutionContext,
    -- | Specifies information about where and how to save the results of the query execution. If the query runs in a workgroup, then workgroup's settings may override query settings. This affects the query results location. The workgroup settings override is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
    resultConfiguration :: Core.Maybe Types.ResultConfiguration,
    -- | The name of the workgroup in which the query is being started.
    workGroup :: Core.Maybe Types.WorkGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartQueryExecution' value with any optional fields omitted.
mkStartQueryExecution ::
  -- | 'queryString'
  Types.QueryString ->
  StartQueryExecution
mkStartQueryExecution queryString =
  StartQueryExecution'
    { queryString,
      clientRequestToken = Core.Nothing,
      queryExecutionContext = Core.Nothing,
      resultConfiguration = Core.Nothing,
      workGroup = Core.Nothing
    }

-- | The SQL query statements to be executed.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqeQueryString :: Lens.Lens' StartQueryExecution Types.QueryString
sqeQueryString = Lens.field @"queryString"
{-# DEPRECATED sqeQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

-- | A unique case-sensitive string used to ensure the request to create the query is idempotent (executes only once). If another @StartQueryExecution@ request is received, the same response is returned and another query is not created. If a parameter has changed, for example, the @QueryString@ , an error is returned.
--
-- /Important:/ This token is listed as not required because AWS SDKs (for example the AWS SDK for Java) auto-generate the token for users. If you are not using the AWS SDK or the AWS CLI, you must provide this token or the action will fail.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqeClientRequestToken :: Lens.Lens' StartQueryExecution (Core.Maybe Types.IdempotencyToken)
sqeClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED sqeClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The database within which the query executes.
--
-- /Note:/ Consider using 'queryExecutionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqeQueryExecutionContext :: Lens.Lens' StartQueryExecution (Core.Maybe Types.QueryExecutionContext)
sqeQueryExecutionContext = Lens.field @"queryExecutionContext"
{-# DEPRECATED sqeQueryExecutionContext "Use generic-lens or generic-optics with 'queryExecutionContext' instead." #-}

-- | Specifies information about where and how to save the results of the query execution. If the query runs in a workgroup, then workgroup's settings may override query settings. This affects the query results location. The workgroup settings override is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
--
-- /Note:/ Consider using 'resultConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqeResultConfiguration :: Lens.Lens' StartQueryExecution (Core.Maybe Types.ResultConfiguration)
sqeResultConfiguration = Lens.field @"resultConfiguration"
{-# DEPRECATED sqeResultConfiguration "Use generic-lens or generic-optics with 'resultConfiguration' instead." #-}

-- | The name of the workgroup in which the query is being started.
--
-- /Note:/ Consider using 'workGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqeWorkGroup :: Lens.Lens' StartQueryExecution (Core.Maybe Types.WorkGroupName)
sqeWorkGroup = Lens.field @"workGroup"
{-# DEPRECATED sqeWorkGroup "Use generic-lens or generic-optics with 'workGroup' instead." #-}

instance Core.FromJSON StartQueryExecution where
  toJSON StartQueryExecution {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("QueryString" Core..= queryString),
            ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
            ("QueryExecutionContext" Core..=) Core.<$> queryExecutionContext,
            ("ResultConfiguration" Core..=) Core.<$> resultConfiguration,
            ("WorkGroup" Core..=) Core.<$> workGroup
          ]
      )

instance Core.AWSRequest StartQueryExecution where
  type Rs StartQueryExecution = StartQueryExecutionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonAthena.StartQueryExecution")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartQueryExecutionResponse'
            Core.<$> (x Core..:? "QueryExecutionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartQueryExecutionResponse' smart constructor.
data StartQueryExecutionResponse = StartQueryExecutionResponse'
  { -- | The unique ID of the query that ran as a result of this request.
    queryExecutionId :: Core.Maybe Types.QueryExecutionId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartQueryExecutionResponse' value with any optional fields omitted.
mkStartQueryExecutionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartQueryExecutionResponse
mkStartQueryExecutionResponse responseStatus =
  StartQueryExecutionResponse'
    { queryExecutionId = Core.Nothing,
      responseStatus
    }

-- | The unique ID of the query that ran as a result of this request.
--
-- /Note:/ Consider using 'queryExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqerrsQueryExecutionId :: Lens.Lens' StartQueryExecutionResponse (Core.Maybe Types.QueryExecutionId)
sqerrsQueryExecutionId = Lens.field @"queryExecutionId"
{-# DEPRECATED sqerrsQueryExecutionId "Use generic-lens or generic-optics with 'queryExecutionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqerrsResponseStatus :: Lens.Lens' StartQueryExecutionResponse Core.Int
sqerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sqerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
