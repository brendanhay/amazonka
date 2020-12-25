{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DeleteExpression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an @'Expression' @ from the search domain. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-expressions.html Configuring Expressions> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DeleteExpression
  ( -- * Creating a request
    DeleteExpression (..),
    mkDeleteExpression,

    -- ** Request lenses
    defDomainName,
    defExpressionName,

    -- * Destructuring the response
    DeleteExpressionResponse (..),
    mkDeleteExpressionResponse,

    -- ** Response lenses
    derfrsExpression,
    derfrsResponseStatus,
  )
where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DeleteExpression' @ operation. Specifies the name of the domain you want to update and the name of the expression you want to delete.
--
-- /See:/ 'mkDeleteExpression' smart constructor.
data DeleteExpression = DeleteExpression'
  { domainName :: Types.DomainName,
    -- | The name of the @'Expression' @ to delete.
    expressionName :: Types.ExpressionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteExpression' value with any optional fields omitted.
mkDeleteExpression ::
  -- | 'domainName'
  Types.DomainName ->
  -- | 'expressionName'
  Types.ExpressionName ->
  DeleteExpression
mkDeleteExpression domainName expressionName =
  DeleteExpression' {domainName, expressionName}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defDomainName :: Lens.Lens' DeleteExpression Types.DomainName
defDomainName = Lens.field @"domainName"
{-# DEPRECATED defDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The name of the @'Expression' @ to delete.
--
-- /Note:/ Consider using 'expressionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defExpressionName :: Lens.Lens' DeleteExpression Types.ExpressionName
defExpressionName = Lens.field @"expressionName"
{-# DEPRECATED defExpressionName "Use generic-lens or generic-optics with 'expressionName' instead." #-}

instance Core.AWSRequest DeleteExpression where
  type Rs DeleteExpression = DeleteExpressionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DeleteExpression")
                Core.<> (Core.pure ("Version", "2013-01-01"))
                Core.<> (Core.toQueryValue "DomainName" domainName)
                Core.<> (Core.toQueryValue "ExpressionName" expressionName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteExpressionResult"
      ( \s h x ->
          DeleteExpressionResponse'
            Core.<$> (x Core..@ "Expression") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a @'DeleteExpression' @ request. Specifies the expression being deleted.
--
-- /See:/ 'mkDeleteExpressionResponse' smart constructor.
data DeleteExpressionResponse = DeleteExpressionResponse'
  { -- | The status of the expression being deleted.
    expression :: Types.ExpressionStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteExpressionResponse' value with any optional fields omitted.
mkDeleteExpressionResponse ::
  -- | 'expression'
  Types.ExpressionStatus ->
  -- | 'responseStatus'
  Core.Int ->
  DeleteExpressionResponse
mkDeleteExpressionResponse expression responseStatus =
  DeleteExpressionResponse' {expression, responseStatus}

-- | The status of the expression being deleted.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derfrsExpression :: Lens.Lens' DeleteExpressionResponse Types.ExpressionStatus
derfrsExpression = Lens.field @"expression"
{-# DEPRECATED derfrsExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derfrsResponseStatus :: Lens.Lens' DeleteExpressionResponse Core.Int
derfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED derfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
