{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DefineExpression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures an @'Expression' @ for the search domain. Used to create new expressions and modify existing ones. If the expression exists, the new configuration replaces the old one. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-expressions.html Configuring Expressions> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DefineExpression
  ( -- * Creating a request
    DefineExpression (..),
    mkDefineExpression,

    -- ** Request lenses
    dDomainName,
    dExpression,

    -- * Destructuring the response
    DefineExpressionResponse (..),
    mkDefineExpressionResponse,

    -- ** Response lenses
    derrsExpression,
    derrsResponseStatus,
  )
where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DefineExpression' @ operation. Specifies the name of the domain you want to update and the expression you want to configure.
--
-- /See:/ 'mkDefineExpression' smart constructor.
data DefineExpression = DefineExpression'
  { domainName :: Types.DomainName,
    expression :: Types.Expression
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DefineExpression' value with any optional fields omitted.
mkDefineExpression ::
  -- | 'domainName'
  Types.DomainName ->
  -- | 'expression'
  Types.Expression ->
  DefineExpression
mkDefineExpression domainName expression =
  DefineExpression' {domainName, expression}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomainName :: Lens.Lens' DefineExpression Types.DomainName
dDomainName = Lens.field @"domainName"
{-# DEPRECATED dDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dExpression :: Lens.Lens' DefineExpression Types.Expression
dExpression = Lens.field @"expression"
{-# DEPRECATED dExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

instance Core.AWSRequest DefineExpression where
  type Rs DefineExpression = DefineExpressionResponse
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
            ( Core.pure ("Action", "DefineExpression")
                Core.<> (Core.pure ("Version", "2013-01-01"))
                Core.<> (Core.toQueryValue "DomainName" domainName)
                Core.<> (Core.toQueryValue "Expression" expression)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DefineExpressionResult"
      ( \s h x ->
          DefineExpressionResponse'
            Core.<$> (x Core..@ "Expression") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a @DefineExpression@ request. Contains the status of the newly-configured expression.
--
-- /See:/ 'mkDefineExpressionResponse' smart constructor.
data DefineExpressionResponse = DefineExpressionResponse'
  { expression :: Types.ExpressionStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DefineExpressionResponse' value with any optional fields omitted.
mkDefineExpressionResponse ::
  -- | 'expression'
  Types.ExpressionStatus ->
  -- | 'responseStatus'
  Core.Int ->
  DefineExpressionResponse
mkDefineExpressionResponse expression responseStatus =
  DefineExpressionResponse' {expression, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsExpression :: Lens.Lens' DefineExpressionResponse Types.ExpressionStatus
derrsExpression = Lens.field @"expression"
{-# DEPRECATED derrsExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsResponseStatus :: Lens.Lens' DefineExpressionResponse Core.Int
derrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED derrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
