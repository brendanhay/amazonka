{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DefineExpression (..)
    , mkDefineExpression
    -- ** Request lenses
    , dDomainName
    , dExpression

    -- * Destructuring the response
    , DefineExpressionResponse (..)
    , mkDefineExpressionResponse
    -- ** Response lenses
    , derrsExpression
    , derrsResponseStatus
    ) where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DefineExpression' @ operation. Specifies the name of the domain you want to update and the expression you want to configure.
--
-- /See:/ 'mkDefineExpression' smart constructor.
data DefineExpression = DefineExpression'
  { domainName :: Types.DomainName
  , expression :: Types.Expression
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DefineExpression' value with any optional fields omitted.
mkDefineExpression
    :: Types.DomainName -- ^ 'domainName'
    -> Types.Expression -- ^ 'expression'
    -> DefineExpression
mkDefineExpression domainName expression
  = DefineExpression'{domainName, expression}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomainName :: Lens.Lens' DefineExpression Types.DomainName
dDomainName = Lens.field @"domainName"
{-# INLINEABLE dDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dExpression :: Lens.Lens' DefineExpression Types.Expression
dExpression = Lens.field @"expression"
{-# INLINEABLE dExpression #-}
{-# DEPRECATED expression "Use generic-lens or generic-optics with 'expression' instead"  #-}

instance Core.ToQuery DefineExpression where
        toQuery DefineExpression{..}
          = Core.toQueryPair "Action" ("DefineExpression" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2013-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName
              Core.<> Core.toQueryPair "Expression" expression

instance Core.ToHeaders DefineExpression where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DefineExpression where
        type Rs DefineExpression = DefineExpressionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DefineExpressionResult"
              (\ s h x ->
                 DefineExpressionResponse' Core.<$>
                   (x Core..@ "Expression") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @DefineExpression@ request. Contains the status of the newly-configured expression.
--
-- /See:/ 'mkDefineExpressionResponse' smart constructor.
data DefineExpressionResponse = DefineExpressionResponse'
  { expression :: Types.ExpressionStatus
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DefineExpressionResponse' value with any optional fields omitted.
mkDefineExpressionResponse
    :: Types.ExpressionStatus -- ^ 'expression'
    -> Core.Int -- ^ 'responseStatus'
    -> DefineExpressionResponse
mkDefineExpressionResponse expression responseStatus
  = DefineExpressionResponse'{expression, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsExpression :: Lens.Lens' DefineExpressionResponse Types.ExpressionStatus
derrsExpression = Lens.field @"expression"
{-# INLINEABLE derrsExpression #-}
{-# DEPRECATED expression "Use generic-lens or generic-optics with 'expression' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsResponseStatus :: Lens.Lens' DefineExpressionResponse Core.Int
derrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE derrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
