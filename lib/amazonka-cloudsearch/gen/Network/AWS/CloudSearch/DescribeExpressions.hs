{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeExpressions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the expressions configured for the search domain. Can be limited to specific expressions by name. By default, shows all expressions and includes any pending changes to the configuration. Set the @Deployed@ option to @true@ to show the active configuration and exclude pending changes. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-expressions.html Configuring Expressions> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DescribeExpressions
  ( -- * Creating a request
    DescribeExpressions (..),
    mkDescribeExpressions,

    -- ** Request lenses
    deDomainName,
    deDeployed,
    deExpressionNames,

    -- * Destructuring the response
    DescribeExpressionsResponse (..),
    mkDescribeExpressionsResponse,

    -- ** Response lenses
    drsExpressions,
    drsResponseStatus,
  )
where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DescribeDomains' @ operation. Specifies the name of the domain you want to describe. To restrict the response to particular expressions, specify the names of the expressions you want to describe. To show the active configuration and exclude any pending changes, set the @Deployed@ option to @true@ .
--
-- /See:/ 'mkDescribeExpressions' smart constructor.
data DescribeExpressions = DescribeExpressions'
  { -- | The name of the domain you want to describe.
    domainName :: Types.DomainName,
    -- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
    deployed :: Core.Maybe Core.Bool,
    -- | Limits the @'DescribeExpressions' @ response to the specified expressions. If not specified, all expressions are shown.
    expressionNames :: Core.Maybe [Types.StandardName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeExpressions' value with any optional fields omitted.
mkDescribeExpressions ::
  -- | 'domainName'
  Types.DomainName ->
  DescribeExpressions
mkDescribeExpressions domainName =
  DescribeExpressions'
    { domainName,
      deployed = Core.Nothing,
      expressionNames = Core.Nothing
    }

-- | The name of the domain you want to describe.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deDomainName :: Lens.Lens' DescribeExpressions Types.DomainName
deDomainName = Lens.field @"domainName"
{-# DEPRECATED deDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
--
-- /Note:/ Consider using 'deployed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deDeployed :: Lens.Lens' DescribeExpressions (Core.Maybe Core.Bool)
deDeployed = Lens.field @"deployed"
{-# DEPRECATED deDeployed "Use generic-lens or generic-optics with 'deployed' instead." #-}

-- | Limits the @'DescribeExpressions' @ response to the specified expressions. If not specified, all expressions are shown.
--
-- /Note:/ Consider using 'expressionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deExpressionNames :: Lens.Lens' DescribeExpressions (Core.Maybe [Types.StandardName])
deExpressionNames = Lens.field @"expressionNames"
{-# DEPRECATED deExpressionNames "Use generic-lens or generic-optics with 'expressionNames' instead." #-}

instance Core.AWSRequest DescribeExpressions where
  type Rs DescribeExpressions = DescribeExpressionsResponse
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
            ( Core.pure ("Action", "DescribeExpressions")
                Core.<> (Core.pure ("Version", "2013-01-01"))
                Core.<> (Core.toQueryValue "DomainName" domainName)
                Core.<> (Core.toQueryValue "Deployed" Core.<$> deployed)
                Core.<> ( Core.toQueryValue
                            "ExpressionNames"
                            (Core.toQueryList "member" Core.<$> expressionNames)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeExpressionsResult"
      ( \s h x ->
          DescribeExpressionsResponse'
            Core.<$> ( x Core..@? "Expressions" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a @DescribeExpressions@ request. Contains the expressions configured for the domain specified in the request.
--
-- /See:/ 'mkDescribeExpressionsResponse' smart constructor.
data DescribeExpressionsResponse = DescribeExpressionsResponse'
  { -- | The expressions configured for the domain.
    expressions :: [Types.ExpressionStatus],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeExpressionsResponse' value with any optional fields omitted.
mkDescribeExpressionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeExpressionsResponse
mkDescribeExpressionsResponse responseStatus =
  DescribeExpressionsResponse'
    { expressions = Core.mempty,
      responseStatus
    }

-- | The expressions configured for the domain.
--
-- /Note:/ Consider using 'expressions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsExpressions :: Lens.Lens' DescribeExpressionsResponse [Types.ExpressionStatus]
drsExpressions = Lens.field @"expressions"
{-# DEPRECATED drsExpressions "Use generic-lens or generic-optics with 'expressions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeExpressionsResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
