{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeExpressions (..)
    , mkDescribeExpressions
    -- ** Request lenses
    , deDomainName
    , deDeployed
    , deExpressionNames

    -- * Destructuring the response
    , DescribeExpressionsResponse (..)
    , mkDescribeExpressionsResponse
    -- ** Response lenses
    , drsExpressions
    , drsResponseStatus
    ) where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DescribeDomains' @ operation. Specifies the name of the domain you want to describe. To restrict the response to particular expressions, specify the names of the expressions you want to describe. To show the active configuration and exclude any pending changes, set the @Deployed@ option to @true@ .
--
-- /See:/ 'mkDescribeExpressions' smart constructor.
data DescribeExpressions = DescribeExpressions'
  { domainName :: Types.DomainName
    -- ^ The name of the domain you want to describe.
  , deployed :: Core.Maybe Core.Bool
    -- ^ Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
  , expressionNames :: Core.Maybe [Types.StandardName]
    -- ^ Limits the @'DescribeExpressions' @ response to the specified expressions. If not specified, all expressions are shown.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeExpressions' value with any optional fields omitted.
mkDescribeExpressions
    :: Types.DomainName -- ^ 'domainName'
    -> DescribeExpressions
mkDescribeExpressions domainName
  = DescribeExpressions'{domainName, deployed = Core.Nothing,
                         expressionNames = Core.Nothing}

-- | The name of the domain you want to describe.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deDomainName :: Lens.Lens' DescribeExpressions Types.DomainName
deDomainName = Lens.field @"domainName"
{-# INLINEABLE deDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
--
-- /Note:/ Consider using 'deployed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deDeployed :: Lens.Lens' DescribeExpressions (Core.Maybe Core.Bool)
deDeployed = Lens.field @"deployed"
{-# INLINEABLE deDeployed #-}
{-# DEPRECATED deployed "Use generic-lens or generic-optics with 'deployed' instead"  #-}

-- | Limits the @'DescribeExpressions' @ response to the specified expressions. If not specified, all expressions are shown.
--
-- /Note:/ Consider using 'expressionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deExpressionNames :: Lens.Lens' DescribeExpressions (Core.Maybe [Types.StandardName])
deExpressionNames = Lens.field @"expressionNames"
{-# INLINEABLE deExpressionNames #-}
{-# DEPRECATED expressionNames "Use generic-lens or generic-optics with 'expressionNames' instead"  #-}

instance Core.ToQuery DescribeExpressions where
        toQuery DescribeExpressions{..}
          = Core.toQueryPair "Action" ("DescribeExpressions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2013-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Deployed") deployed
              Core.<>
              Core.toQueryPair "ExpressionNames"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   expressionNames)

instance Core.ToHeaders DescribeExpressions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeExpressions where
        type Rs DescribeExpressions = DescribeExpressionsResponse
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
          = Response.receiveXMLWrapper "DescribeExpressionsResult"
              (\ s h x ->
                 DescribeExpressionsResponse' Core.<$>
                   (x Core..@ "Expressions" Core..@! Core.mempty Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @DescribeExpressions@ request. Contains the expressions configured for the domain specified in the request.
--
-- /See:/ 'mkDescribeExpressionsResponse' smart constructor.
data DescribeExpressionsResponse = DescribeExpressionsResponse'
  { expressions :: [Types.ExpressionStatus]
    -- ^ The expressions configured for the domain.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeExpressionsResponse' value with any optional fields omitted.
mkDescribeExpressionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeExpressionsResponse
mkDescribeExpressionsResponse responseStatus
  = DescribeExpressionsResponse'{expressions = Core.mempty,
                                 responseStatus}

-- | The expressions configured for the domain.
--
-- /Note:/ Consider using 'expressions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsExpressions :: Lens.Lens' DescribeExpressionsResponse [Types.ExpressionStatus]
drsExpressions = Lens.field @"expressions"
{-# INLINEABLE drsExpressions #-}
{-# DEPRECATED expressions "Use generic-lens or generic-optics with 'expressions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeExpressionsResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
