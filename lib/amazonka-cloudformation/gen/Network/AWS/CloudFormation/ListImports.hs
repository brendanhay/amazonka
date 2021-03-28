{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListImports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all stacks that are importing an exported output value. To modify or remove an exported output value, first use this action to see which stacks are using it. To see the exported output values in your account, see 'ListExports' . 
--
-- For more information about importing an exported output value, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/intrinsic-function-reference-importvalue.html @Fn::ImportValue@ > function. 
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListImports
    (
    -- * Creating a request
      ListImports (..)
    , mkListImports
    -- ** Request lenses
    , liExportName
    , liNextToken

    -- * Destructuring the response
    , ListImportsResponse (..)
    , mkListImportsResponse
    -- ** Response lenses
    , lirrsImports
    , lirrsNextToken
    , lirrsResponseStatus
    ) where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListImports' smart constructor.
data ListImports = ListImports'
  { exportName :: Types.ExportName
    -- ^ The name of the exported output value. AWS CloudFormation returns the stack names that are importing this value. 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A string (provided by the 'ListImports' response output) that identifies the next page of stacks that are importing the specified exported output value. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListImports' value with any optional fields omitted.
mkListImports
    :: Types.ExportName -- ^ 'exportName'
    -> ListImports
mkListImports exportName
  = ListImports'{exportName, nextToken = Core.Nothing}

-- | The name of the exported output value. AWS CloudFormation returns the stack names that are importing this value. 
--
-- /Note:/ Consider using 'exportName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liExportName :: Lens.Lens' ListImports Types.ExportName
liExportName = Lens.field @"exportName"
{-# INLINEABLE liExportName #-}
{-# DEPRECATED exportName "Use generic-lens or generic-optics with 'exportName' instead"  #-}

-- | A string (provided by the 'ListImports' response output) that identifies the next page of stacks that are importing the specified exported output value. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListImports (Core.Maybe Types.NextToken)
liNextToken = Lens.field @"nextToken"
{-# INLINEABLE liNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListImports where
        toQuery ListImports{..}
          = Core.toQueryPair "Action" ("ListImports" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-15" :: Core.Text)
              Core.<> Core.toQueryPair "ExportName" exportName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListImports where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListImports where
        type Rs ListImports = ListImportsResponse
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
          = Response.receiveXMLWrapper "ListImportsResult"
              (\ s h x ->
                 ListImportsResponse' Core.<$>
                   (x Core..@? "Imports" Core..<@> Core.parseXMLList "member")
                     Core.<*> x Core..@? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListImports where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"imports" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListImportsResponse' smart constructor.
data ListImportsResponse = ListImportsResponse'
  { imports :: Core.Maybe [Types.StackName]
    -- ^ A list of stack names that are importing the specified exported output value. 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A string that identifies the next page of exports. If there is no additional page, this value is null.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListImportsResponse' value with any optional fields omitted.
mkListImportsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListImportsResponse
mkListImportsResponse responseStatus
  = ListImportsResponse'{imports = Core.Nothing,
                         nextToken = Core.Nothing, responseStatus}

-- | A list of stack names that are importing the specified exported output value. 
--
-- /Note:/ Consider using 'imports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsImports :: Lens.Lens' ListImportsResponse (Core.Maybe [Types.StackName])
lirrsImports = Lens.field @"imports"
{-# INLINEABLE lirrsImports #-}
{-# DEPRECATED imports "Use generic-lens or generic-optics with 'imports' instead"  #-}

-- | A string that identifies the next page of exports. If there is no additional page, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsNextToken :: Lens.Lens' ListImportsResponse (Core.Maybe Types.NextToken)
lirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsResponseStatus :: Lens.Lens' ListImportsResponse Core.Int
lirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
