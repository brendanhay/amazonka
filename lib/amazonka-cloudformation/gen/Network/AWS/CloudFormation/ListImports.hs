{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListImports (..),
    mkListImports,

    -- ** Request lenses
    liExportName,
    liNextToken,

    -- * Destructuring the response
    ListImportsResponse (..),
    mkListImportsResponse,

    -- ** Response lenses
    lirrsImports,
    lirrsNextToken,
    lirrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListImports' smart constructor.
data ListImports = ListImports'
  { -- | The name of the exported output value. AWS CloudFormation returns the stack names that are importing this value.
    exportName :: Types.ExportName,
    -- | A string (provided by the 'ListImports' response output) that identifies the next page of stacks that are importing the specified exported output value.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListImports' value with any optional fields omitted.
mkListImports ::
  -- | 'exportName'
  Types.ExportName ->
  ListImports
mkListImports exportName =
  ListImports' {exportName, nextToken = Core.Nothing}

-- | The name of the exported output value. AWS CloudFormation returns the stack names that are importing this value.
--
-- /Note:/ Consider using 'exportName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liExportName :: Lens.Lens' ListImports Types.ExportName
liExportName = Lens.field @"exportName"
{-# DEPRECATED liExportName "Use generic-lens or generic-optics with 'exportName' instead." #-}

-- | A string (provided by the 'ListImports' response output) that identifies the next page of stacks that are importing the specified exported output value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListImports (Core.Maybe Types.NextToken)
liNextToken = Lens.field @"nextToken"
{-# DEPRECATED liNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListImports where
  type Rs ListImports = ListImportsResponse
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
            ( Core.pure ("Action", "ListImports")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "ExportName" exportName)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListImportsResult"
      ( \s h x ->
          ListImportsResponse'
            Core.<$> (x Core..@? "Imports" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListImports where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"imports" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListImportsResponse' smart constructor.
data ListImportsResponse = ListImportsResponse'
  { -- | A list of stack names that are importing the specified exported output value.
    imports :: Core.Maybe [Types.StackName],
    -- | A string that identifies the next page of exports. If there is no additional page, this value is null.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListImportsResponse' value with any optional fields omitted.
mkListImportsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListImportsResponse
mkListImportsResponse responseStatus =
  ListImportsResponse'
    { imports = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of stack names that are importing the specified exported output value.
--
-- /Note:/ Consider using 'imports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsImports :: Lens.Lens' ListImportsResponse (Core.Maybe [Types.StackName])
lirrsImports = Lens.field @"imports"
{-# DEPRECATED lirrsImports "Use generic-lens or generic-optics with 'imports' instead." #-}

-- | A string that identifies the next page of exports. If there is no additional page, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsNextToken :: Lens.Lens' ListImportsResponse (Core.Maybe Types.NextToken)
lirrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lirrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsResponseStatus :: Lens.Lens' ListImportsResponse Core.Int
lirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
