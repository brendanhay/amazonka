{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListExports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all exported output values in the account and Region in which you call this action. Use this action to see the exported output values that you can import into other stacks. To import values, use the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/intrinsic-function-reference-importvalue.html @Fn::ImportValue@ > function.
--
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-exports.html AWS CloudFormation Export Stack Output Values> .
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListExports
  ( -- * Creating a request
    ListExports (..),
    mkListExports,

    -- ** Request lenses
    leNextToken,

    -- * Destructuring the response
    ListExportsResponse (..),
    mkListExportsResponse,

    -- ** Response lenses
    lerrsExports,
    lerrsNextToken,
    lerrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListExports' smart constructor.
newtype ListExports = ListExports'
  { -- | A string (provided by the 'ListExports' response output) that identifies the next page of exported output values that you asked to retrieve.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListExports' value with any optional fields omitted.
mkListExports ::
  ListExports
mkListExports = ListExports' {nextToken = Core.Nothing}

-- | A string (provided by the 'ListExports' response output) that identifies the next page of exported output values that you asked to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leNextToken :: Lens.Lens' ListExports (Core.Maybe Types.NextToken)
leNextToken = Lens.field @"nextToken"
{-# DEPRECATED leNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListExports where
  type Rs ListExports = ListExportsResponse
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
            ( Core.pure ("Action", "ListExports")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListExportsResult"
      ( \s h x ->
          ListExportsResponse'
            Core.<$> (x Core..@? "Exports" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListExports where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"exports" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListExportsResponse' smart constructor.
data ListExportsResponse = ListExportsResponse'
  { -- | The output for the 'ListExports' action.
    exports :: Core.Maybe [Types.Export],
    -- | If the output exceeds 100 exported output values, a string that identifies the next page of exports. If there is no additional page, this value is null.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListExportsResponse' value with any optional fields omitted.
mkListExportsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListExportsResponse
mkListExportsResponse responseStatus =
  ListExportsResponse'
    { exports = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The output for the 'ListExports' action.
--
-- /Note:/ Consider using 'exports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsExports :: Lens.Lens' ListExportsResponse (Core.Maybe [Types.Export])
lerrsExports = Lens.field @"exports"
{-# DEPRECATED lerrsExports "Use generic-lens or generic-optics with 'exports' instead." #-}

-- | If the output exceeds 100 exported output values, a string that identifies the next page of exports. If there is no additional page, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsNextToken :: Lens.Lens' ListExportsResponse (Core.Maybe Types.NextToken)
lerrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lerrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsResponseStatus :: Lens.Lens' ListExportsResponse Core.Int
lerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
