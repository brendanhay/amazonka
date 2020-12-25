{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DescribeUserStackAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the UserStackAssociation objects. You must specify either or both of the following:
--
--
--     * The stack name
--
--
--     * The user name (email address of the user associated with the stack) and the authentication type for the user
--
--
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeUserStackAssociations
  ( -- * Creating a request
    DescribeUserStackAssociations (..),
    mkDescribeUserStackAssociations,

    -- ** Request lenses
    dusaAuthenticationType,
    dusaMaxResults,
    dusaNextToken,
    dusaStackName,
    dusaUserName,

    -- * Destructuring the response
    DescribeUserStackAssociationsResponse (..),
    mkDescribeUserStackAssociationsResponse,

    -- ** Response lenses
    dusarrsNextToken,
    dusarrsUserStackAssociations,
    dusarrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeUserStackAssociations' smart constructor.
data DescribeUserStackAssociations = DescribeUserStackAssociations'
  { -- | The authentication type for the user who is associated with the stack. You must specify USERPOOL.
    authenticationType :: Core.Maybe Types.AuthenticationType,
    -- | The maximum size of each page of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
    nextToken :: Core.Maybe Types.String,
    -- | The name of the stack that is associated with the user.
    stackName :: Core.Maybe Types.String,
    -- | The email address of the user who is associated with the stack.
    userName :: Core.Maybe Types.Username
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUserStackAssociations' value with any optional fields omitted.
mkDescribeUserStackAssociations ::
  DescribeUserStackAssociations
mkDescribeUserStackAssociations =
  DescribeUserStackAssociations'
    { authenticationType = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      stackName = Core.Nothing,
      userName = Core.Nothing
    }

-- | The authentication type for the user who is associated with the stack. You must specify USERPOOL.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dusaAuthenticationType :: Lens.Lens' DescribeUserStackAssociations (Core.Maybe Types.AuthenticationType)
dusaAuthenticationType = Lens.field @"authenticationType"
{-# DEPRECATED dusaAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

-- | The maximum size of each page of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dusaMaxResults :: Lens.Lens' DescribeUserStackAssociations (Core.Maybe Core.Natural)
dusaMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dusaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dusaNextToken :: Lens.Lens' DescribeUserStackAssociations (Core.Maybe Types.String)
dusaNextToken = Lens.field @"nextToken"
{-# DEPRECATED dusaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the stack that is associated with the user.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dusaStackName :: Lens.Lens' DescribeUserStackAssociations (Core.Maybe Types.String)
dusaStackName = Lens.field @"stackName"
{-# DEPRECATED dusaStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The email address of the user who is associated with the stack.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dusaUserName :: Lens.Lens' DescribeUserStackAssociations (Core.Maybe Types.Username)
dusaUserName = Lens.field @"userName"
{-# DEPRECATED dusaUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Core.FromJSON DescribeUserStackAssociations where
  toJSON DescribeUserStackAssociations {..} =
    Core.object
      ( Core.catMaybes
          [ ("AuthenticationType" Core..=) Core.<$> authenticationType,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("StackName" Core..=) Core.<$> stackName,
            ("UserName" Core..=) Core.<$> userName
          ]
      )

instance Core.AWSRequest DescribeUserStackAssociations where
  type
    Rs DescribeUserStackAssociations =
      DescribeUserStackAssociationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "PhotonAdminProxyService.DescribeUserStackAssociations"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserStackAssociationsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "UserStackAssociations")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeUserStackAssociations where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"userStackAssociations" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeUserStackAssociationsResponse' smart constructor.
data DescribeUserStackAssociationsResponse = DescribeUserStackAssociationsResponse'
  { -- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
    nextToken :: Core.Maybe Types.String,
    -- | The UserStackAssociation objects.
    userStackAssociations :: Core.Maybe (Core.NonEmpty Types.UserStackAssociation),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUserStackAssociationsResponse' value with any optional fields omitted.
mkDescribeUserStackAssociationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeUserStackAssociationsResponse
mkDescribeUserStackAssociationsResponse responseStatus =
  DescribeUserStackAssociationsResponse'
    { nextToken = Core.Nothing,
      userStackAssociations = Core.Nothing,
      responseStatus
    }

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dusarrsNextToken :: Lens.Lens' DescribeUserStackAssociationsResponse (Core.Maybe Types.String)
dusarrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dusarrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The UserStackAssociation objects.
--
-- /Note:/ Consider using 'userStackAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dusarrsUserStackAssociations :: Lens.Lens' DescribeUserStackAssociationsResponse (Core.Maybe (Core.NonEmpty Types.UserStackAssociation))
dusarrsUserStackAssociations = Lens.field @"userStackAssociations"
{-# DEPRECATED dusarrsUserStackAssociations "Use generic-lens or generic-optics with 'userStackAssociations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dusarrsResponseStatus :: Lens.Lens' DescribeUserStackAssociationsResponse Core.Int
dusarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dusarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
