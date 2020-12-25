{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.ListUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all ActiveMQ users.
module Network.AWS.MQ.ListUsers
  ( -- * Creating a request
    ListUsers (..),
    mkListUsers,

    -- ** Request lenses
    luBrokerId,
    luMaxResults,
    luNextToken,

    -- * Destructuring the response
    ListUsersResponse (..),
    mkListUsersResponse,

    -- ** Response lenses
    lurrsBrokerId,
    lurrsMaxResults,
    lurrsNextToken,
    lurrsUsers,
    lurrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListUsers' smart constructor.
data ListUsers = ListUsers'
  { -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Core.Text,
    -- | The maximum number of ActiveMQ users that can be returned per page (20 by default). This value must be an integer from 5 to 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListUsers' value with any optional fields omitted.
mkListUsers ::
  -- | 'brokerId'
  Core.Text ->
  ListUsers
mkListUsers brokerId =
  ListUsers'
    { brokerId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luBrokerId :: Lens.Lens' ListUsers Core.Text
luBrokerId = Lens.field @"brokerId"
{-# DEPRECATED luBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

-- | The maximum number of ActiveMQ users that can be returned per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luMaxResults :: Lens.Lens' ListUsers (Core.Maybe Core.Natural)
luMaxResults = Lens.field @"maxResults"
{-# DEPRECATED luMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luNextToken :: Lens.Lens' ListUsers (Core.Maybe Core.Text)
luNextToken = Lens.field @"nextToken"
{-# DEPRECATED luNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListUsers where
  type Rs ListUsers = ListUsersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/v1/brokers/" Core.<> (Core.toText brokerId) Core.<> ("/users")),
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUsersResponse'
            Core.<$> (x Core..:? "brokerId")
            Core.<*> (x Core..:? "maxResults")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "users")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
  { -- | Required. The unique ID that Amazon MQ generates for the broker.
    brokerId :: Core.Maybe Core.Text,
    -- | Required. The maximum number of ActiveMQ users that can be returned per page (20 by default). This value must be an integer from 5 to 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
    nextToken :: Core.Maybe Core.Text,
    -- | Required. The list of all ActiveMQ usernames for the specified broker.
    users :: Core.Maybe [Types.UserSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListUsersResponse' value with any optional fields omitted.
mkListUsersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListUsersResponse
mkListUsersResponse responseStatus =
  ListUsersResponse'
    { brokerId = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      users = Core.Nothing,
      responseStatus
    }

-- | Required. The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lurrsBrokerId :: Lens.Lens' ListUsersResponse (Core.Maybe Core.Text)
lurrsBrokerId = Lens.field @"brokerId"
{-# DEPRECATED lurrsBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

-- | Required. The maximum number of ActiveMQ users that can be returned per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lurrsMaxResults :: Lens.Lens' ListUsersResponse (Core.Maybe Core.Natural)
lurrsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lurrsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lurrsNextToken :: Lens.Lens' ListUsersResponse (Core.Maybe Core.Text)
lurrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lurrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Required. The list of all ActiveMQ usernames for the specified broker.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lurrsUsers :: Lens.Lens' ListUsersResponse (Core.Maybe [Types.UserSummary])
lurrsUsers = Lens.field @"users"
{-# DEPRECATED lurrsUsers "Use generic-lens or generic-optics with 'users' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lurrsResponseStatus :: Lens.Lens' ListUsersResponse Core.Int
lurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
