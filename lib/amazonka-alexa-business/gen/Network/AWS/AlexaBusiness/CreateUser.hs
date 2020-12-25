{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user.
module Network.AWS.AlexaBusiness.CreateUser
  ( -- * Creating a request
    CreateUser (..),
    mkCreateUser,

    -- ** Request lenses
    cuUserId,
    cuClientRequestToken,
    cuEmail,
    cuFirstName,
    cuLastName,
    cuTags,

    -- * Destructuring the response
    CreateUserResponse (..),
    mkCreateUserResponse,

    -- ** Response lenses
    currsUserArn,
    currsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The ARN for the user.
    userId :: Types.UserId,
    -- | A unique, user-specified identifier for this request that ensures idempotency.
    clientRequestToken :: Core.Maybe Types.ClientRequestToken,
    -- | The email address for the user.
    email :: Core.Maybe Types.Email,
    -- | The first name for the user.
    firstName :: Core.Maybe Types.FirstName,
    -- | The last name for the user.
    lastName :: Core.Maybe Types.User_LastName,
    -- | The tags for the user.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUser' value with any optional fields omitted.
mkCreateUser ::
  -- | 'userId'
  Types.UserId ->
  CreateUser
mkCreateUser userId =
  CreateUser'
    { userId,
      clientRequestToken = Core.Nothing,
      email = Core.Nothing,
      firstName = Core.Nothing,
      lastName = Core.Nothing,
      tags = Core.Nothing
    }

-- | The ARN for the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuUserId :: Lens.Lens' CreateUser Types.UserId
cuUserId = Lens.field @"userId"
{-# DEPRECATED cuUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | A unique, user-specified identifier for this request that ensures idempotency.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuClientRequestToken :: Lens.Lens' CreateUser (Core.Maybe Types.ClientRequestToken)
cuClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED cuClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The email address for the user.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuEmail :: Lens.Lens' CreateUser (Core.Maybe Types.Email)
cuEmail = Lens.field @"email"
{-# DEPRECATED cuEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The first name for the user.
--
-- /Note:/ Consider using 'firstName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuFirstName :: Lens.Lens' CreateUser (Core.Maybe Types.FirstName)
cuFirstName = Lens.field @"firstName"
{-# DEPRECATED cuFirstName "Use generic-lens or generic-optics with 'firstName' instead." #-}

-- | The last name for the user.
--
-- /Note:/ Consider using 'lastName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuLastName :: Lens.Lens' CreateUser (Core.Maybe Types.User_LastName)
cuLastName = Lens.field @"lastName"
{-# DEPRECATED cuLastName "Use generic-lens or generic-optics with 'lastName' instead." #-}

-- | The tags for the user.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuTags :: Lens.Lens' CreateUser (Core.Maybe [Types.Tag])
cuTags = Lens.field @"tags"
{-# DEPRECATED cuTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateUser where
  toJSON CreateUser {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserId" Core..= userId),
            ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
            ("Email" Core..=) Core.<$> email,
            ("FirstName" Core..=) Core.<$> firstName,
            ("LastName" Core..=) Core.<$> lastName,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateUser where
  type Rs CreateUser = CreateUserResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.CreateUser")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserResponse'
            Core.<$> (x Core..:? "UserArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The ARN of the newly created user in the response.
    userArn :: Core.Maybe Types.Arn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUserResponse' value with any optional fields omitted.
mkCreateUserResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateUserResponse
mkCreateUserResponse responseStatus =
  CreateUserResponse' {userArn = Core.Nothing, responseStatus}

-- | The ARN of the newly created user in the response.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currsUserArn :: Lens.Lens' CreateUserResponse (Core.Maybe Types.Arn)
currsUserArn = Lens.field @"userArn"
{-# DEPRECATED currsUserArn "Use generic-lens or generic-optics with 'userArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currsResponseStatus :: Lens.Lens' CreateUserResponse Core.Int
currsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED currsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
