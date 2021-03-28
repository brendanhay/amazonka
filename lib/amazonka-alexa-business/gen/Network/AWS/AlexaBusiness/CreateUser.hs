{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateUser (..)
    , mkCreateUser
    -- ** Request lenses
    , cuUserId
    , cuClientRequestToken
    , cuEmail
    , cuFirstName
    , cuLastName
    , cuTags

    -- * Destructuring the response
    , CreateUserResponse (..)
    , mkCreateUserResponse
    -- ** Response lenses
    , currsUserArn
    , currsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateUser' smart constructor.
data CreateUser = CreateUser'
  { userId :: Types.UserId
    -- ^ The ARN for the user.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ A unique, user-specified identifier for this request that ensures idempotency. 
  , email :: Core.Maybe Types.Email
    -- ^ The email address for the user.
  , firstName :: Core.Maybe Types.FirstName
    -- ^ The first name for the user.
  , lastName :: Core.Maybe Types.User_LastName
    -- ^ The last name for the user.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags for the user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUser' value with any optional fields omitted.
mkCreateUser
    :: Types.UserId -- ^ 'userId'
    -> CreateUser
mkCreateUser userId
  = CreateUser'{userId, clientRequestToken = Core.Nothing,
                email = Core.Nothing, firstName = Core.Nothing,
                lastName = Core.Nothing, tags = Core.Nothing}

-- | The ARN for the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuUserId :: Lens.Lens' CreateUser Types.UserId
cuUserId = Lens.field @"userId"
{-# INLINEABLE cuUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

-- | A unique, user-specified identifier for this request that ensures idempotency. 
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuClientRequestToken :: Lens.Lens' CreateUser (Core.Maybe Types.ClientRequestToken)
cuClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE cuClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The email address for the user.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuEmail :: Lens.Lens' CreateUser (Core.Maybe Types.Email)
cuEmail = Lens.field @"email"
{-# INLINEABLE cuEmail #-}
{-# DEPRECATED email "Use generic-lens or generic-optics with 'email' instead"  #-}

-- | The first name for the user.
--
-- /Note:/ Consider using 'firstName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuFirstName :: Lens.Lens' CreateUser (Core.Maybe Types.FirstName)
cuFirstName = Lens.field @"firstName"
{-# INLINEABLE cuFirstName #-}
{-# DEPRECATED firstName "Use generic-lens or generic-optics with 'firstName' instead"  #-}

-- | The last name for the user.
--
-- /Note:/ Consider using 'lastName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuLastName :: Lens.Lens' CreateUser (Core.Maybe Types.User_LastName)
cuLastName = Lens.field @"lastName"
{-# INLINEABLE cuLastName #-}
{-# DEPRECATED lastName "Use generic-lens or generic-optics with 'lastName' instead"  #-}

-- | The tags for the user.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuTags :: Lens.Lens' CreateUser (Core.Maybe [Types.Tag])
cuTags = Lens.field @"tags"
{-# INLINEABLE cuTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateUser where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateUser where
        toHeaders CreateUser{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.CreateUser") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateUser where
        toJSON CreateUser{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserId" Core..= userId),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("Email" Core..=) Core.<$> email,
                  ("FirstName" Core..=) Core.<$> firstName,
                  ("LastName" Core..=) Core.<$> lastName,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateUser where
        type Rs CreateUser = CreateUserResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateUserResponse' Core.<$>
                   (x Core..:? "UserArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { userArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the newly created user in the response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUserResponse' value with any optional fields omitted.
mkCreateUserResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateUserResponse
mkCreateUserResponse responseStatus
  = CreateUserResponse'{userArn = Core.Nothing, responseStatus}

-- | The ARN of the newly created user in the response.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currsUserArn :: Lens.Lens' CreateUserResponse (Core.Maybe Types.Arn)
currsUserArn = Lens.field @"userArn"
{-# INLINEABLE currsUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currsResponseStatus :: Lens.Lens' CreateUserResponse Core.Int
currsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE currsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
