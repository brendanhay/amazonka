{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.GetCallerIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about the IAM user or role whose credentials are used to call the operation.
module Network.AWS.STS.GetCallerIdentity
    (
    -- * Creating a request
      GetCallerIdentity (..)
    , mkGetCallerIdentity

    -- * Destructuring the response
    , GetCallerIdentityResponse (..)
    , mkGetCallerIdentityResponse
    -- ** Response lenses
    , gcirrsAccount
    , gcirrsArn
    , gcirrsUserId
    , gcirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.STS.Types as Types

-- | /See:/ 'mkGetCallerIdentity' smart constructor.
data GetCallerIdentity = GetCallerIdentity'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCallerIdentity' value with any optional fields omitted.
mkGetCallerIdentity
    :: GetCallerIdentity
mkGetCallerIdentity = GetCallerIdentity'

instance Core.ToQuery GetCallerIdentity where
        toQuery GetCallerIdentity{..}
          = Core.toQueryPair "Action" ("GetCallerIdentity" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-06-15" :: Core.Text)

instance Core.ToHeaders GetCallerIdentity where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetCallerIdentity where
        type Rs GetCallerIdentity = GetCallerIdentityResponse
        toRequest x@_
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
          = Response.receiveXMLWrapper "GetCallerIdentityResult"
              (\ s h x ->
                 GetCallerIdentityResponse' Core.<$>
                   (x Core..@? "Account") Core.<*> x Core..@? "Arn" Core.<*>
                     x Core..@? "UserId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'GetCallerIdentity' request, including information about the entity making the request.
--
-- /See:/ 'mkGetCallerIdentityResponse' smart constructor.
data GetCallerIdentityResponse = GetCallerIdentityResponse'
  { account :: Core.Maybe Types.Account
    -- ^ The AWS account ID number of the account that owns or contains the calling entity.
  , arn :: Core.Maybe Types.ArnType
    -- ^ The AWS ARN associated with the calling entity.
  , userId :: Core.Maybe Types.UserIdType
    -- ^ The unique identifier of the calling entity. The exact value depends on the type of entity that is making the call. The values returned are those listed in the __aws:userid__ column in the <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_variables.html#principaltable Principal table> found on the __Policy Variables__ reference page in the /IAM User Guide/ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCallerIdentityResponse' value with any optional fields omitted.
mkGetCallerIdentityResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCallerIdentityResponse
mkGetCallerIdentityResponse responseStatus
  = GetCallerIdentityResponse'{account = Core.Nothing,
                               arn = Core.Nothing, userId = Core.Nothing, responseStatus}

-- | The AWS account ID number of the account that owns or contains the calling entity.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsAccount :: Lens.Lens' GetCallerIdentityResponse (Core.Maybe Types.Account)
gcirrsAccount = Lens.field @"account"
{-# INLINEABLE gcirrsAccount #-}
{-# DEPRECATED account "Use generic-lens or generic-optics with 'account' instead"  #-}

-- | The AWS ARN associated with the calling entity.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsArn :: Lens.Lens' GetCallerIdentityResponse (Core.Maybe Types.ArnType)
gcirrsArn = Lens.field @"arn"
{-# INLINEABLE gcirrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The unique identifier of the calling entity. The exact value depends on the type of entity that is making the call. The values returned are those listed in the __aws:userid__ column in the <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_variables.html#principaltable Principal table> found on the __Policy Variables__ reference page in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsUserId :: Lens.Lens' GetCallerIdentityResponse (Core.Maybe Types.UserIdType)
gcirrsUserId = Lens.field @"userId"
{-# INLINEABLE gcirrsUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsResponseStatus :: Lens.Lens' GetCallerIdentityResponse Core.Int
gcirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
