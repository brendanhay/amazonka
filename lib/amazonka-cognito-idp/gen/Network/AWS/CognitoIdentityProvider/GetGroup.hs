{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.GetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a group.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.GetGroup
    (
    -- * Creating a request
      GetGroup (..)
    , mkGetGroup
    -- ** Request lenses
    , ggGroupName
    , ggUserPoolId

    -- * Destructuring the response
    , GetGroupResponse (..)
    , mkGetGroupResponse
    -- ** Response lenses
    , ggrrsGroup
    , ggrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetGroup' smart constructor.
data GetGroup = GetGroup'
  { groupName :: Types.GroupNameType
    -- ^ The name of the group.
  , userPoolId :: Types.UserPoolIdType
    -- ^ The user pool ID for the user pool.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroup' value with any optional fields omitted.
mkGetGroup
    :: Types.GroupNameType -- ^ 'groupName'
    -> Types.UserPoolIdType -- ^ 'userPoolId'
    -> GetGroup
mkGetGroup groupName userPoolId = GetGroup'{groupName, userPoolId}

-- | The name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggGroupName :: Lens.Lens' GetGroup Types.GroupNameType
ggGroupName = Lens.field @"groupName"
{-# INLINEABLE ggGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggUserPoolId :: Lens.Lens' GetGroup Types.UserPoolIdType
ggUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE ggUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

instance Core.ToQuery GetGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetGroup where
        toHeaders GetGroup{..}
          = Core.pure
              ("X-Amz-Target", "AWSCognitoIdentityProviderService.GetGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetGroup where
        toJSON GetGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GroupName" Core..= groupName),
                  Core.Just ("UserPoolId" Core..= userPoolId)])

instance Core.AWSRequest GetGroup where
        type Rs GetGroup = GetGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetGroupResponse' Core.<$>
                   (x Core..:? "Group") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
  { group :: Core.Maybe Types.GroupType
    -- ^ The group object for the group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetGroupResponse' value with any optional fields omitted.
mkGetGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetGroupResponse
mkGetGroupResponse responseStatus
  = GetGroupResponse'{group = Core.Nothing, responseStatus}

-- | The group object for the group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrsGroup :: Lens.Lens' GetGroupResponse (Core.Maybe Types.GroupType)
ggrrsGroup = Lens.field @"group"
{-# INLINEABLE ggrrsGroup #-}
{-# DEPRECATED group "Use generic-lens or generic-optics with 'group' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrsResponseStatus :: Lens.Lens' GetGroupResponse Core.Int
ggrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ggrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
