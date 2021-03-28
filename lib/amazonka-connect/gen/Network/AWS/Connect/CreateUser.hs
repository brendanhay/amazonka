{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.CreateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user account for the specified Amazon Connect instance.
--
-- For information about how to create user accounts using the Amazon Connect console, see <https://docs.aws.amazon.com/connect/latest/adminguide/user-management.html Add Users> in the /Amazon Connect Administrator Guide/ .
module Network.AWS.Connect.CreateUser
    (
    -- * Creating a request
      CreateUser (..)
    , mkCreateUser
    -- ** Request lenses
    , cuUsername
    , cuPhoneConfig
    , cuSecurityProfileIds
    , cuRoutingProfileId
    , cuInstanceId
    , cuDirectoryUserId
    , cuHierarchyGroupId
    , cuIdentityInfo
    , cuPassword
    , cuTags

    -- * Destructuring the response
    , CreateUserResponse (..)
    , mkCreateUserResponse
    -- ** Response lenses
    , currsUserArn
    , currsUserId
    , currsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateUser' smart constructor.
data CreateUser = CreateUser'
  { username :: Types.Username
    -- ^ The user name for the account. For instances not using SAML for identity management, the user name can include up to 20 characters. If you are using SAML for identity management, the user name can include up to 64 characters from [a-zA-Z0-9_-.\@]+.
  , phoneConfig :: Types.UserPhoneConfig
    -- ^ The phone settings for the user.
  , securityProfileIds :: Core.NonEmpty Types.SecurityProfileId
    -- ^ The identifier of the security profile for the user.
  , routingProfileId :: Types.RoutingProfileId
    -- ^ The identifier of the routing profile for the user.
  , instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , directoryUserId :: Core.Maybe Types.DirectoryUserId
    -- ^ The identifier of the user account in the directory used for identity management. If Amazon Connect cannot access the directory, you can specify this identifier to authenticate users. If you include the identifier, we assume that Amazon Connect cannot access the directory. Otherwise, the identity information is used to authenticate users from your directory.
--
-- This parameter is required if you are using an existing directory for identity management in Amazon Connect when Amazon Connect cannot access your directory to authenticate users. If you are using SAML for identity management and include this parameter, an error is returned.
  , hierarchyGroupId :: Core.Maybe Types.HierarchyGroupId
    -- ^ The identifier of the hierarchy group for the user.
  , identityInfo :: Core.Maybe Types.UserIdentityInfo
    -- ^ The information about the identity of the user.
  , password :: Core.Maybe Types.Password
    -- ^ The password for the user account. A password is required if you are using Amazon Connect for identity management. Otherwise, it is an error to include a password.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ One or more tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUser' value with any optional fields omitted.
mkCreateUser
    :: Types.Username -- ^ 'username'
    -> Types.UserPhoneConfig -- ^ 'phoneConfig'
    -> Core.NonEmpty Types.SecurityProfileId -- ^ 'securityProfileIds'
    -> Types.RoutingProfileId -- ^ 'routingProfileId'
    -> Types.InstanceId -- ^ 'instanceId'
    -> CreateUser
mkCreateUser username phoneConfig securityProfileIds
  routingProfileId instanceId
  = CreateUser'{username, phoneConfig, securityProfileIds,
                routingProfileId, instanceId, directoryUserId = Core.Nothing,
                hierarchyGroupId = Core.Nothing, identityInfo = Core.Nothing,
                password = Core.Nothing, tags = Core.Nothing}

-- | The user name for the account. For instances not using SAML for identity management, the user name can include up to 20 characters. If you are using SAML for identity management, the user name can include up to 64 characters from [a-zA-Z0-9_-.\@]+.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuUsername :: Lens.Lens' CreateUser Types.Username
cuUsername = Lens.field @"username"
{-# INLINEABLE cuUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

-- | The phone settings for the user.
--
-- /Note:/ Consider using 'phoneConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuPhoneConfig :: Lens.Lens' CreateUser Types.UserPhoneConfig
cuPhoneConfig = Lens.field @"phoneConfig"
{-# INLINEABLE cuPhoneConfig #-}
{-# DEPRECATED phoneConfig "Use generic-lens or generic-optics with 'phoneConfig' instead"  #-}

-- | The identifier of the security profile for the user.
--
-- /Note:/ Consider using 'securityProfileIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuSecurityProfileIds :: Lens.Lens' CreateUser (Core.NonEmpty Types.SecurityProfileId)
cuSecurityProfileIds = Lens.field @"securityProfileIds"
{-# INLINEABLE cuSecurityProfileIds #-}
{-# DEPRECATED securityProfileIds "Use generic-lens or generic-optics with 'securityProfileIds' instead"  #-}

-- | The identifier of the routing profile for the user.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuRoutingProfileId :: Lens.Lens' CreateUser Types.RoutingProfileId
cuRoutingProfileId = Lens.field @"routingProfileId"
{-# INLINEABLE cuRoutingProfileId #-}
{-# DEPRECATED routingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead"  #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuInstanceId :: Lens.Lens' CreateUser Types.InstanceId
cuInstanceId = Lens.field @"instanceId"
{-# INLINEABLE cuInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The identifier of the user account in the directory used for identity management. If Amazon Connect cannot access the directory, you can specify this identifier to authenticate users. If you include the identifier, we assume that Amazon Connect cannot access the directory. Otherwise, the identity information is used to authenticate users from your directory.
--
-- This parameter is required if you are using an existing directory for identity management in Amazon Connect when Amazon Connect cannot access your directory to authenticate users. If you are using SAML for identity management and include this parameter, an error is returned.
--
-- /Note:/ Consider using 'directoryUserId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuDirectoryUserId :: Lens.Lens' CreateUser (Core.Maybe Types.DirectoryUserId)
cuDirectoryUserId = Lens.field @"directoryUserId"
{-# INLINEABLE cuDirectoryUserId #-}
{-# DEPRECATED directoryUserId "Use generic-lens or generic-optics with 'directoryUserId' instead"  #-}

-- | The identifier of the hierarchy group for the user.
--
-- /Note:/ Consider using 'hierarchyGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuHierarchyGroupId :: Lens.Lens' CreateUser (Core.Maybe Types.HierarchyGroupId)
cuHierarchyGroupId = Lens.field @"hierarchyGroupId"
{-# INLINEABLE cuHierarchyGroupId #-}
{-# DEPRECATED hierarchyGroupId "Use generic-lens or generic-optics with 'hierarchyGroupId' instead"  #-}

-- | The information about the identity of the user.
--
-- /Note:/ Consider using 'identityInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuIdentityInfo :: Lens.Lens' CreateUser (Core.Maybe Types.UserIdentityInfo)
cuIdentityInfo = Lens.field @"identityInfo"
{-# INLINEABLE cuIdentityInfo #-}
{-# DEPRECATED identityInfo "Use generic-lens or generic-optics with 'identityInfo' instead"  #-}

-- | The password for the user account. A password is required if you are using Amazon Connect for identity management. Otherwise, it is an error to include a password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuPassword :: Lens.Lens' CreateUser (Core.Maybe Types.Password)
cuPassword = Lens.field @"password"
{-# INLINEABLE cuPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuTags :: Lens.Lens' CreateUser (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
cuTags = Lens.field @"tags"
{-# INLINEABLE cuTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateUser where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateUser where
        toHeaders CreateUser{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateUser where
        toJSON CreateUser{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Username" Core..= username),
                  Core.Just ("PhoneConfig" Core..= phoneConfig),
                  Core.Just ("SecurityProfileIds" Core..= securityProfileIds),
                  Core.Just ("RoutingProfileId" Core..= routingProfileId),
                  ("DirectoryUserId" Core..=) Core.<$> directoryUserId,
                  ("HierarchyGroupId" Core..=) Core.<$> hierarchyGroupId,
                  ("IdentityInfo" Core..=) Core.<$> identityInfo,
                  ("Password" Core..=) Core.<$> password,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateUser where
        type Rs CreateUser = CreateUserResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/users/" Core.<> Core.toText instanceId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateUserResponse' Core.<$>
                   (x Core..:? "UserArn") Core.<*> x Core..:? "UserId" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { userArn :: Core.Maybe Types.ARN
    -- ^ The Amazon Resource Name (ARN) of the user account.
  , userId :: Core.Maybe Types.UserId
    -- ^ The identifier of the user account.
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
  = CreateUserResponse'{userArn = Core.Nothing,
                        userId = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the user account.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currsUserArn :: Lens.Lens' CreateUserResponse (Core.Maybe Types.ARN)
currsUserArn = Lens.field @"userArn"
{-# INLINEABLE currsUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

-- | The identifier of the user account.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currsUserId :: Lens.Lens' CreateUserResponse (Core.Maybe Types.UserId)
currsUserId = Lens.field @"userId"
{-# INLINEABLE currsUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currsResponseStatus :: Lens.Lens' CreateUserResponse Core.Int
currsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE currsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
