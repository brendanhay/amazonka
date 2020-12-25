{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateUser (..),
    mkCreateUser,

    -- ** Request lenses
    cuUsername,
    cuPhoneConfig,
    cuSecurityProfileIds,
    cuRoutingProfileId,
    cuInstanceId,
    cuDirectoryUserId,
    cuHierarchyGroupId,
    cuIdentityInfo,
    cuPassword,
    cuTags,

    -- * Destructuring the response
    CreateUserResponse (..),
    mkCreateUserResponse,

    -- ** Response lenses
    currsUserArn,
    currsUserId,
    currsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The user name for the account. For instances not using SAML for identity management, the user name can include up to 20 characters. If you are using SAML for identity management, the user name can include up to 64 characters from [a-zA-Z0-9_-.\@]+.
    username :: Types.Username,
    -- | The phone settings for the user.
    phoneConfig :: Types.UserPhoneConfig,
    -- | The identifier of the security profile for the user.
    securityProfileIds :: Core.NonEmpty Types.SecurityProfileId,
    -- | The identifier of the routing profile for the user.
    routingProfileId :: Types.RoutingProfileId,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The identifier of the user account in the directory used for identity management. If Amazon Connect cannot access the directory, you can specify this identifier to authenticate users. If you include the identifier, we assume that Amazon Connect cannot access the directory. Otherwise, the identity information is used to authenticate users from your directory.
    --
    -- This parameter is required if you are using an existing directory for identity management in Amazon Connect when Amazon Connect cannot access your directory to authenticate users. If you are using SAML for identity management and include this parameter, an error is returned.
    directoryUserId :: Core.Maybe Types.DirectoryUserId,
    -- | The identifier of the hierarchy group for the user.
    hierarchyGroupId :: Core.Maybe Types.HierarchyGroupId,
    -- | The information about the identity of the user.
    identityInfo :: Core.Maybe Types.UserIdentityInfo,
    -- | The password for the user account. A password is required if you are using Amazon Connect for identity management. Otherwise, it is an error to include a password.
    password :: Core.Maybe Types.Password,
    -- | One or more tags.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUser' value with any optional fields omitted.
mkCreateUser ::
  -- | 'username'
  Types.Username ->
  -- | 'phoneConfig'
  Types.UserPhoneConfig ->
  -- | 'securityProfileIds'
  Core.NonEmpty Types.SecurityProfileId ->
  -- | 'routingProfileId'
  Types.RoutingProfileId ->
  -- | 'instanceId'
  Types.InstanceId ->
  CreateUser
mkCreateUser
  username
  phoneConfig
  securityProfileIds
  routingProfileId
  instanceId =
    CreateUser'
      { username,
        phoneConfig,
        securityProfileIds,
        routingProfileId,
        instanceId,
        directoryUserId = Core.Nothing,
        hierarchyGroupId = Core.Nothing,
        identityInfo = Core.Nothing,
        password = Core.Nothing,
        tags = Core.Nothing
      }

-- | The user name for the account. For instances not using SAML for identity management, the user name can include up to 20 characters. If you are using SAML for identity management, the user name can include up to 64 characters from [a-zA-Z0-9_-.\@]+.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuUsername :: Lens.Lens' CreateUser Types.Username
cuUsername = Lens.field @"username"
{-# DEPRECATED cuUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The phone settings for the user.
--
-- /Note:/ Consider using 'phoneConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuPhoneConfig :: Lens.Lens' CreateUser Types.UserPhoneConfig
cuPhoneConfig = Lens.field @"phoneConfig"
{-# DEPRECATED cuPhoneConfig "Use generic-lens or generic-optics with 'phoneConfig' instead." #-}

-- | The identifier of the security profile for the user.
--
-- /Note:/ Consider using 'securityProfileIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuSecurityProfileIds :: Lens.Lens' CreateUser (Core.NonEmpty Types.SecurityProfileId)
cuSecurityProfileIds = Lens.field @"securityProfileIds"
{-# DEPRECATED cuSecurityProfileIds "Use generic-lens or generic-optics with 'securityProfileIds' instead." #-}

-- | The identifier of the routing profile for the user.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuRoutingProfileId :: Lens.Lens' CreateUser Types.RoutingProfileId
cuRoutingProfileId = Lens.field @"routingProfileId"
{-# DEPRECATED cuRoutingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuInstanceId :: Lens.Lens' CreateUser Types.InstanceId
cuInstanceId = Lens.field @"instanceId"
{-# DEPRECATED cuInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the user account in the directory used for identity management. If Amazon Connect cannot access the directory, you can specify this identifier to authenticate users. If you include the identifier, we assume that Amazon Connect cannot access the directory. Otherwise, the identity information is used to authenticate users from your directory.
--
-- This parameter is required if you are using an existing directory for identity management in Amazon Connect when Amazon Connect cannot access your directory to authenticate users. If you are using SAML for identity management and include this parameter, an error is returned.
--
-- /Note:/ Consider using 'directoryUserId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuDirectoryUserId :: Lens.Lens' CreateUser (Core.Maybe Types.DirectoryUserId)
cuDirectoryUserId = Lens.field @"directoryUserId"
{-# DEPRECATED cuDirectoryUserId "Use generic-lens or generic-optics with 'directoryUserId' instead." #-}

-- | The identifier of the hierarchy group for the user.
--
-- /Note:/ Consider using 'hierarchyGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuHierarchyGroupId :: Lens.Lens' CreateUser (Core.Maybe Types.HierarchyGroupId)
cuHierarchyGroupId = Lens.field @"hierarchyGroupId"
{-# DEPRECATED cuHierarchyGroupId "Use generic-lens or generic-optics with 'hierarchyGroupId' instead." #-}

-- | The information about the identity of the user.
--
-- /Note:/ Consider using 'identityInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuIdentityInfo :: Lens.Lens' CreateUser (Core.Maybe Types.UserIdentityInfo)
cuIdentityInfo = Lens.field @"identityInfo"
{-# DEPRECATED cuIdentityInfo "Use generic-lens or generic-optics with 'identityInfo' instead." #-}

-- | The password for the user account. A password is required if you are using Amazon Connect for identity management. Otherwise, it is an error to include a password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuPassword :: Lens.Lens' CreateUser (Core.Maybe Types.Password)
cuPassword = Lens.field @"password"
{-# DEPRECATED cuPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuTags :: Lens.Lens' CreateUser (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
cuTags = Lens.field @"tags"
{-# DEPRECATED cuTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateUser where
  toJSON CreateUser {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Username" Core..= username),
            Core.Just ("PhoneConfig" Core..= phoneConfig),
            Core.Just ("SecurityProfileIds" Core..= securityProfileIds),
            Core.Just ("RoutingProfileId" Core..= routingProfileId),
            ("DirectoryUserId" Core..=) Core.<$> directoryUserId,
            ("HierarchyGroupId" Core..=) Core.<$> hierarchyGroupId,
            ("IdentityInfo" Core..=) Core.<$> identityInfo,
            ("Password" Core..=) Core.<$> password,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateUser where
  type Rs CreateUser = CreateUserResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath ("/users/" Core.<> (Core.toText instanceId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserResponse'
            Core.<$> (x Core..:? "UserArn")
            Core.<*> (x Core..:? "UserId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The Amazon Resource Name (ARN) of the user account.
    userArn :: Core.Maybe Types.ARN,
    -- | The identifier of the user account.
    userId :: Core.Maybe Types.UserId,
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
  CreateUserResponse'
    { userArn = Core.Nothing,
      userId = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the user account.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currsUserArn :: Lens.Lens' CreateUserResponse (Core.Maybe Types.ARN)
currsUserArn = Lens.field @"userArn"
{-# DEPRECATED currsUserArn "Use generic-lens or generic-optics with 'userArn' instead." #-}

-- | The identifier of the user account.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currsUserId :: Lens.Lens' CreateUserResponse (Core.Maybe Types.UserId)
currsUserId = Lens.field @"userId"
{-# DEPRECATED currsUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currsResponseStatus :: Lens.Lens' CreateUserResponse Core.Int
currsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED currsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
