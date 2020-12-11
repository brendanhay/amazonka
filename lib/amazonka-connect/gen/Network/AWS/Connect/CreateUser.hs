{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    cuDirectoryUserId,
    cuIdentityInfo,
    cuPassword,
    cuHierarchyGroupId,
    cuTags,
    cuUsername,
    cuPhoneConfig,
    cuSecurityProfileIds,
    cuRoutingProfileId,
    cuInstanceId,

    -- * Destructuring the response
    CreateUserResponse (..),
    mkCreateUserResponse,

    -- ** Response lenses
    cursUserId,
    cursUserARN,
    cursResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateUser' smart constructor.
data CreateUser = CreateUser'
  { directoryUserId ::
      Lude.Maybe Lude.Text,
    identityInfo :: Lude.Maybe UserIdentityInfo,
    password :: Lude.Maybe Lude.Text,
    hierarchyGroupId :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    username :: Lude.Text,
    phoneConfig :: UserPhoneConfig,
    securityProfileIds :: Lude.NonEmpty Lude.Text,
    routingProfileId :: Lude.Text,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUser' with the minimum fields required to make a request.
--
-- * 'directoryUserId' - The identifier of the user account in the directory used for identity management. If Amazon Connect cannot access the directory, you can specify this identifier to authenticate users. If you include the identifier, we assume that Amazon Connect cannot access the directory. Otherwise, the identity information is used to authenticate users from your directory.
--
-- This parameter is required if you are using an existing directory for identity management in Amazon Connect when Amazon Connect cannot access your directory to authenticate users. If you are using SAML for identity management and include this parameter, an error is returned.
-- * 'hierarchyGroupId' - The identifier of the hierarchy group for the user.
-- * 'identityInfo' - The information about the identity of the user.
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'password' - The password for the user account. A password is required if you are using Amazon Connect for identity management. Otherwise, it is an error to include a password.
-- * 'phoneConfig' - The phone settings for the user.
-- * 'routingProfileId' - The identifier of the routing profile for the user.
-- * 'securityProfileIds' - The identifier of the security profile for the user.
-- * 'tags' - One or more tags.
-- * 'username' - The user name for the account. For instances not using SAML for identity management, the user name can include up to 20 characters. If you are using SAML for identity management, the user name can include up to 64 characters from [a-zA-Z0-9_-.\@]+.
mkCreateUser ::
  -- | 'username'
  Lude.Text ->
  -- | 'phoneConfig'
  UserPhoneConfig ->
  -- | 'securityProfileIds'
  Lude.NonEmpty Lude.Text ->
  -- | 'routingProfileId'
  Lude.Text ->
  -- | 'instanceId'
  Lude.Text ->
  CreateUser
mkCreateUser
  pUsername_
  pPhoneConfig_
  pSecurityProfileIds_
  pRoutingProfileId_
  pInstanceId_ =
    CreateUser'
      { directoryUserId = Lude.Nothing,
        identityInfo = Lude.Nothing,
        password = Lude.Nothing,
        hierarchyGroupId = Lude.Nothing,
        tags = Lude.Nothing,
        username = pUsername_,
        phoneConfig = pPhoneConfig_,
        securityProfileIds = pSecurityProfileIds_,
        routingProfileId = pRoutingProfileId_,
        instanceId = pInstanceId_
      }

-- | The identifier of the user account in the directory used for identity management. If Amazon Connect cannot access the directory, you can specify this identifier to authenticate users. If you include the identifier, we assume that Amazon Connect cannot access the directory. Otherwise, the identity information is used to authenticate users from your directory.
--
-- This parameter is required if you are using an existing directory for identity management in Amazon Connect when Amazon Connect cannot access your directory to authenticate users. If you are using SAML for identity management and include this parameter, an error is returned.
--
-- /Note:/ Consider using 'directoryUserId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuDirectoryUserId :: Lens.Lens' CreateUser (Lude.Maybe Lude.Text)
cuDirectoryUserId = Lens.lens (directoryUserId :: CreateUser -> Lude.Maybe Lude.Text) (\s a -> s {directoryUserId = a} :: CreateUser)
{-# DEPRECATED cuDirectoryUserId "Use generic-lens or generic-optics with 'directoryUserId' instead." #-}

-- | The information about the identity of the user.
--
-- /Note:/ Consider using 'identityInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuIdentityInfo :: Lens.Lens' CreateUser (Lude.Maybe UserIdentityInfo)
cuIdentityInfo = Lens.lens (identityInfo :: CreateUser -> Lude.Maybe UserIdentityInfo) (\s a -> s {identityInfo = a} :: CreateUser)
{-# DEPRECATED cuIdentityInfo "Use generic-lens or generic-optics with 'identityInfo' instead." #-}

-- | The password for the user account. A password is required if you are using Amazon Connect for identity management. Otherwise, it is an error to include a password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuPassword :: Lens.Lens' CreateUser (Lude.Maybe Lude.Text)
cuPassword = Lens.lens (password :: CreateUser -> Lude.Maybe Lude.Text) (\s a -> s {password = a} :: CreateUser)
{-# DEPRECATED cuPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The identifier of the hierarchy group for the user.
--
-- /Note:/ Consider using 'hierarchyGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuHierarchyGroupId :: Lens.Lens' CreateUser (Lude.Maybe Lude.Text)
cuHierarchyGroupId = Lens.lens (hierarchyGroupId :: CreateUser -> Lude.Maybe Lude.Text) (\s a -> s {hierarchyGroupId = a} :: CreateUser)
{-# DEPRECATED cuHierarchyGroupId "Use generic-lens or generic-optics with 'hierarchyGroupId' instead." #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuTags :: Lens.Lens' CreateUser (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cuTags = Lens.lens (tags :: CreateUser -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateUser)
{-# DEPRECATED cuTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The user name for the account. For instances not using SAML for identity management, the user name can include up to 20 characters. If you are using SAML for identity management, the user name can include up to 64 characters from [a-zA-Z0-9_-.\@]+.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuUsername :: Lens.Lens' CreateUser Lude.Text
cuUsername = Lens.lens (username :: CreateUser -> Lude.Text) (\s a -> s {username = a} :: CreateUser)
{-# DEPRECATED cuUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The phone settings for the user.
--
-- /Note:/ Consider using 'phoneConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuPhoneConfig :: Lens.Lens' CreateUser UserPhoneConfig
cuPhoneConfig = Lens.lens (phoneConfig :: CreateUser -> UserPhoneConfig) (\s a -> s {phoneConfig = a} :: CreateUser)
{-# DEPRECATED cuPhoneConfig "Use generic-lens or generic-optics with 'phoneConfig' instead." #-}

-- | The identifier of the security profile for the user.
--
-- /Note:/ Consider using 'securityProfileIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuSecurityProfileIds :: Lens.Lens' CreateUser (Lude.NonEmpty Lude.Text)
cuSecurityProfileIds = Lens.lens (securityProfileIds :: CreateUser -> Lude.NonEmpty Lude.Text) (\s a -> s {securityProfileIds = a} :: CreateUser)
{-# DEPRECATED cuSecurityProfileIds "Use generic-lens or generic-optics with 'securityProfileIds' instead." #-}

-- | The identifier of the routing profile for the user.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuRoutingProfileId :: Lens.Lens' CreateUser Lude.Text
cuRoutingProfileId = Lens.lens (routingProfileId :: CreateUser -> Lude.Text) (\s a -> s {routingProfileId = a} :: CreateUser)
{-# DEPRECATED cuRoutingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuInstanceId :: Lens.Lens' CreateUser Lude.Text
cuInstanceId = Lens.lens (instanceId :: CreateUser -> Lude.Text) (\s a -> s {instanceId = a} :: CreateUser)
{-# DEPRECATED cuInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest CreateUser where
  type Rs CreateUser = CreateUserResponse
  request = Req.putJSON connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateUserResponse'
            Lude.<$> (x Lude..?> "UserId")
            Lude.<*> (x Lude..?> "UserArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DirectoryUserId" Lude..=) Lude.<$> directoryUserId,
            ("IdentityInfo" Lude..=) Lude.<$> identityInfo,
            ("Password" Lude..=) Lude.<$> password,
            ("HierarchyGroupId" Lude..=) Lude.<$> hierarchyGroupId,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Username" Lude..= username),
            Lude.Just ("PhoneConfig" Lude..= phoneConfig),
            Lude.Just ("SecurityProfileIds" Lude..= securityProfileIds),
            Lude.Just ("RoutingProfileId" Lude..= routingProfileId)
          ]
      )

instance Lude.ToPath CreateUser where
  toPath CreateUser' {..} =
    Lude.mconcat ["/users/", Lude.toBS instanceId]

instance Lude.ToQuery CreateUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { userId ::
      Lude.Maybe Lude.Text,
    userARN :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'userARN' - The Amazon Resource Name (ARN) of the user account.
-- * 'userId' - The identifier of the user account.
mkCreateUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateUserResponse
mkCreateUserResponse pResponseStatus_ =
  CreateUserResponse'
    { userId = Lude.Nothing,
      userARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the user account.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cursUserId :: Lens.Lens' CreateUserResponse (Lude.Maybe Lude.Text)
cursUserId = Lens.lens (userId :: CreateUserResponse -> Lude.Maybe Lude.Text) (\s a -> s {userId = a} :: CreateUserResponse)
{-# DEPRECATED cursUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The Amazon Resource Name (ARN) of the user account.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cursUserARN :: Lens.Lens' CreateUserResponse (Lude.Maybe Lude.Text)
cursUserARN = Lens.lens (userARN :: CreateUserResponse -> Lude.Maybe Lude.Text) (\s a -> s {userARN = a} :: CreateUserResponse)
{-# DEPRECATED cursUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cursResponseStatus :: Lens.Lens' CreateUserResponse Lude.Int
cursResponseStatus = Lens.lens (responseStatus :: CreateUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateUserResponse)
{-# DEPRECATED cursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
