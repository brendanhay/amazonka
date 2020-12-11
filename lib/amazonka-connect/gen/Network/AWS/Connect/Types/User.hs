-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.User
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.User
  ( User (..),

    -- * Smart constructor
    mkUser,

    -- * Lenses
    uRoutingProfileId,
    uDirectoryUserId,
    uARN,
    uIdentityInfo,
    uSecurityProfileIds,
    uUsername,
    uId,
    uHierarchyGroupId,
    uPhoneConfig,
    uTags,
  )
where

import Network.AWS.Connect.Types.UserIdentityInfo
import Network.AWS.Connect.Types.UserPhoneConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a user account for a Amazon Connect instance.
--
-- /See:/ 'mkUser' smart constructor.
data User = User'
  { routingProfileId :: Lude.Maybe Lude.Text,
    directoryUserId :: Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    identityInfo :: Lude.Maybe UserIdentityInfo,
    securityProfileIds :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    username :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    hierarchyGroupId :: Lude.Maybe Lude.Text,
    phoneConfig :: Lude.Maybe UserPhoneConfig,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the user account.
-- * 'directoryUserId' - The identifier of the user account in the directory used for identity management.
-- * 'hierarchyGroupId' - The identifier of the hierarchy group for the user.
-- * 'id' - The identifier of the user account.
-- * 'identityInfo' - Information about the user identity.
-- * 'phoneConfig' - Information about the phone configuration for the user.
-- * 'routingProfileId' - The identifier of the routing profile for the user.
-- * 'securityProfileIds' - The identifiers of the security profiles for the user.
-- * 'tags' - The tags.
-- * 'username' - The user name assigned to the user account.
mkUser ::
  User
mkUser =
  User'
    { routingProfileId = Lude.Nothing,
      directoryUserId = Lude.Nothing,
      arn = Lude.Nothing,
      identityInfo = Lude.Nothing,
      securityProfileIds = Lude.Nothing,
      username = Lude.Nothing,
      id = Lude.Nothing,
      hierarchyGroupId = Lude.Nothing,
      phoneConfig = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The identifier of the routing profile for the user.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uRoutingProfileId :: Lens.Lens' User (Lude.Maybe Lude.Text)
uRoutingProfileId = Lens.lens (routingProfileId :: User -> Lude.Maybe Lude.Text) (\s a -> s {routingProfileId = a} :: User)
{-# DEPRECATED uRoutingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead." #-}

-- | The identifier of the user account in the directory used for identity management.
--
-- /Note:/ Consider using 'directoryUserId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDirectoryUserId :: Lens.Lens' User (Lude.Maybe Lude.Text)
uDirectoryUserId = Lens.lens (directoryUserId :: User -> Lude.Maybe Lude.Text) (\s a -> s {directoryUserId = a} :: User)
{-# DEPRECATED uDirectoryUserId "Use generic-lens or generic-optics with 'directoryUserId' instead." #-}

-- | The Amazon Resource Name (ARN) of the user account.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uARN :: Lens.Lens' User (Lude.Maybe Lude.Text)
uARN = Lens.lens (arn :: User -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: User)
{-# DEPRECATED uARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Information about the user identity.
--
-- /Note:/ Consider using 'identityInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uIdentityInfo :: Lens.Lens' User (Lude.Maybe UserIdentityInfo)
uIdentityInfo = Lens.lens (identityInfo :: User -> Lude.Maybe UserIdentityInfo) (\s a -> s {identityInfo = a} :: User)
{-# DEPRECATED uIdentityInfo "Use generic-lens or generic-optics with 'identityInfo' instead." #-}

-- | The identifiers of the security profiles for the user.
--
-- /Note:/ Consider using 'securityProfileIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uSecurityProfileIds :: Lens.Lens' User (Lude.Maybe (Lude.NonEmpty Lude.Text))
uSecurityProfileIds = Lens.lens (securityProfileIds :: User -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {securityProfileIds = a} :: User)
{-# DEPRECATED uSecurityProfileIds "Use generic-lens or generic-optics with 'securityProfileIds' instead." #-}

-- | The user name assigned to the user account.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUsername :: Lens.Lens' User (Lude.Maybe Lude.Text)
uUsername = Lens.lens (username :: User -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: User)
{-# DEPRECATED uUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The identifier of the user account.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uId :: Lens.Lens' User (Lude.Maybe Lude.Text)
uId = Lens.lens (id :: User -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: User)
{-# DEPRECATED uId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The identifier of the hierarchy group for the user.
--
-- /Note:/ Consider using 'hierarchyGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uHierarchyGroupId :: Lens.Lens' User (Lude.Maybe Lude.Text)
uHierarchyGroupId = Lens.lens (hierarchyGroupId :: User -> Lude.Maybe Lude.Text) (\s a -> s {hierarchyGroupId = a} :: User)
{-# DEPRECATED uHierarchyGroupId "Use generic-lens or generic-optics with 'hierarchyGroupId' instead." #-}

-- | Information about the phone configuration for the user.
--
-- /Note:/ Consider using 'phoneConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uPhoneConfig :: Lens.Lens' User (Lude.Maybe UserPhoneConfig)
uPhoneConfig = Lens.lens (phoneConfig :: User -> Lude.Maybe UserPhoneConfig) (\s a -> s {phoneConfig = a} :: User)
{-# DEPRECATED uPhoneConfig "Use generic-lens or generic-optics with 'phoneConfig' instead." #-}

-- | The tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uTags :: Lens.Lens' User (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
uTags = Lens.lens (tags :: User -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: User)
{-# DEPRECATED uTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON User where
  parseJSON =
    Lude.withObject
      "User"
      ( \x ->
          User'
            Lude.<$> (x Lude..:? "RoutingProfileId")
            Lude.<*> (x Lude..:? "DirectoryUserId")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "IdentityInfo")
            Lude.<*> (x Lude..:? "SecurityProfileIds")
            Lude.<*> (x Lude..:? "Username")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "HierarchyGroupId")
            Lude.<*> (x Lude..:? "PhoneConfig")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
      )
