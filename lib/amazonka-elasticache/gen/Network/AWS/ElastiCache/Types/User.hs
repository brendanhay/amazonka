{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.User
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.User
  ( User (..),

    -- * Smart constructor
    mkUser,

    -- * Lenses
    uStatus,
    uARN,
    uUserGroupIds,
    uAuthentication,
    uEngine,
    uUserName,
    uAccessString,
    uUserId,
  )
where

import Network.AWS.ElastiCache.Types.Authentication
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkUser' smart constructor.
data User = User'
  { -- | Indicates the user status. Can be "active", "modifying" or "deleting".
    status :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the user account.
    arn :: Lude.Maybe Lude.Text,
    -- | Returns a list of the user group IDs the user belongs to.
    userGroupIds :: Lude.Maybe [Lude.Text],
    -- | Denotes whether the user requires a password to authenticate.
    authentication :: Lude.Maybe Authentication,
    -- | Must be Redis.
    engine :: Lude.Maybe Lude.Text,
    -- | The username of the user.
    userName :: Lude.Maybe Lude.Text,
    -- | Access permissions string used for this user account.
    accessString :: Lude.Maybe Lude.Text,
    -- | The ID of the user.
    userId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- * 'status' - Indicates the user status. Can be "active", "modifying" or "deleting".
-- * 'arn' - The Amazon Resource Name (ARN) of the user account.
-- * 'userGroupIds' - Returns a list of the user group IDs the user belongs to.
-- * 'authentication' - Denotes whether the user requires a password to authenticate.
-- * 'engine' - Must be Redis.
-- * 'userName' - The username of the user.
-- * 'accessString' - Access permissions string used for this user account.
-- * 'userId' - The ID of the user.
mkUser ::
  User
mkUser =
  User'
    { status = Lude.Nothing,
      arn = Lude.Nothing,
      userGroupIds = Lude.Nothing,
      authentication = Lude.Nothing,
      engine = Lude.Nothing,
      userName = Lude.Nothing,
      accessString = Lude.Nothing,
      userId = Lude.Nothing
    }

-- | Indicates the user status. Can be "active", "modifying" or "deleting".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uStatus :: Lens.Lens' User (Lude.Maybe Lude.Text)
uStatus = Lens.lens (status :: User -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: User)
{-# DEPRECATED uStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Name (ARN) of the user account.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uARN :: Lens.Lens' User (Lude.Maybe Lude.Text)
uARN = Lens.lens (arn :: User -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: User)
{-# DEPRECATED uARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Returns a list of the user group IDs the user belongs to.
--
-- /Note:/ Consider using 'userGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUserGroupIds :: Lens.Lens' User (Lude.Maybe [Lude.Text])
uUserGroupIds = Lens.lens (userGroupIds :: User -> Lude.Maybe [Lude.Text]) (\s a -> s {userGroupIds = a} :: User)
{-# DEPRECATED uUserGroupIds "Use generic-lens or generic-optics with 'userGroupIds' instead." #-}

-- | Denotes whether the user requires a password to authenticate.
--
-- /Note:/ Consider using 'authentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uAuthentication :: Lens.Lens' User (Lude.Maybe Authentication)
uAuthentication = Lens.lens (authentication :: User -> Lude.Maybe Authentication) (\s a -> s {authentication = a} :: User)
{-# DEPRECATED uAuthentication "Use generic-lens or generic-optics with 'authentication' instead." #-}

-- | Must be Redis.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uEngine :: Lens.Lens' User (Lude.Maybe Lude.Text)
uEngine = Lens.lens (engine :: User -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: User)
{-# DEPRECATED uEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The username of the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUserName :: Lens.Lens' User (Lude.Maybe Lude.Text)
uUserName = Lens.lens (userName :: User -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: User)
{-# DEPRECATED uUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | Access permissions string used for this user account.
--
-- /Note:/ Consider using 'accessString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uAccessString :: Lens.Lens' User (Lude.Maybe Lude.Text)
uAccessString = Lens.lens (accessString :: User -> Lude.Maybe Lude.Text) (\s a -> s {accessString = a} :: User)
{-# DEPRECATED uAccessString "Use generic-lens or generic-optics with 'accessString' instead." #-}

-- | The ID of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUserId :: Lens.Lens' User (Lude.Maybe Lude.Text)
uUserId = Lens.lens (userId :: User -> Lude.Maybe Lude.Text) (\s a -> s {userId = a} :: User)
{-# DEPRECATED uUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Lude.FromXML User where
  parseXML x =
    User'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "ARN")
      Lude.<*> ( x Lude..@? "UserGroupIds" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "Authentication")
      Lude.<*> (x Lude..@? "Engine")
      Lude.<*> (x Lude..@? "UserName")
      Lude.<*> (x Lude..@? "AccessString")
      Lude.<*> (x Lude..@? "UserId")
