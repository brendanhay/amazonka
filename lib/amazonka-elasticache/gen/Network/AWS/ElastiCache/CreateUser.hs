{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For Redis engine version 6.x onwards: Creates a Redis user. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.RBAC.html Using Role Based Access Control (RBAC)> .
module Network.AWS.ElastiCache.CreateUser
  ( -- * Creating a request
    CreateUser (..),
    mkCreateUser,

    -- ** Request lenses
    cuPasswords,
    cuNoPasswordRequired,
    cuUserId,
    cuUserName,
    cuEngine,
    cuAccessString,

    -- * Destructuring the response
    User (..),
    mkUser,

    -- ** Response lenses
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

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateUser' smart constructor.
data CreateUser = CreateUser'
  { passwords ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    noPasswordRequired :: Lude.Maybe Lude.Bool,
    userId :: Lude.Text,
    userName :: Lude.Text,
    engine :: Lude.Text,
    accessString :: Lude.Text
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
-- * 'accessString' - Access permissions string used for this user account.
-- * 'engine' - Must be Redis.
-- * 'noPasswordRequired' - Indicates a password is not required for this user account.
-- * 'passwords' - Passwords used for this user account. You can create up to two passwords for each user.
-- * 'userId' - The ID of the user.
-- * 'userName' - The username of the user.
mkCreateUser ::
  -- | 'userId'
  Lude.Text ->
  -- | 'userName'
  Lude.Text ->
  -- | 'engine'
  Lude.Text ->
  -- | 'accessString'
  Lude.Text ->
  CreateUser
mkCreateUser pUserId_ pUserName_ pEngine_ pAccessString_ =
  CreateUser'
    { passwords = Lude.Nothing,
      noPasswordRequired = Lude.Nothing,
      userId = pUserId_,
      userName = pUserName_,
      engine = pEngine_,
      accessString = pAccessString_
    }

-- | Passwords used for this user account. You can create up to two passwords for each user.
--
-- /Note:/ Consider using 'passwords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuPasswords :: Lens.Lens' CreateUser (Lude.Maybe (Lude.NonEmpty Lude.Text))
cuPasswords = Lens.lens (passwords :: CreateUser -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {passwords = a} :: CreateUser)
{-# DEPRECATED cuPasswords "Use generic-lens or generic-optics with 'passwords' instead." #-}

-- | Indicates a password is not required for this user account.
--
-- /Note:/ Consider using 'noPasswordRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuNoPasswordRequired :: Lens.Lens' CreateUser (Lude.Maybe Lude.Bool)
cuNoPasswordRequired = Lens.lens (noPasswordRequired :: CreateUser -> Lude.Maybe Lude.Bool) (\s a -> s {noPasswordRequired = a} :: CreateUser)
{-# DEPRECATED cuNoPasswordRequired "Use generic-lens or generic-optics with 'noPasswordRequired' instead." #-}

-- | The ID of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuUserId :: Lens.Lens' CreateUser Lude.Text
cuUserId = Lens.lens (userId :: CreateUser -> Lude.Text) (\s a -> s {userId = a} :: CreateUser)
{-# DEPRECATED cuUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The username of the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuUserName :: Lens.Lens' CreateUser Lude.Text
cuUserName = Lens.lens (userName :: CreateUser -> Lude.Text) (\s a -> s {userName = a} :: CreateUser)
{-# DEPRECATED cuUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | Must be Redis.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuEngine :: Lens.Lens' CreateUser Lude.Text
cuEngine = Lens.lens (engine :: CreateUser -> Lude.Text) (\s a -> s {engine = a} :: CreateUser)
{-# DEPRECATED cuEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | Access permissions string used for this user account.
--
-- /Note:/ Consider using 'accessString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuAccessString :: Lens.Lens' CreateUser Lude.Text
cuAccessString = Lens.lens (accessString :: CreateUser -> Lude.Text) (\s a -> s {accessString = a} :: CreateUser)
{-# DEPRECATED cuAccessString "Use generic-lens or generic-optics with 'accessString' instead." #-}

instance Lude.AWSRequest CreateUser where
  type Rs CreateUser = User
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "CreateUserResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders CreateUser where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateUser where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateUser where
  toQuery CreateUser' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateUser" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "Passwords"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> passwords),
        "NoPasswordRequired" Lude.=: noPasswordRequired,
        "UserId" Lude.=: userId,
        "UserName" Lude.=: userName,
        "Engine" Lude.=: engine,
        "AccessString" Lude.=: accessString
      ]
