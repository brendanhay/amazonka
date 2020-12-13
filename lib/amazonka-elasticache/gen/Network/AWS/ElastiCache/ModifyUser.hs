{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ModifyUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes user password(s) and/or access string.
module Network.AWS.ElastiCache.ModifyUser
  ( -- * Creating a request
    ModifyUser (..),
    mkModifyUser,

    -- ** Request lenses
    muAppendAccessString,
    muAccessString,
    muUserId,
    muPasswords,
    muNoPasswordRequired,

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

-- | /See:/ 'mkModifyUser' smart constructor.
data ModifyUser = ModifyUser'
  { -- | Adds additional user permissions to the access string.
    appendAccessString :: Lude.Maybe Lude.Text,
    -- | Access permissions string used for this user account.
    accessString :: Lude.Maybe Lude.Text,
    -- | The ID of the user.
    userId :: Lude.Text,
    -- | The passwords belonging to the user account. You are allowed up to two.
    passwords :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | Indicates no password is required for the user account.
    noPasswordRequired :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyUser' with the minimum fields required to make a request.
--
-- * 'appendAccessString' - Adds additional user permissions to the access string.
-- * 'accessString' - Access permissions string used for this user account.
-- * 'userId' - The ID of the user.
-- * 'passwords' - The passwords belonging to the user account. You are allowed up to two.
-- * 'noPasswordRequired' - Indicates no password is required for the user account.
mkModifyUser ::
  -- | 'userId'
  Lude.Text ->
  ModifyUser
mkModifyUser pUserId_ =
  ModifyUser'
    { appendAccessString = Lude.Nothing,
      accessString = Lude.Nothing,
      userId = pUserId_,
      passwords = Lude.Nothing,
      noPasswordRequired = Lude.Nothing
    }

-- | Adds additional user permissions to the access string.
--
-- /Note:/ Consider using 'appendAccessString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muAppendAccessString :: Lens.Lens' ModifyUser (Lude.Maybe Lude.Text)
muAppendAccessString = Lens.lens (appendAccessString :: ModifyUser -> Lude.Maybe Lude.Text) (\s a -> s {appendAccessString = a} :: ModifyUser)
{-# DEPRECATED muAppendAccessString "Use generic-lens or generic-optics with 'appendAccessString' instead." #-}

-- | Access permissions string used for this user account.
--
-- /Note:/ Consider using 'accessString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muAccessString :: Lens.Lens' ModifyUser (Lude.Maybe Lude.Text)
muAccessString = Lens.lens (accessString :: ModifyUser -> Lude.Maybe Lude.Text) (\s a -> s {accessString = a} :: ModifyUser)
{-# DEPRECATED muAccessString "Use generic-lens or generic-optics with 'accessString' instead." #-}

-- | The ID of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muUserId :: Lens.Lens' ModifyUser Lude.Text
muUserId = Lens.lens (userId :: ModifyUser -> Lude.Text) (\s a -> s {userId = a} :: ModifyUser)
{-# DEPRECATED muUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The passwords belonging to the user account. You are allowed up to two.
--
-- /Note:/ Consider using 'passwords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muPasswords :: Lens.Lens' ModifyUser (Lude.Maybe (Lude.NonEmpty Lude.Text))
muPasswords = Lens.lens (passwords :: ModifyUser -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {passwords = a} :: ModifyUser)
{-# DEPRECATED muPasswords "Use generic-lens or generic-optics with 'passwords' instead." #-}

-- | Indicates no password is required for the user account.
--
-- /Note:/ Consider using 'noPasswordRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muNoPasswordRequired :: Lens.Lens' ModifyUser (Lude.Maybe Lude.Bool)
muNoPasswordRequired = Lens.lens (noPasswordRequired :: ModifyUser -> Lude.Maybe Lude.Bool) (\s a -> s {noPasswordRequired = a} :: ModifyUser)
{-# DEPRECATED muNoPasswordRequired "Use generic-lens or generic-optics with 'noPasswordRequired' instead." #-}

instance Lude.AWSRequest ModifyUser where
  type Rs ModifyUser = User
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "ModifyUserResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders ModifyUser where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyUser where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyUser where
  toQuery ModifyUser' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyUser" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "AppendAccessString" Lude.=: appendAccessString,
        "AccessString" Lude.=: accessString,
        "UserId" Lude.=: userId,
        "Passwords"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> passwords),
        "NoPasswordRequired" Lude.=: noPasswordRequired
      ]
