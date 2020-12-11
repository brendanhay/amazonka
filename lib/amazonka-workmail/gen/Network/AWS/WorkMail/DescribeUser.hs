{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DescribeUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information regarding the user.
module Network.AWS.WorkMail.DescribeUser
  ( -- * Creating a request
    DescribeUser (..),
    mkDescribeUser,

    -- ** Request lenses
    duOrganizationId,
    duUserId,

    -- * Destructuring the response
    DescribeUserResponse (..),
    mkDescribeUserResponse,

    -- ** Response lenses
    dursEmail,
    dursState,
    dursUserId,
    dursDisabledDate,
    dursName,
    dursDisplayName,
    dursUserRole,
    dursEnabledDate,
    dursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkDescribeUser' smart constructor.
data DescribeUser = DescribeUser'
  { organizationId :: Lude.Text,
    userId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUser' with the minimum fields required to make a request.
--
-- * 'organizationId' - The identifier for the organization under which the user exists.
-- * 'userId' - The identifier for the user to be described.
mkDescribeUser ::
  -- | 'organizationId'
  Lude.Text ->
  -- | 'userId'
  Lude.Text ->
  DescribeUser
mkDescribeUser pOrganizationId_ pUserId_ =
  DescribeUser'
    { organizationId = pOrganizationId_,
      userId = pUserId_
    }

-- | The identifier for the organization under which the user exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duOrganizationId :: Lens.Lens' DescribeUser Lude.Text
duOrganizationId = Lens.lens (organizationId :: DescribeUser -> Lude.Text) (\s a -> s {organizationId = a} :: DescribeUser)
{-# DEPRECATED duOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifier for the user to be described.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duUserId :: Lens.Lens' DescribeUser Lude.Text
duUserId = Lens.lens (userId :: DescribeUser -> Lude.Text) (\s a -> s {userId = a} :: DescribeUser)
{-# DEPRECATED duUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Lude.AWSRequest DescribeUser where
  type Rs DescribeUser = DescribeUserResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeUserResponse'
            Lude.<$> (x Lude..?> "Email")
            Lude.<*> (x Lude..?> "State")
            Lude.<*> (x Lude..?> "UserId")
            Lude.<*> (x Lude..?> "DisabledDate")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "DisplayName")
            Lude.<*> (x Lude..?> "UserRole")
            Lude.<*> (x Lude..?> "EnabledDate")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.DescribeUser" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeUser where
  toJSON DescribeUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("OrganizationId" Lude..= organizationId),
            Lude.Just ("UserId" Lude..= userId)
          ]
      )

instance Lude.ToPath DescribeUser where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeUserResponse' smart constructor.
data DescribeUserResponse = DescribeUserResponse'
  { email ::
      Lude.Maybe Lude.Text,
    state :: Lude.Maybe EntityState,
    userId :: Lude.Maybe Lude.Text,
    disabledDate :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    displayName :: Lude.Maybe Lude.Text,
    userRole :: Lude.Maybe UserRole,
    enabledDate :: Lude.Maybe Lude.Timestamp,
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

-- | Creates a value of 'DescribeUserResponse' with the minimum fields required to make a request.
--
-- * 'disabledDate' - The date and time at which the user was disabled for Amazon WorkMail usage, in UNIX epoch time format.
-- * 'displayName' - The display name of the user.
-- * 'email' - The email of the user.
-- * 'enabledDate' - The date and time at which the user was enabled for Amazon WorkMail usage, in UNIX epoch time format.
-- * 'name' - The name for the user.
-- * 'responseStatus' - The response status code.
-- * 'state' - The state of a user: enabled (registered to Amazon WorkMail) or disabled (deregistered or never registered to WorkMail).
-- * 'userId' - The identifier for the described user.
-- * 'userRole' - In certain cases, other entities are modeled as users. If interoperability is enabled, resources are imported into Amazon WorkMail as users. Because different WorkMail organizations rely on different directory types, administrators can distinguish between an unregistered user (account is disabled and has a user role) and the directory administrators. The values are USER, RESOURCE, and SYSTEM_USER.
mkDescribeUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeUserResponse
mkDescribeUserResponse pResponseStatus_ =
  DescribeUserResponse'
    { email = Lude.Nothing,
      state = Lude.Nothing,
      userId = Lude.Nothing,
      disabledDate = Lude.Nothing,
      name = Lude.Nothing,
      displayName = Lude.Nothing,
      userRole = Lude.Nothing,
      enabledDate = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The email of the user.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursEmail :: Lens.Lens' DescribeUserResponse (Lude.Maybe Lude.Text)
dursEmail = Lens.lens (email :: DescribeUserResponse -> Lude.Maybe Lude.Text) (\s a -> s {email = a} :: DescribeUserResponse)
{-# DEPRECATED dursEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The state of a user: enabled (registered to Amazon WorkMail) or disabled (deregistered or never registered to WorkMail).
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursState :: Lens.Lens' DescribeUserResponse (Lude.Maybe EntityState)
dursState = Lens.lens (state :: DescribeUserResponse -> Lude.Maybe EntityState) (\s a -> s {state = a} :: DescribeUserResponse)
{-# DEPRECATED dursState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The identifier for the described user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursUserId :: Lens.Lens' DescribeUserResponse (Lude.Maybe Lude.Text)
dursUserId = Lens.lens (userId :: DescribeUserResponse -> Lude.Maybe Lude.Text) (\s a -> s {userId = a} :: DescribeUserResponse)
{-# DEPRECATED dursUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The date and time at which the user was disabled for Amazon WorkMail usage, in UNIX epoch time format.
--
-- /Note:/ Consider using 'disabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursDisabledDate :: Lens.Lens' DescribeUserResponse (Lude.Maybe Lude.Timestamp)
dursDisabledDate = Lens.lens (disabledDate :: DescribeUserResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {disabledDate = a} :: DescribeUserResponse)
{-# DEPRECATED dursDisabledDate "Use generic-lens or generic-optics with 'disabledDate' instead." #-}

-- | The name for the user.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursName :: Lens.Lens' DescribeUserResponse (Lude.Maybe Lude.Text)
dursName = Lens.lens (name :: DescribeUserResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeUserResponse)
{-# DEPRECATED dursName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The display name of the user.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursDisplayName :: Lens.Lens' DescribeUserResponse (Lude.Maybe Lude.Text)
dursDisplayName = Lens.lens (displayName :: DescribeUserResponse -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: DescribeUserResponse)
{-# DEPRECATED dursDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | In certain cases, other entities are modeled as users. If interoperability is enabled, resources are imported into Amazon WorkMail as users. Because different WorkMail organizations rely on different directory types, administrators can distinguish between an unregistered user (account is disabled and has a user role) and the directory administrators. The values are USER, RESOURCE, and SYSTEM_USER.
--
-- /Note:/ Consider using 'userRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursUserRole :: Lens.Lens' DescribeUserResponse (Lude.Maybe UserRole)
dursUserRole = Lens.lens (userRole :: DescribeUserResponse -> Lude.Maybe UserRole) (\s a -> s {userRole = a} :: DescribeUserResponse)
{-# DEPRECATED dursUserRole "Use generic-lens or generic-optics with 'userRole' instead." #-}

-- | The date and time at which the user was enabled for Amazon WorkMail usage, in UNIX epoch time format.
--
-- /Note:/ Consider using 'enabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursEnabledDate :: Lens.Lens' DescribeUserResponse (Lude.Maybe Lude.Timestamp)
dursEnabledDate = Lens.lens (enabledDate :: DescribeUserResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {enabledDate = a} :: DescribeUserResponse)
{-# DEPRECATED dursEnabledDate "Use generic-lens or generic-optics with 'enabledDate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursResponseStatus :: Lens.Lens' DescribeUserResponse Lude.Int
dursResponseStatus = Lens.lens (responseStatus :: DescribeUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeUserResponse)
{-# DEPRECATED dursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
