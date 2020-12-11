{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DescribeUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified user account. You can find the instance ID in the console (it’s the final part of the ARN). The console does not display the user IDs. Instead, list the users and note the IDs provided in the output.
module Network.AWS.Connect.DescribeUser
  ( -- * Creating a request
    DescribeUser (..),
    mkDescribeUser,

    -- ** Request lenses
    duUserId,
    duInstanceId,

    -- * Destructuring the response
    DescribeUserResponse (..),
    mkDescribeUserResponse,

    -- ** Response lenses
    dursUser,
    dursResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeUser' smart constructor.
data DescribeUser = DescribeUser'
  { userId :: Lude.Text,
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

-- | Creates a value of 'DescribeUser' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'userId' - The identifier of the user account.
mkDescribeUser ::
  -- | 'userId'
  Lude.Text ->
  -- | 'instanceId'
  Lude.Text ->
  DescribeUser
mkDescribeUser pUserId_ pInstanceId_ =
  DescribeUser' {userId = pUserId_, instanceId = pInstanceId_}

-- | The identifier of the user account.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duUserId :: Lens.Lens' DescribeUser Lude.Text
duUserId = Lens.lens (userId :: DescribeUser -> Lude.Text) (\s a -> s {userId = a} :: DescribeUser)
{-# DEPRECATED duUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duInstanceId :: Lens.Lens' DescribeUser Lude.Text
duInstanceId = Lens.lens (instanceId :: DescribeUser -> Lude.Text) (\s a -> s {instanceId = a} :: DescribeUser)
{-# DEPRECATED duInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest DescribeUser where
  type Rs DescribeUser = DescribeUserResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeUserResponse'
            Lude.<$> (x Lude..?> "User") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeUser where
  toPath DescribeUser' {..} =
    Lude.mconcat
      ["/users/", Lude.toBS instanceId, "/", Lude.toBS userId]

instance Lude.ToQuery DescribeUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeUserResponse' smart constructor.
data DescribeUserResponse = DescribeUserResponse'
  { user ::
      Lude.Maybe User,
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
-- * 'responseStatus' - The response status code.
-- * 'user' - Information about the user account and configuration settings.
mkDescribeUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeUserResponse
mkDescribeUserResponse pResponseStatus_ =
  DescribeUserResponse'
    { user = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the user account and configuration settings.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursUser :: Lens.Lens' DescribeUserResponse (Lude.Maybe User)
dursUser = Lens.lens (user :: DescribeUserResponse -> Lude.Maybe User) (\s a -> s {user = a} :: DescribeUserResponse)
{-# DEPRECATED dursUser "Use generic-lens or generic-optics with 'user' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursResponseStatus :: Lens.Lens' DescribeUserResponse Lude.Int
dursResponseStatus = Lens.lens (responseStatus :: DescribeUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeUserResponse)
{-# DEPRECATED dursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
