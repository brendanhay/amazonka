{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminListDevices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists devices, as an administrator.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminListDevices
  ( -- * Creating a request
    AdminListDevices (..),
    mkAdminListDevices,

    -- ** Request lenses
    aldPaginationToken,
    aldUserPoolId,
    aldUsername,
    aldLimit,

    -- * Destructuring the response
    AdminListDevicesResponse (..),
    mkAdminListDevicesResponse,

    -- ** Response lenses
    aldrsPaginationToken,
    aldrsDevices,
    aldrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to list devices, as an administrator.
--
-- /See:/ 'mkAdminListDevices' smart constructor.
data AdminListDevices = AdminListDevices'
  { -- | The pagination token.
    paginationToken :: Lude.Maybe Lude.Text,
    -- | The user pool ID.
    userPoolId :: Lude.Text,
    -- | The user name.
    username :: Lude.Sensitive Lude.Text,
    -- | The limit of the devices request.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminListDevices' with the minimum fields required to make a request.
--
-- * 'paginationToken' - The pagination token.
-- * 'userPoolId' - The user pool ID.
-- * 'username' - The user name.
-- * 'limit' - The limit of the devices request.
mkAdminListDevices ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  AdminListDevices
mkAdminListDevices pUserPoolId_ pUsername_ =
  AdminListDevices'
    { paginationToken = Lude.Nothing,
      userPoolId = pUserPoolId_,
      username = pUsername_,
      limit = Lude.Nothing
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldPaginationToken :: Lens.Lens' AdminListDevices (Lude.Maybe Lude.Text)
aldPaginationToken = Lens.lens (paginationToken :: AdminListDevices -> Lude.Maybe Lude.Text) (\s a -> s {paginationToken = a} :: AdminListDevices)
{-# DEPRECATED aldPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldUserPoolId :: Lens.Lens' AdminListDevices Lude.Text
aldUserPoolId = Lens.lens (userPoolId :: AdminListDevices -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminListDevices)
{-# DEPRECATED aldUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldUsername :: Lens.Lens' AdminListDevices (Lude.Sensitive Lude.Text)
aldUsername = Lens.lens (username :: AdminListDevices -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminListDevices)
{-# DEPRECATED aldUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The limit of the devices request.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldLimit :: Lens.Lens' AdminListDevices (Lude.Maybe Lude.Natural)
aldLimit = Lens.lens (limit :: AdminListDevices -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: AdminListDevices)
{-# DEPRECATED aldLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest AdminListDevices where
  type Rs AdminListDevices = AdminListDevicesResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          AdminListDevicesResponse'
            Lude.<$> (x Lude..?> "PaginationToken")
            Lude.<*> (x Lude..?> "Devices" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AdminListDevices where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminListDevices" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminListDevices where
  toJSON AdminListDevices' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PaginationToken" Lude..=) Lude.<$> paginationToken,
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Username" Lude..= username),
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath AdminListDevices where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminListDevices where
  toQuery = Lude.const Lude.mempty

-- | Lists the device's response, as an administrator.
--
-- /See:/ 'mkAdminListDevicesResponse' smart constructor.
data AdminListDevicesResponse = AdminListDevicesResponse'
  { -- | The pagination token.
    paginationToken :: Lude.Maybe Lude.Text,
    -- | The devices in the list of devices response.
    devices :: Lude.Maybe [DeviceType],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminListDevicesResponse' with the minimum fields required to make a request.
--
-- * 'paginationToken' - The pagination token.
-- * 'devices' - The devices in the list of devices response.
-- * 'responseStatus' - The response status code.
mkAdminListDevicesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AdminListDevicesResponse
mkAdminListDevicesResponse pResponseStatus_ =
  AdminListDevicesResponse'
    { paginationToken = Lude.Nothing,
      devices = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldrsPaginationToken :: Lens.Lens' AdminListDevicesResponse (Lude.Maybe Lude.Text)
aldrsPaginationToken = Lens.lens (paginationToken :: AdminListDevicesResponse -> Lude.Maybe Lude.Text) (\s a -> s {paginationToken = a} :: AdminListDevicesResponse)
{-# DEPRECATED aldrsPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

-- | The devices in the list of devices response.
--
-- /Note:/ Consider using 'devices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldrsDevices :: Lens.Lens' AdminListDevicesResponse (Lude.Maybe [DeviceType])
aldrsDevices = Lens.lens (devices :: AdminListDevicesResponse -> Lude.Maybe [DeviceType]) (\s a -> s {devices = a} :: AdminListDevicesResponse)
{-# DEPRECATED aldrsDevices "Use generic-lens or generic-optics with 'devices' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldrsResponseStatus :: Lens.Lens' AdminListDevicesResponse Lude.Int
aldrsResponseStatus = Lens.lens (responseStatus :: AdminListDevicesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminListDevicesResponse)
{-# DEPRECATED aldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
