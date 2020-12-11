{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ListDevices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the devices.
module Network.AWS.CognitoIdentityProvider.ListDevices
  ( -- * Creating a request
    ListDevices (..),
    mkListDevices,

    -- ** Request lenses
    ldPaginationToken,
    ldLimit,
    ldAccessToken,

    -- * Destructuring the response
    ListDevicesResponse (..),
    mkListDevicesResponse,

    -- ** Response lenses
    ldrsPaginationToken,
    ldrsDevices,
    ldrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to list the devices.
--
-- /See:/ 'mkListDevices' smart constructor.
data ListDevices = ListDevices'
  { paginationToken ::
      Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    accessToken :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDevices' with the minimum fields required to make a request.
--
-- * 'accessToken' - The access tokens for the request to list devices.
-- * 'limit' - The limit of the device request.
-- * 'paginationToken' - The pagination token for the list request.
mkListDevices ::
  -- | 'accessToken'
  Lude.Sensitive Lude.Text ->
  ListDevices
mkListDevices pAccessToken_ =
  ListDevices'
    { paginationToken = Lude.Nothing,
      limit = Lude.Nothing,
      accessToken = pAccessToken_
    }

-- | The pagination token for the list request.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldPaginationToken :: Lens.Lens' ListDevices (Lude.Maybe Lude.Text)
ldPaginationToken = Lens.lens (paginationToken :: ListDevices -> Lude.Maybe Lude.Text) (\s a -> s {paginationToken = a} :: ListDevices)
{-# DEPRECATED ldPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

-- | The limit of the device request.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldLimit :: Lens.Lens' ListDevices (Lude.Maybe Lude.Natural)
ldLimit = Lens.lens (limit :: ListDevices -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListDevices)
{-# DEPRECATED ldLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The access tokens for the request to list devices.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldAccessToken :: Lens.Lens' ListDevices (Lude.Sensitive Lude.Text)
ldAccessToken = Lens.lens (accessToken :: ListDevices -> Lude.Sensitive Lude.Text) (\s a -> s {accessToken = a} :: ListDevices)
{-# DEPRECATED ldAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

instance Lude.AWSRequest ListDevices where
  type Rs ListDevices = ListDevicesResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDevicesResponse'
            Lude.<$> (x Lude..?> "PaginationToken")
            Lude.<*> (x Lude..?> "Devices" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDevices where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.ListDevices" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDevices where
  toJSON ListDevices' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PaginationToken" Lude..=) Lude.<$> paginationToken,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("AccessToken" Lude..= accessToken)
          ]
      )

instance Lude.ToPath ListDevices where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDevices where
  toQuery = Lude.const Lude.mempty

-- | Represents the response to list devices.
--
-- /See:/ 'mkListDevicesResponse' smart constructor.
data ListDevicesResponse = ListDevicesResponse'
  { paginationToken ::
      Lude.Maybe Lude.Text,
    devices :: Lude.Maybe [DeviceType],
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDevicesResponse' with the minimum fields required to make a request.
--
-- * 'devices' - The devices returned in the list devices response.
-- * 'paginationToken' - The pagination token for the list device response.
-- * 'responseStatus' - The response status code.
mkListDevicesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDevicesResponse
mkListDevicesResponse pResponseStatus_ =
  ListDevicesResponse'
    { paginationToken = Lude.Nothing,
      devices = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token for the list device response.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsPaginationToken :: Lens.Lens' ListDevicesResponse (Lude.Maybe Lude.Text)
ldrsPaginationToken = Lens.lens (paginationToken :: ListDevicesResponse -> Lude.Maybe Lude.Text) (\s a -> s {paginationToken = a} :: ListDevicesResponse)
{-# DEPRECATED ldrsPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

-- | The devices returned in the list devices response.
--
-- /Note:/ Consider using 'devices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsDevices :: Lens.Lens' ListDevicesResponse (Lude.Maybe [DeviceType])
ldrsDevices = Lens.lens (devices :: ListDevicesResponse -> Lude.Maybe [DeviceType]) (\s a -> s {devices = a} :: ListDevicesResponse)
{-# DEPRECATED ldrsDevices "Use generic-lens or generic-optics with 'devices' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsResponseStatus :: Lens.Lens' ListDevicesResponse Lude.Int
ldrsResponseStatus = Lens.lens (responseStatus :: ListDevicesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDevicesResponse)
{-# DEPRECATED ldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
