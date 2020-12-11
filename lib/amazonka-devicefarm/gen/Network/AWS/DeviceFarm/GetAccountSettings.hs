{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetAccountSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of unmetered iOS or unmetered Android devices that have been purchased by the account.
module Network.AWS.DeviceFarm.GetAccountSettings
  ( -- * Creating a request
    GetAccountSettings (..),
    mkGetAccountSettings,

    -- * Destructuring the response
    GetAccountSettingsResponse (..),
    mkGetAccountSettingsResponse,

    -- ** Response lenses
    gasrsAccountSettings,
    gasrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request sent to retrieve the account settings.
--
-- /See:/ 'mkGetAccountSettings' smart constructor.
data GetAccountSettings = GetAccountSettings'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAccountSettings' with the minimum fields required to make a request.
mkGetAccountSettings ::
  GetAccountSettings
mkGetAccountSettings = GetAccountSettings'

instance Lude.AWSRequest GetAccountSettings where
  type Rs GetAccountSettings = GetAccountSettingsResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAccountSettingsResponse'
            Lude.<$> (x Lude..?> "accountSettings")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAccountSettings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.GetAccountSettings" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAccountSettings where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath GetAccountSettings where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAccountSettings where
  toQuery = Lude.const Lude.mempty

-- | Represents the account settings return values from the @GetAccountSettings@ request.
--
-- /See:/ 'mkGetAccountSettingsResponse' smart constructor.
data GetAccountSettingsResponse = GetAccountSettingsResponse'
  { accountSettings ::
      Lude.Maybe AccountSettings,
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

-- | Creates a value of 'GetAccountSettingsResponse' with the minimum fields required to make a request.
--
-- * 'accountSettings' - The account settings.
-- * 'responseStatus' - The response status code.
mkGetAccountSettingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAccountSettingsResponse
mkGetAccountSettingsResponse pResponseStatus_ =
  GetAccountSettingsResponse'
    { accountSettings = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The account settings.
--
-- /Note:/ Consider using 'accountSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrsAccountSettings :: Lens.Lens' GetAccountSettingsResponse (Lude.Maybe AccountSettings)
gasrsAccountSettings = Lens.lens (accountSettings :: GetAccountSettingsResponse -> Lude.Maybe AccountSettings) (\s a -> s {accountSettings = a} :: GetAccountSettingsResponse)
{-# DEPRECATED gasrsAccountSettings "Use generic-lens or generic-optics with 'accountSettings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrsResponseStatus :: Lens.Lens' GetAccountSettingsResponse Lude.Int
gasrsResponseStatus = Lens.lens (responseStatus :: GetAccountSettingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAccountSettingsResponse)
{-# DEPRECATED gasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
