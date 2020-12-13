{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetAccountSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details about your account's <https://docs.aws.amazon.com/lambda/latest/dg/limits.html limits> and usage in an AWS Region.
module Network.AWS.Lambda.GetAccountSettings
  ( -- * Creating a request
    GetAccountSettings (..),
    mkGetAccountSettings,

    -- * Destructuring the response
    GetAccountSettingsResponse (..),
    mkGetAccountSettingsResponse,

    -- ** Response lenses
    gasrsAccountLimit,
    gasrsAccountUsage,
    gasrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAccountSettings' smart constructor.
data GetAccountSettings = GetAccountSettings'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAccountSettings' with the minimum fields required to make a request.
mkGetAccountSettings ::
  GetAccountSettings
mkGetAccountSettings = GetAccountSettings'

instance Lude.AWSRequest GetAccountSettings where
  type Rs GetAccountSettings = GetAccountSettingsResponse
  request = Req.get lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAccountSettingsResponse'
            Lude.<$> (x Lude..?> "AccountLimit")
            Lude.<*> (x Lude..?> "AccountUsage")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAccountSettings where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetAccountSettings where
  toPath = Lude.const "/2016-08-19/account-settings/"

instance Lude.ToQuery GetAccountSettings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAccountSettingsResponse' smart constructor.
data GetAccountSettingsResponse = GetAccountSettingsResponse'
  { -- | Limits that are related to concurrency and code storage.
    accountLimit :: Lude.Maybe AccountLimit,
    -- | The number of functions and amount of storage in use.
    accountUsage :: Lude.Maybe AccountUsage,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAccountSettingsResponse' with the minimum fields required to make a request.
--
-- * 'accountLimit' - Limits that are related to concurrency and code storage.
-- * 'accountUsage' - The number of functions and amount of storage in use.
-- * 'responseStatus' - The response status code.
mkGetAccountSettingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAccountSettingsResponse
mkGetAccountSettingsResponse pResponseStatus_ =
  GetAccountSettingsResponse'
    { accountLimit = Lude.Nothing,
      accountUsage = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Limits that are related to concurrency and code storage.
--
-- /Note:/ Consider using 'accountLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrsAccountLimit :: Lens.Lens' GetAccountSettingsResponse (Lude.Maybe AccountLimit)
gasrsAccountLimit = Lens.lens (accountLimit :: GetAccountSettingsResponse -> Lude.Maybe AccountLimit) (\s a -> s {accountLimit = a} :: GetAccountSettingsResponse)
{-# DEPRECATED gasrsAccountLimit "Use generic-lens or generic-optics with 'accountLimit' instead." #-}

-- | The number of functions and amount of storage in use.
--
-- /Note:/ Consider using 'accountUsage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrsAccountUsage :: Lens.Lens' GetAccountSettingsResponse (Lude.Maybe AccountUsage)
gasrsAccountUsage = Lens.lens (accountUsage :: GetAccountSettingsResponse -> Lude.Maybe AccountUsage) (\s a -> s {accountUsage = a} :: GetAccountSettingsResponse)
{-# DEPRECATED gasrsAccountUsage "Use generic-lens or generic-optics with 'accountUsage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrsResponseStatus :: Lens.Lens' GetAccountSettingsResponse Lude.Int
gasrsResponseStatus = Lens.lens (responseStatus :: GetAccountSettingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAccountSettingsResponse)
{-# DEPRECATED gasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
