{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.GetSnowballUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the Snow Family service limit for your account, and also the number of Snow devices your account has in use.
--
-- The default service limit for the number of Snow devices that you can have at one time is 1. If you want to increase your service limit, contact AWS Support.
module Network.AWS.Snowball.GetSnowballUsage
  ( -- * Creating a request
    GetSnowballUsage (..),
    mkGetSnowballUsage,

    -- * Destructuring the response
    GetSnowballUsageResponse (..),
    mkGetSnowballUsageResponse,

    -- ** Response lenses
    grsSnowballsInUse,
    grsSnowballLimit,
    grsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkGetSnowballUsage' smart constructor.
data GetSnowballUsage = GetSnowballUsage'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSnowballUsage' with the minimum fields required to make a request.
mkGetSnowballUsage ::
  GetSnowballUsage
mkGetSnowballUsage = GetSnowballUsage'

instance Lude.AWSRequest GetSnowballUsage where
  type Rs GetSnowballUsage = GetSnowballUsageResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSnowballUsageResponse'
            Lude.<$> (x Lude..?> "SnowballsInUse")
            Lude.<*> (x Lude..?> "SnowballLimit")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSnowballUsage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSIESnowballJobManagementService.GetSnowballUsage" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSnowballUsage where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath GetSnowballUsage where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSnowballUsage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSnowballUsageResponse' smart constructor.
data GetSnowballUsageResponse = GetSnowballUsageResponse'
  { snowballsInUse ::
      Lude.Maybe Lude.Int,
    snowballLimit :: Lude.Maybe Lude.Int,
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

-- | Creates a value of 'GetSnowballUsageResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'snowballLimit' - The service limit for number of Snow devices this account can have at once. The default service limit is 1 (one).
-- * 'snowballsInUse' - The number of Snow devices that this account is currently using.
mkGetSnowballUsageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSnowballUsageResponse
mkGetSnowballUsageResponse pResponseStatus_ =
  GetSnowballUsageResponse'
    { snowballsInUse = Lude.Nothing,
      snowballLimit = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The number of Snow devices that this account is currently using.
--
-- /Note:/ Consider using 'snowballsInUse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsSnowballsInUse :: Lens.Lens' GetSnowballUsageResponse (Lude.Maybe Lude.Int)
grsSnowballsInUse = Lens.lens (snowballsInUse :: GetSnowballUsageResponse -> Lude.Maybe Lude.Int) (\s a -> s {snowballsInUse = a} :: GetSnowballUsageResponse)
{-# DEPRECATED grsSnowballsInUse "Use generic-lens or generic-optics with 'snowballsInUse' instead." #-}

-- | The service limit for number of Snow devices this account can have at once. The default service limit is 1 (one).
--
-- /Note:/ Consider using 'snowballLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsSnowballLimit :: Lens.Lens' GetSnowballUsageResponse (Lude.Maybe Lude.Int)
grsSnowballLimit = Lens.lens (snowballLimit :: GetSnowballUsageResponse -> Lude.Maybe Lude.Int) (\s a -> s {snowballLimit = a} :: GetSnowballUsageResponse)
{-# DEPRECATED grsSnowballLimit "Use generic-lens or generic-optics with 'snowballLimit' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetSnowballUsageResponse Lude.Int
grsResponseStatus = Lens.lens (responseStatus :: GetSnowballUsageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSnowballUsageResponse)
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
