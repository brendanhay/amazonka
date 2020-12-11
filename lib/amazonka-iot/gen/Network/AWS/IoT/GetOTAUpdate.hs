{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetOTAUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an OTA update.
module Network.AWS.IoT.GetOTAUpdate
  ( -- * Creating a request
    GetOTAUpdate (..),
    mkGetOTAUpdate,

    -- ** Request lenses
    gotauOtaUpdateId,

    -- * Destructuring the response
    GetOTAUpdateResponse (..),
    mkGetOTAUpdateResponse,

    -- ** Response lenses
    gotaursOtaUpdateInfo,
    gotaursResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetOTAUpdate' smart constructor.
newtype GetOTAUpdate = GetOTAUpdate' {otaUpdateId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOTAUpdate' with the minimum fields required to make a request.
--
-- * 'otaUpdateId' - The OTA update ID.
mkGetOTAUpdate ::
  -- | 'otaUpdateId'
  Lude.Text ->
  GetOTAUpdate
mkGetOTAUpdate pOtaUpdateId_ =
  GetOTAUpdate' {otaUpdateId = pOtaUpdateId_}

-- | The OTA update ID.
--
-- /Note:/ Consider using 'otaUpdateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gotauOtaUpdateId :: Lens.Lens' GetOTAUpdate Lude.Text
gotauOtaUpdateId = Lens.lens (otaUpdateId :: GetOTAUpdate -> Lude.Text) (\s a -> s {otaUpdateId = a} :: GetOTAUpdate)
{-# DEPRECATED gotauOtaUpdateId "Use generic-lens or generic-optics with 'otaUpdateId' instead." #-}

instance Lude.AWSRequest GetOTAUpdate where
  type Rs GetOTAUpdate = GetOTAUpdateResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetOTAUpdateResponse'
            Lude.<$> (x Lude..?> "otaUpdateInfo")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetOTAUpdate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetOTAUpdate where
  toPath GetOTAUpdate' {..} =
    Lude.mconcat ["/otaUpdates/", Lude.toBS otaUpdateId]

instance Lude.ToQuery GetOTAUpdate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetOTAUpdateResponse' smart constructor.
data GetOTAUpdateResponse = GetOTAUpdateResponse'
  { otaUpdateInfo ::
      Lude.Maybe OTAUpdateInfo,
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

-- | Creates a value of 'GetOTAUpdateResponse' with the minimum fields required to make a request.
--
-- * 'otaUpdateInfo' - The OTA update info.
-- * 'responseStatus' - The response status code.
mkGetOTAUpdateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetOTAUpdateResponse
mkGetOTAUpdateResponse pResponseStatus_ =
  GetOTAUpdateResponse'
    { otaUpdateInfo = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The OTA update info.
--
-- /Note:/ Consider using 'otaUpdateInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gotaursOtaUpdateInfo :: Lens.Lens' GetOTAUpdateResponse (Lude.Maybe OTAUpdateInfo)
gotaursOtaUpdateInfo = Lens.lens (otaUpdateInfo :: GetOTAUpdateResponse -> Lude.Maybe OTAUpdateInfo) (\s a -> s {otaUpdateInfo = a} :: GetOTAUpdateResponse)
{-# DEPRECATED gotaursOtaUpdateInfo "Use generic-lens or generic-optics with 'otaUpdateInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gotaursResponseStatus :: Lens.Lens' GetOTAUpdateResponse Lude.Int
gotaursResponseStatus = Lens.lens (responseStatus :: GetOTAUpdateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetOTAUpdateResponse)
{-# DEPRECATED gotaursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
