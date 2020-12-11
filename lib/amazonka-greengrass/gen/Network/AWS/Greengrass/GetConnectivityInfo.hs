{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetConnectivityInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the connectivity information for a core.
module Network.AWS.Greengrass.GetConnectivityInfo
  ( -- * Creating a request
    GetConnectivityInfo (..),
    mkGetConnectivityInfo,

    -- ** Request lenses
    gciThingName,

    -- * Destructuring the response
    GetConnectivityInfoResponse (..),
    mkGetConnectivityInfoResponse,

    -- ** Response lenses
    gcirsMessage,
    gcirsConnectivityInfo,
    gcirsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetConnectivityInfo' smart constructor.
newtype GetConnectivityInfo = GetConnectivityInfo'
  { thingName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConnectivityInfo' with the minimum fields required to make a request.
--
-- * 'thingName' - The thing name.
mkGetConnectivityInfo ::
  -- | 'thingName'
  Lude.Text ->
  GetConnectivityInfo
mkGetConnectivityInfo pThingName_ =
  GetConnectivityInfo' {thingName = pThingName_}

-- | The thing name.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gciThingName :: Lens.Lens' GetConnectivityInfo Lude.Text
gciThingName = Lens.lens (thingName :: GetConnectivityInfo -> Lude.Text) (\s a -> s {thingName = a} :: GetConnectivityInfo)
{-# DEPRECATED gciThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest GetConnectivityInfo where
  type Rs GetConnectivityInfo = GetConnectivityInfoResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetConnectivityInfoResponse'
            Lude.<$> (x Lude..?> "message")
            Lude.<*> (x Lude..?> "ConnectivityInfo" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetConnectivityInfo where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetConnectivityInfo where
  toPath GetConnectivityInfo' {..} =
    Lude.mconcat
      ["/greengrass/things/", Lude.toBS thingName, "/connectivityInfo"]

instance Lude.ToQuery GetConnectivityInfo where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetConnectivityInfoResponse' smart constructor.
data GetConnectivityInfoResponse = GetConnectivityInfoResponse'
  { message ::
      Lude.Maybe Lude.Text,
    connectivityInfo ::
      Lude.Maybe [ConnectivityInfo],
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

-- | Creates a value of 'GetConnectivityInfoResponse' with the minimum fields required to make a request.
--
-- * 'connectivityInfo' - Connectivity info list.
-- * 'message' - A message about the connectivity info request.
-- * 'responseStatus' - The response status code.
mkGetConnectivityInfoResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetConnectivityInfoResponse
mkGetConnectivityInfoResponse pResponseStatus_ =
  GetConnectivityInfoResponse'
    { message = Lude.Nothing,
      connectivityInfo = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A message about the connectivity info request.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsMessage :: Lens.Lens' GetConnectivityInfoResponse (Lude.Maybe Lude.Text)
gcirsMessage = Lens.lens (message :: GetConnectivityInfoResponse -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: GetConnectivityInfoResponse)
{-# DEPRECATED gcirsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | Connectivity info list.
--
-- /Note:/ Consider using 'connectivityInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsConnectivityInfo :: Lens.Lens' GetConnectivityInfoResponse (Lude.Maybe [ConnectivityInfo])
gcirsConnectivityInfo = Lens.lens (connectivityInfo :: GetConnectivityInfoResponse -> Lude.Maybe [ConnectivityInfo]) (\s a -> s {connectivityInfo = a} :: GetConnectivityInfoResponse)
{-# DEPRECATED gcirsConnectivityInfo "Use generic-lens or generic-optics with 'connectivityInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsResponseStatus :: Lens.Lens' GetConnectivityInfoResponse Lude.Int
gcirsResponseStatus = Lens.lens (responseStatus :: GetConnectivityInfoResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetConnectivityInfoResponse)
{-# DEPRECATED gcirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
