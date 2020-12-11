{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateConnectivityInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the connectivity information for the core. Any devices that belong to the group which has this core will receive this information in order to find the location of the core and connect to it.
module Network.AWS.Greengrass.UpdateConnectivityInfo
  ( -- * Creating a request
    UpdateConnectivityInfo (..),
    mkUpdateConnectivityInfo,

    -- ** Request lenses
    uciConnectivityInfo,
    uciThingName,

    -- * Destructuring the response
    UpdateConnectivityInfoResponse (..),
    mkUpdateConnectivityInfoResponse,

    -- ** Response lenses
    ucirsVersion,
    ucirsMessage,
    ucirsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Connectivity information.
--
-- /See:/ 'mkUpdateConnectivityInfo' smart constructor.
data UpdateConnectivityInfo = UpdateConnectivityInfo'
  { connectivityInfo ::
      Lude.Maybe [ConnectivityInfo],
    thingName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateConnectivityInfo' with the minimum fields required to make a request.
--
-- * 'connectivityInfo' - A list of connectivity info.
-- * 'thingName' - The thing name.
mkUpdateConnectivityInfo ::
  -- | 'thingName'
  Lude.Text ->
  UpdateConnectivityInfo
mkUpdateConnectivityInfo pThingName_ =
  UpdateConnectivityInfo'
    { connectivityInfo = Lude.Nothing,
      thingName = pThingName_
    }

-- | A list of connectivity info.
--
-- /Note:/ Consider using 'connectivityInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uciConnectivityInfo :: Lens.Lens' UpdateConnectivityInfo (Lude.Maybe [ConnectivityInfo])
uciConnectivityInfo = Lens.lens (connectivityInfo :: UpdateConnectivityInfo -> Lude.Maybe [ConnectivityInfo]) (\s a -> s {connectivityInfo = a} :: UpdateConnectivityInfo)
{-# DEPRECATED uciConnectivityInfo "Use generic-lens or generic-optics with 'connectivityInfo' instead." #-}

-- | The thing name.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uciThingName :: Lens.Lens' UpdateConnectivityInfo Lude.Text
uciThingName = Lens.lens (thingName :: UpdateConnectivityInfo -> Lude.Text) (\s a -> s {thingName = a} :: UpdateConnectivityInfo)
{-# DEPRECATED uciThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest UpdateConnectivityInfo where
  type Rs UpdateConnectivityInfo = UpdateConnectivityInfoResponse
  request = Req.putJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateConnectivityInfoResponse'
            Lude.<$> (x Lude..?> "Version")
            Lude.<*> (x Lude..?> "message")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateConnectivityInfo where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateConnectivityInfo where
  toJSON UpdateConnectivityInfo' {..} =
    Lude.object
      ( Lude.catMaybes
          [("ConnectivityInfo" Lude..=) Lude.<$> connectivityInfo]
      )

instance Lude.ToPath UpdateConnectivityInfo where
  toPath UpdateConnectivityInfo' {..} =
    Lude.mconcat
      ["/greengrass/things/", Lude.toBS thingName, "/connectivityInfo"]

instance Lude.ToQuery UpdateConnectivityInfo where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateConnectivityInfoResponse' smart constructor.
data UpdateConnectivityInfoResponse = UpdateConnectivityInfoResponse'
  { version ::
      Lude.Maybe Lude.Text,
    message ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'UpdateConnectivityInfoResponse' with the minimum fields required to make a request.
--
-- * 'message' - A message about the connectivity info update request.
-- * 'responseStatus' - The response status code.
-- * 'version' - The new version of the connectivity info.
mkUpdateConnectivityInfoResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateConnectivityInfoResponse
mkUpdateConnectivityInfoResponse pResponseStatus_ =
  UpdateConnectivityInfoResponse'
    { version = Lude.Nothing,
      message = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The new version of the connectivity info.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucirsVersion :: Lens.Lens' UpdateConnectivityInfoResponse (Lude.Maybe Lude.Text)
ucirsVersion = Lens.lens (version :: UpdateConnectivityInfoResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: UpdateConnectivityInfoResponse)
{-# DEPRECATED ucirsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | A message about the connectivity info update request.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucirsMessage :: Lens.Lens' UpdateConnectivityInfoResponse (Lude.Maybe Lude.Text)
ucirsMessage = Lens.lens (message :: UpdateConnectivityInfoResponse -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: UpdateConnectivityInfoResponse)
{-# DEPRECATED ucirsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucirsResponseStatus :: Lens.Lens' UpdateConnectivityInfoResponse Lude.Int
ucirsResponseStatus = Lens.lens (responseStatus :: UpdateConnectivityInfoResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateConnectivityInfoResponse)
{-# DEPRECATED ucirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
