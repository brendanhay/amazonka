{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.UpdateRadius
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Remote Authentication Dial In User Service (RADIUS) server information for an AD Connector or Microsoft AD directory.
module Network.AWS.DirectoryService.UpdateRadius
  ( -- * Creating a request
    UpdateRadius (..),
    mkUpdateRadius,

    -- ** Request lenses
    urDirectoryId,
    urRadiusSettings,

    -- * Destructuring the response
    UpdateRadiusResponse (..),
    mkUpdateRadiusResponse,

    -- ** Response lenses
    urrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'UpdateRadius' operation.
--
-- /See:/ 'mkUpdateRadius' smart constructor.
data UpdateRadius = UpdateRadius'
  { directoryId :: Lude.Text,
    radiusSettings :: RadiusSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRadius' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory for which to update the RADIUS server information.
-- * 'radiusSettings' - A 'RadiusSettings' object that contains information about the RADIUS server.
mkUpdateRadius ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'radiusSettings'
  RadiusSettings ->
  UpdateRadius
mkUpdateRadius pDirectoryId_ pRadiusSettings_ =
  UpdateRadius'
    { directoryId = pDirectoryId_,
      radiusSettings = pRadiusSettings_
    }

-- | The identifier of the directory for which to update the RADIUS server information.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urDirectoryId :: Lens.Lens' UpdateRadius Lude.Text
urDirectoryId = Lens.lens (directoryId :: UpdateRadius -> Lude.Text) (\s a -> s {directoryId = a} :: UpdateRadius)
{-# DEPRECATED urDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | A 'RadiusSettings' object that contains information about the RADIUS server.
--
-- /Note:/ Consider using 'radiusSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urRadiusSettings :: Lens.Lens' UpdateRadius RadiusSettings
urRadiusSettings = Lens.lens (radiusSettings :: UpdateRadius -> RadiusSettings) (\s a -> s {radiusSettings = a} :: UpdateRadius)
{-# DEPRECATED urRadiusSettings "Use generic-lens or generic-optics with 'radiusSettings' instead." #-}

instance Lude.AWSRequest UpdateRadius where
  type Rs UpdateRadius = UpdateRadiusResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateRadiusResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateRadius where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.UpdateRadius" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateRadius where
  toJSON UpdateRadius' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("RadiusSettings" Lude..= radiusSettings)
          ]
      )

instance Lude.ToPath UpdateRadius where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateRadius where
  toQuery = Lude.const Lude.mempty

-- | Contains the results of the 'UpdateRadius' operation.
--
-- /See:/ 'mkUpdateRadiusResponse' smart constructor.
newtype UpdateRadiusResponse = UpdateRadiusResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRadiusResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateRadiusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateRadiusResponse
mkUpdateRadiusResponse pResponseStatus_ =
  UpdateRadiusResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsResponseStatus :: Lens.Lens' UpdateRadiusResponse Lude.Int
urrsResponseStatus = Lens.lens (responseStatus :: UpdateRadiusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateRadiusResponse)
{-# DEPRECATED urrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
