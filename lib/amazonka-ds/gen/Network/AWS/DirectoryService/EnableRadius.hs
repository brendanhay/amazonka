{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.EnableRadius
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables multi-factor authentication (MFA) with the Remote Authentication Dial In User Service (RADIUS) server for an AD Connector or Microsoft AD directory.
module Network.AWS.DirectoryService.EnableRadius
  ( -- * Creating a request
    EnableRadius (..),
    mkEnableRadius,

    -- ** Request lenses
    erDirectoryId,
    erRadiusSettings,

    -- * Destructuring the response
    EnableRadiusResponse (..),
    mkEnableRadiusResponse,

    -- ** Response lenses
    errsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'EnableRadius' operation.
--
-- /See:/ 'mkEnableRadius' smart constructor.
data EnableRadius = EnableRadius'
  { -- | The identifier of the directory for which to enable MFA.
    directoryId :: Lude.Text,
    -- | A 'RadiusSettings' object that contains information about the RADIUS server.
    radiusSettings :: RadiusSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableRadius' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory for which to enable MFA.
-- * 'radiusSettings' - A 'RadiusSettings' object that contains information about the RADIUS server.
mkEnableRadius ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'radiusSettings'
  RadiusSettings ->
  EnableRadius
mkEnableRadius pDirectoryId_ pRadiusSettings_ =
  EnableRadius'
    { directoryId = pDirectoryId_,
      radiusSettings = pRadiusSettings_
    }

-- | The identifier of the directory for which to enable MFA.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erDirectoryId :: Lens.Lens' EnableRadius Lude.Text
erDirectoryId = Lens.lens (directoryId :: EnableRadius -> Lude.Text) (\s a -> s {directoryId = a} :: EnableRadius)
{-# DEPRECATED erDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | A 'RadiusSettings' object that contains information about the RADIUS server.
--
-- /Note:/ Consider using 'radiusSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erRadiusSettings :: Lens.Lens' EnableRadius RadiusSettings
erRadiusSettings = Lens.lens (radiusSettings :: EnableRadius -> RadiusSettings) (\s a -> s {radiusSettings = a} :: EnableRadius)
{-# DEPRECATED erRadiusSettings "Use generic-lens or generic-optics with 'radiusSettings' instead." #-}

instance Lude.AWSRequest EnableRadius where
  type Rs EnableRadius = EnableRadiusResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          EnableRadiusResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnableRadius where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.EnableRadius" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EnableRadius where
  toJSON EnableRadius' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("RadiusSettings" Lude..= radiusSettings)
          ]
      )

instance Lude.ToPath EnableRadius where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableRadius where
  toQuery = Lude.const Lude.mempty

-- | Contains the results of the 'EnableRadius' operation.
--
-- /See:/ 'mkEnableRadiusResponse' smart constructor.
newtype EnableRadiusResponse = EnableRadiusResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableRadiusResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkEnableRadiusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EnableRadiusResponse
mkEnableRadiusResponse pResponseStatus_ =
  EnableRadiusResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
errsResponseStatus :: Lens.Lens' EnableRadiusResponse Lude.Int
errsResponseStatus = Lens.lens (responseStatus :: EnableRadiusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnableRadiusResponse)
{-# DEPRECATED errsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
