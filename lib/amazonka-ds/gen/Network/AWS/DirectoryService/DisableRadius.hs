{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DisableRadius
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables multi-factor authentication (MFA) with the Remote Authentication Dial In User Service (RADIUS) server for an AD Connector or Microsoft AD directory.
module Network.AWS.DirectoryService.DisableRadius
  ( -- * Creating a request
    DisableRadius (..),
    mkDisableRadius,

    -- ** Request lenses
    drDirectoryId,

    -- * Destructuring the response
    DisableRadiusResponse (..),
    mkDisableRadiusResponse,

    -- ** Response lenses
    drsrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'DisableRadius' operation.
--
-- /See:/ 'mkDisableRadius' smart constructor.
newtype DisableRadius = DisableRadius'
  { -- | The identifier of the directory for which to disable MFA.
    directoryId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableRadius' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory for which to disable MFA.
mkDisableRadius ::
  -- | 'directoryId'
  Lude.Text ->
  DisableRadius
mkDisableRadius pDirectoryId_ =
  DisableRadius' {directoryId = pDirectoryId_}

-- | The identifier of the directory for which to disable MFA.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drDirectoryId :: Lens.Lens' DisableRadius Lude.Text
drDirectoryId = Lens.lens (directoryId :: DisableRadius -> Lude.Text) (\s a -> s {directoryId = a} :: DisableRadius)
{-# DEPRECATED drDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

instance Lude.AWSRequest DisableRadius where
  type Rs DisableRadius = DisableRadiusResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisableRadiusResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisableRadius where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.DisableRadius" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisableRadius where
  toJSON DisableRadius' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DirectoryId" Lude..= directoryId)])

instance Lude.ToPath DisableRadius where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableRadius where
  toQuery = Lude.const Lude.mempty

-- | Contains the results of the 'DisableRadius' operation.
--
-- /See:/ 'mkDisableRadiusResponse' smart constructor.
newtype DisableRadiusResponse = DisableRadiusResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableRadiusResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisableRadiusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisableRadiusResponse
mkDisableRadiusResponse pResponseStatus_ =
  DisableRadiusResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsrsResponseStatus :: Lens.Lens' DisableRadiusResponse Lude.Int
drsrsResponseStatus = Lens.lens (responseStatus :: DisableRadiusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisableRadiusResponse)
{-# DEPRECATED drsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
