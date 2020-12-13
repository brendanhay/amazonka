{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetConsoleScreenshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a JPG-format screenshot of a running instance to help with troubleshooting.
--
-- The returned content is Base64-encoded.
module Network.AWS.EC2.GetConsoleScreenshot
  ( -- * Creating a request
    GetConsoleScreenshot (..),
    mkGetConsoleScreenshot,

    -- ** Request lenses
    gcsInstanceId,
    gcsWakeUp,
    gcsDryRun,

    -- * Destructuring the response
    GetConsoleScreenshotResponse (..),
    mkGetConsoleScreenshotResponse,

    -- ** Response lenses
    gcsrsInstanceId,
    gcsrsImageData,
    gcsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetConsoleScreenshot' smart constructor.
data GetConsoleScreenshot = GetConsoleScreenshot'
  { -- | The ID of the instance.
    instanceId :: Lude.Text,
    -- | When set to @true@ , acts as keystroke input and wakes up an instance that's in standby or "sleep" mode.
    wakeUp :: Lude.Maybe Lude.Bool,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConsoleScreenshot' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'wakeUp' - When set to @true@ , acts as keystroke input and wakes up an instance that's in standby or "sleep" mode.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkGetConsoleScreenshot ::
  -- | 'instanceId'
  Lude.Text ->
  GetConsoleScreenshot
mkGetConsoleScreenshot pInstanceId_ =
  GetConsoleScreenshot'
    { instanceId = pInstanceId_,
      wakeUp = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsInstanceId :: Lens.Lens' GetConsoleScreenshot Lude.Text
gcsInstanceId = Lens.lens (instanceId :: GetConsoleScreenshot -> Lude.Text) (\s a -> s {instanceId = a} :: GetConsoleScreenshot)
{-# DEPRECATED gcsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | When set to @true@ , acts as keystroke input and wakes up an instance that's in standby or "sleep" mode.
--
-- /Note:/ Consider using 'wakeUp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsWakeUp :: Lens.Lens' GetConsoleScreenshot (Lude.Maybe Lude.Bool)
gcsWakeUp = Lens.lens (wakeUp :: GetConsoleScreenshot -> Lude.Maybe Lude.Bool) (\s a -> s {wakeUp = a} :: GetConsoleScreenshot)
{-# DEPRECATED gcsWakeUp "Use generic-lens or generic-optics with 'wakeUp' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsDryRun :: Lens.Lens' GetConsoleScreenshot (Lude.Maybe Lude.Bool)
gcsDryRun = Lens.lens (dryRun :: GetConsoleScreenshot -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: GetConsoleScreenshot)
{-# DEPRECATED gcsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest GetConsoleScreenshot where
  type Rs GetConsoleScreenshot = GetConsoleScreenshotResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetConsoleScreenshotResponse'
            Lude.<$> (x Lude..@? "instanceId")
            Lude.<*> (x Lude..@? "imageData")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetConsoleScreenshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetConsoleScreenshot where
  toPath = Lude.const "/"

instance Lude.ToQuery GetConsoleScreenshot where
  toQuery GetConsoleScreenshot' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetConsoleScreenshot" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "InstanceId" Lude.=: instanceId,
        "WakeUp" Lude.=: wakeUp,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkGetConsoleScreenshotResponse' smart constructor.
data GetConsoleScreenshotResponse = GetConsoleScreenshotResponse'
  { -- | The ID of the instance.
    instanceId :: Lude.Maybe Lude.Text,
    -- | The data that comprises the image.
    imageData :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConsoleScreenshotResponse' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'imageData' - The data that comprises the image.
-- * 'responseStatus' - The response status code.
mkGetConsoleScreenshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetConsoleScreenshotResponse
mkGetConsoleScreenshotResponse pResponseStatus_ =
  GetConsoleScreenshotResponse'
    { instanceId = Lude.Nothing,
      imageData = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrsInstanceId :: Lens.Lens' GetConsoleScreenshotResponse (Lude.Maybe Lude.Text)
gcsrsInstanceId = Lens.lens (instanceId :: GetConsoleScreenshotResponse -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: GetConsoleScreenshotResponse)
{-# DEPRECATED gcsrsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The data that comprises the image.
--
-- /Note:/ Consider using 'imageData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrsImageData :: Lens.Lens' GetConsoleScreenshotResponse (Lude.Maybe Lude.Text)
gcsrsImageData = Lens.lens (imageData :: GetConsoleScreenshotResponse -> Lude.Maybe Lude.Text) (\s a -> s {imageData = a} :: GetConsoleScreenshotResponse)
{-# DEPRECATED gcsrsImageData "Use generic-lens or generic-optics with 'imageData' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrsResponseStatus :: Lens.Lens' GetConsoleScreenshotResponse Lude.Int
gcsrsResponseStatus = Lens.lens (responseStatus :: GetConsoleScreenshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetConsoleScreenshotResponse)
{-# DEPRECATED gcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
