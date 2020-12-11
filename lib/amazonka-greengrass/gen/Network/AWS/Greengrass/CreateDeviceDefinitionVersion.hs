{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateDeviceDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a device definition that has already been defined.
module Network.AWS.Greengrass.CreateDeviceDefinitionVersion
  ( -- * Creating a request
    CreateDeviceDefinitionVersion (..),
    mkCreateDeviceDefinitionVersion,

    -- ** Request lenses
    cddvAmznClientToken,
    cddvDevices,
    cddvDeviceDefinitionId,

    -- * Destructuring the response
    CreateDeviceDefinitionVersionResponse (..),
    mkCreateDeviceDefinitionVersionResponse,

    -- ** Response lenses
    cddvrsARN,
    cddvrsCreationTimestamp,
    cddvrsVersion,
    cddvrsId,
    cddvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDeviceDefinitionVersion' smart constructor.
data CreateDeviceDefinitionVersion = CreateDeviceDefinitionVersion'
  { amznClientToken ::
      Lude.Maybe Lude.Text,
    devices :: Lude.Maybe [Device],
    deviceDefinitionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDeviceDefinitionVersion' with the minimum fields required to make a request.
--
-- * 'amznClientToken' - A client token used to correlate requests and responses.
-- * 'deviceDefinitionId' - The ID of the device definition.
-- * 'devices' - A list of devices in the definition version.
mkCreateDeviceDefinitionVersion ::
  -- | 'deviceDefinitionId'
  Lude.Text ->
  CreateDeviceDefinitionVersion
mkCreateDeviceDefinitionVersion pDeviceDefinitionId_ =
  CreateDeviceDefinitionVersion'
    { amznClientToken = Lude.Nothing,
      devices = Lude.Nothing,
      deviceDefinitionId = pDeviceDefinitionId_
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddvAmznClientToken :: Lens.Lens' CreateDeviceDefinitionVersion (Lude.Maybe Lude.Text)
cddvAmznClientToken = Lens.lens (amznClientToken :: CreateDeviceDefinitionVersion -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: CreateDeviceDefinitionVersion)
{-# DEPRECATED cddvAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | A list of devices in the definition version.
--
-- /Note:/ Consider using 'devices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddvDevices :: Lens.Lens' CreateDeviceDefinitionVersion (Lude.Maybe [Device])
cddvDevices = Lens.lens (devices :: CreateDeviceDefinitionVersion -> Lude.Maybe [Device]) (\s a -> s {devices = a} :: CreateDeviceDefinitionVersion)
{-# DEPRECATED cddvDevices "Use generic-lens or generic-optics with 'devices' instead." #-}

-- | The ID of the device definition.
--
-- /Note:/ Consider using 'deviceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddvDeviceDefinitionId :: Lens.Lens' CreateDeviceDefinitionVersion Lude.Text
cddvDeviceDefinitionId = Lens.lens (deviceDefinitionId :: CreateDeviceDefinitionVersion -> Lude.Text) (\s a -> s {deviceDefinitionId = a} :: CreateDeviceDefinitionVersion)
{-# DEPRECATED cddvDeviceDefinitionId "Use generic-lens or generic-optics with 'deviceDefinitionId' instead." #-}

instance Lude.AWSRequest CreateDeviceDefinitionVersion where
  type
    Rs CreateDeviceDefinitionVersion =
      CreateDeviceDefinitionVersionResponse
  request = Req.postJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDeviceDefinitionVersionResponse'
            Lude.<$> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Version")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDeviceDefinitionVersion where
  toHeaders CreateDeviceDefinitionVersion' {..} =
    Lude.mconcat
      [ "X-Amzn-Client-Token" Lude.=# amznClientToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateDeviceDefinitionVersion where
  toJSON CreateDeviceDefinitionVersion' {..} =
    Lude.object
      (Lude.catMaybes [("Devices" Lude..=) Lude.<$> devices])

instance Lude.ToPath CreateDeviceDefinitionVersion where
  toPath CreateDeviceDefinitionVersion' {..} =
    Lude.mconcat
      [ "/greengrass/definition/devices/",
        Lude.toBS deviceDefinitionId,
        "/versions"
      ]

instance Lude.ToQuery CreateDeviceDefinitionVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDeviceDefinitionVersionResponse' smart constructor.
data CreateDeviceDefinitionVersionResponse = CreateDeviceDefinitionVersionResponse'
  { arn ::
      Lude.Maybe
        Lude.Text,
    creationTimestamp ::
      Lude.Maybe
        Lude.Text,
    version ::
      Lude.Maybe
        Lude.Text,
    id ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDeviceDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the version.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
-- * 'id' - The ID of the parent definition that the version is associated with.
-- * 'responseStatus' - The response status code.
-- * 'version' - The ID of the version.
mkCreateDeviceDefinitionVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDeviceDefinitionVersionResponse
mkCreateDeviceDefinitionVersionResponse pResponseStatus_ =
  CreateDeviceDefinitionVersionResponse'
    { arn = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddvrsARN :: Lens.Lens' CreateDeviceDefinitionVersionResponse (Lude.Maybe Lude.Text)
cddvrsARN = Lens.lens (arn :: CreateDeviceDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateDeviceDefinitionVersionResponse)
{-# DEPRECATED cddvrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddvrsCreationTimestamp :: Lens.Lens' CreateDeviceDefinitionVersionResponse (Lude.Maybe Lude.Text)
cddvrsCreationTimestamp = Lens.lens (creationTimestamp :: CreateDeviceDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: CreateDeviceDefinitionVersionResponse)
{-# DEPRECATED cddvrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddvrsVersion :: Lens.Lens' CreateDeviceDefinitionVersionResponse (Lude.Maybe Lude.Text)
cddvrsVersion = Lens.lens (version :: CreateDeviceDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: CreateDeviceDefinitionVersionResponse)
{-# DEPRECATED cddvrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The ID of the parent definition that the version is associated with.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddvrsId :: Lens.Lens' CreateDeviceDefinitionVersionResponse (Lude.Maybe Lude.Text)
cddvrsId = Lens.lens (id :: CreateDeviceDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateDeviceDefinitionVersionResponse)
{-# DEPRECATED cddvrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddvrsResponseStatus :: Lens.Lens' CreateDeviceDefinitionVersionResponse Lude.Int
cddvrsResponseStatus = Lens.lens (responseStatus :: CreateDeviceDefinitionVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDeviceDefinitionVersionResponse)
{-# DEPRECATED cddvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
