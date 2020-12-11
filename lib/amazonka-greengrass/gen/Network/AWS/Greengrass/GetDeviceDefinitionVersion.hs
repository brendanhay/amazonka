{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetDeviceDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a device definition version.
module Network.AWS.Greengrass.GetDeviceDefinitionVersion
  ( -- * Creating a request
    GetDeviceDefinitionVersion (..),
    mkGetDeviceDefinitionVersion,

    -- ** Request lenses
    gddvNextToken,
    gddvDeviceDefinitionVersionId,
    gddvDeviceDefinitionId,

    -- * Destructuring the response
    GetDeviceDefinitionVersionResponse (..),
    mkGetDeviceDefinitionVersionResponse,

    -- ** Response lenses
    gddvrsDefinition,
    gddvrsARN,
    gddvrsNextToken,
    gddvrsCreationTimestamp,
    gddvrsVersion,
    gddvrsId,
    gddvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDeviceDefinitionVersion' smart constructor.
data GetDeviceDefinitionVersion = GetDeviceDefinitionVersion'
  { nextToken ::
      Lude.Maybe Lude.Text,
    deviceDefinitionVersionId ::
      Lude.Text,
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

-- | Creates a value of 'GetDeviceDefinitionVersion' with the minimum fields required to make a request.
--
-- * 'deviceDefinitionId' - The ID of the device definition.
-- * 'deviceDefinitionVersionId' - The ID of the device definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListDeviceDefinitionVersions'' requests. If the version is the last one that was associated with a device definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
mkGetDeviceDefinitionVersion ::
  -- | 'deviceDefinitionVersionId'
  Lude.Text ->
  -- | 'deviceDefinitionId'
  Lude.Text ->
  GetDeviceDefinitionVersion
mkGetDeviceDefinitionVersion
  pDeviceDefinitionVersionId_
  pDeviceDefinitionId_ =
    GetDeviceDefinitionVersion'
      { nextToken = Lude.Nothing,
        deviceDefinitionVersionId = pDeviceDefinitionVersionId_,
        deviceDefinitionId = pDeviceDefinitionId_
      }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddvNextToken :: Lens.Lens' GetDeviceDefinitionVersion (Lude.Maybe Lude.Text)
gddvNextToken = Lens.lens (nextToken :: GetDeviceDefinitionVersion -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetDeviceDefinitionVersion)
{-# DEPRECATED gddvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the device definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListDeviceDefinitionVersions'' requests. If the version is the last one that was associated with a device definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
--
-- /Note:/ Consider using 'deviceDefinitionVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddvDeviceDefinitionVersionId :: Lens.Lens' GetDeviceDefinitionVersion Lude.Text
gddvDeviceDefinitionVersionId = Lens.lens (deviceDefinitionVersionId :: GetDeviceDefinitionVersion -> Lude.Text) (\s a -> s {deviceDefinitionVersionId = a} :: GetDeviceDefinitionVersion)
{-# DEPRECATED gddvDeviceDefinitionVersionId "Use generic-lens or generic-optics with 'deviceDefinitionVersionId' instead." #-}

-- | The ID of the device definition.
--
-- /Note:/ Consider using 'deviceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddvDeviceDefinitionId :: Lens.Lens' GetDeviceDefinitionVersion Lude.Text
gddvDeviceDefinitionId = Lens.lens (deviceDefinitionId :: GetDeviceDefinitionVersion -> Lude.Text) (\s a -> s {deviceDefinitionId = a} :: GetDeviceDefinitionVersion)
{-# DEPRECATED gddvDeviceDefinitionId "Use generic-lens or generic-optics with 'deviceDefinitionId' instead." #-}

instance Lude.AWSRequest GetDeviceDefinitionVersion where
  type
    Rs GetDeviceDefinitionVersion =
      GetDeviceDefinitionVersionResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDeviceDefinitionVersionResponse'
            Lude.<$> (x Lude..?> "Definition")
            Lude.<*> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Version")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDeviceDefinitionVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetDeviceDefinitionVersion where
  toPath GetDeviceDefinitionVersion' {..} =
    Lude.mconcat
      [ "/greengrass/definition/devices/",
        Lude.toBS deviceDefinitionId,
        "/versions/",
        Lude.toBS deviceDefinitionVersionId
      ]

instance Lude.ToQuery GetDeviceDefinitionVersion where
  toQuery GetDeviceDefinitionVersion' {..} =
    Lude.mconcat ["NextToken" Lude.=: nextToken]

-- | /See:/ 'mkGetDeviceDefinitionVersionResponse' smart constructor.
data GetDeviceDefinitionVersionResponse = GetDeviceDefinitionVersionResponse'
  { definition ::
      Lude.Maybe
        DeviceDefinitionVersion,
    arn ::
      Lude.Maybe Lude.Text,
    nextToken ::
      Lude.Maybe Lude.Text,
    creationTimestamp ::
      Lude.Maybe Lude.Text,
    version ::
      Lude.Maybe Lude.Text,
    id ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetDeviceDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the device definition version.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the device definition version was created.
-- * 'definition' - Information about the device definition version.
-- * 'id' - The ID of the device definition version.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'responseStatus' - The response status code.
-- * 'version' - The version of the device definition version.
mkGetDeviceDefinitionVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDeviceDefinitionVersionResponse
mkGetDeviceDefinitionVersionResponse pResponseStatus_ =
  GetDeviceDefinitionVersionResponse'
    { definition = Lude.Nothing,
      arn = Lude.Nothing,
      nextToken = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the device definition version.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddvrsDefinition :: Lens.Lens' GetDeviceDefinitionVersionResponse (Lude.Maybe DeviceDefinitionVersion)
gddvrsDefinition = Lens.lens (definition :: GetDeviceDefinitionVersionResponse -> Lude.Maybe DeviceDefinitionVersion) (\s a -> s {definition = a} :: GetDeviceDefinitionVersionResponse)
{-# DEPRECATED gddvrsDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The ARN of the device definition version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddvrsARN :: Lens.Lens' GetDeviceDefinitionVersionResponse (Lude.Maybe Lude.Text)
gddvrsARN = Lens.lens (arn :: GetDeviceDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GetDeviceDefinitionVersionResponse)
{-# DEPRECATED gddvrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddvrsNextToken :: Lens.Lens' GetDeviceDefinitionVersionResponse (Lude.Maybe Lude.Text)
gddvrsNextToken = Lens.lens (nextToken :: GetDeviceDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetDeviceDefinitionVersionResponse)
{-# DEPRECATED gddvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The time, in milliseconds since the epoch, when the device definition version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddvrsCreationTimestamp :: Lens.Lens' GetDeviceDefinitionVersionResponse (Lude.Maybe Lude.Text)
gddvrsCreationTimestamp = Lens.lens (creationTimestamp :: GetDeviceDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: GetDeviceDefinitionVersionResponse)
{-# DEPRECATED gddvrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The version of the device definition version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddvrsVersion :: Lens.Lens' GetDeviceDefinitionVersionResponse (Lude.Maybe Lude.Text)
gddvrsVersion = Lens.lens (version :: GetDeviceDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: GetDeviceDefinitionVersionResponse)
{-# DEPRECATED gddvrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The ID of the device definition version.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddvrsId :: Lens.Lens' GetDeviceDefinitionVersionResponse (Lude.Maybe Lude.Text)
gddvrsId = Lens.lens (id :: GetDeviceDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: GetDeviceDefinitionVersionResponse)
{-# DEPRECATED gddvrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddvrsResponseStatus :: Lens.Lens' GetDeviceDefinitionVersionResponse Lude.Int
gddvrsResponseStatus = Lens.lens (responseStatus :: GetDeviceDefinitionVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDeviceDefinitionVersionResponse)
{-# DEPRECATED gddvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
