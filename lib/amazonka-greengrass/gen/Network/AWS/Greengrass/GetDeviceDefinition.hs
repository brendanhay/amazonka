{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetDeviceDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a device definition.
module Network.AWS.Greengrass.GetDeviceDefinition
  ( -- * Creating a request
    GetDeviceDefinition (..),
    mkGetDeviceDefinition,

    -- ** Request lenses
    gddDeviceDefinitionId,

    -- * Destructuring the response
    GetDeviceDefinitionResponse (..),
    mkGetDeviceDefinitionResponse,

    -- ** Response lenses
    gddrsLatestVersionARN,
    gddrsARN,
    gddrsName,
    gddrsCreationTimestamp,
    gddrsId,
    gddrsLatestVersion,
    gddrsLastUpdatedTimestamp,
    gddrsTags,
    gddrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDeviceDefinition' smart constructor.
newtype GetDeviceDefinition = GetDeviceDefinition'
  { -- | The ID of the device definition.
    deviceDefinitionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDeviceDefinition' with the minimum fields required to make a request.
--
-- * 'deviceDefinitionId' - The ID of the device definition.
mkGetDeviceDefinition ::
  -- | 'deviceDefinitionId'
  Lude.Text ->
  GetDeviceDefinition
mkGetDeviceDefinition pDeviceDefinitionId_ =
  GetDeviceDefinition' {deviceDefinitionId = pDeviceDefinitionId_}

-- | The ID of the device definition.
--
-- /Note:/ Consider using 'deviceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddDeviceDefinitionId :: Lens.Lens' GetDeviceDefinition Lude.Text
gddDeviceDefinitionId = Lens.lens (deviceDefinitionId :: GetDeviceDefinition -> Lude.Text) (\s a -> s {deviceDefinitionId = a} :: GetDeviceDefinition)
{-# DEPRECATED gddDeviceDefinitionId "Use generic-lens or generic-optics with 'deviceDefinitionId' instead." #-}

instance Lude.AWSRequest GetDeviceDefinition where
  type Rs GetDeviceDefinition = GetDeviceDefinitionResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDeviceDefinitionResponse'
            Lude.<$> (x Lude..?> "LatestVersionArn")
            Lude.<*> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (x Lude..?> "LatestVersion")
            Lude.<*> (x Lude..?> "LastUpdatedTimestamp")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDeviceDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetDeviceDefinition where
  toPath GetDeviceDefinition' {..} =
    Lude.mconcat
      ["/greengrass/definition/devices/", Lude.toBS deviceDefinitionId]

instance Lude.ToQuery GetDeviceDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDeviceDefinitionResponse' smart constructor.
data GetDeviceDefinitionResponse = GetDeviceDefinitionResponse'
  { -- | The ARN of the latest version associated with the definition.
    latestVersionARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the definition.
    arn :: Lude.Maybe Lude.Text,
    -- | The name of the definition.
    name :: Lude.Maybe Lude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was created.
    creationTimestamp :: Lude.Maybe Lude.Text,
    -- | The ID of the definition.
    id :: Lude.Maybe Lude.Text,
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Lude.Maybe Lude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was last updated.
    lastUpdatedTimestamp :: Lude.Maybe Lude.Text,
    -- | Tag(s) attached to the resource arn.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDeviceDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'latestVersionARN' - The ARN of the latest version associated with the definition.
-- * 'arn' - The ARN of the definition.
-- * 'name' - The name of the definition.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
-- * 'id' - The ID of the definition.
-- * 'latestVersion' - The ID of the latest version associated with the definition.
-- * 'lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
-- * 'tags' - Tag(s) attached to the resource arn.
-- * 'responseStatus' - The response status code.
mkGetDeviceDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDeviceDefinitionResponse
mkGetDeviceDefinitionResponse pResponseStatus_ =
  GetDeviceDefinitionResponse'
    { latestVersionARN = Lude.Nothing,
      arn = Lude.Nothing,
      name = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      id = Lude.Nothing,
      latestVersion = Lude.Nothing,
      lastUpdatedTimestamp = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsLatestVersionARN :: Lens.Lens' GetDeviceDefinitionResponse (Lude.Maybe Lude.Text)
gddrsLatestVersionARN = Lens.lens (latestVersionARN :: GetDeviceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersionARN = a} :: GetDeviceDefinitionResponse)
{-# DEPRECATED gddrsLatestVersionARN "Use generic-lens or generic-optics with 'latestVersionARN' instead." #-}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsARN :: Lens.Lens' GetDeviceDefinitionResponse (Lude.Maybe Lude.Text)
gddrsARN = Lens.lens (arn :: GetDeviceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GetDeviceDefinitionResponse)
{-# DEPRECATED gddrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsName :: Lens.Lens' GetDeviceDefinitionResponse (Lude.Maybe Lude.Text)
gddrsName = Lens.lens (name :: GetDeviceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetDeviceDefinitionResponse)
{-# DEPRECATED gddrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsCreationTimestamp :: Lens.Lens' GetDeviceDefinitionResponse (Lude.Maybe Lude.Text)
gddrsCreationTimestamp = Lens.lens (creationTimestamp :: GetDeviceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: GetDeviceDefinitionResponse)
{-# DEPRECATED gddrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsId :: Lens.Lens' GetDeviceDefinitionResponse (Lude.Maybe Lude.Text)
gddrsId = Lens.lens (id :: GetDeviceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: GetDeviceDefinitionResponse)
{-# DEPRECATED gddrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsLatestVersion :: Lens.Lens' GetDeviceDefinitionResponse (Lude.Maybe Lude.Text)
gddrsLatestVersion = Lens.lens (latestVersion :: GetDeviceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersion = a} :: GetDeviceDefinitionResponse)
{-# DEPRECATED gddrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsLastUpdatedTimestamp :: Lens.Lens' GetDeviceDefinitionResponse (Lude.Maybe Lude.Text)
gddrsLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: GetDeviceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedTimestamp = a} :: GetDeviceDefinitionResponse)
{-# DEPRECATED gddrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsTags :: Lens.Lens' GetDeviceDefinitionResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gddrsTags = Lens.lens (tags :: GetDeviceDefinitionResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: GetDeviceDefinitionResponse)
{-# DEPRECATED gddrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsResponseStatus :: Lens.Lens' GetDeviceDefinitionResponse Lude.Int
gddrsResponseStatus = Lens.lens (responseStatus :: GetDeviceDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDeviceDefinitionResponse)
{-# DEPRECATED gddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
