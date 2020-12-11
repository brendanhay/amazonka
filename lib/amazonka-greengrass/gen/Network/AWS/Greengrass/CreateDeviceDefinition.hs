{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateDeviceDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a device definition. You may provide the initial version of the device definition now or use ''CreateDeviceDefinitionVersion'' at a later time.
module Network.AWS.Greengrass.CreateDeviceDefinition
  ( -- * Creating a request
    CreateDeviceDefinition (..),
    mkCreateDeviceDefinition,

    -- ** Request lenses
    cddAmznClientToken,
    cddInitialVersion,
    cddName,
    cddTags,

    -- * Destructuring the response
    CreateDeviceDefinitionResponse (..),
    mkCreateDeviceDefinitionResponse,

    -- ** Response lenses
    cddrsLatestVersionARN,
    cddrsARN,
    cddrsName,
    cddrsCreationTimestamp,
    cddrsId,
    cddrsLatestVersion,
    cddrsLastUpdatedTimestamp,
    cddrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDeviceDefinition' smart constructor.
data CreateDeviceDefinition = CreateDeviceDefinition'
  { amznClientToken ::
      Lude.Maybe Lude.Text,
    initialVersion ::
      Lude.Maybe DeviceDefinitionVersion,
    name :: Lude.Maybe Lude.Text,
    tags ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDeviceDefinition' with the minimum fields required to make a request.
--
-- * 'amznClientToken' - A client token used to correlate requests and responses.
-- * 'initialVersion' - Information about the initial version of the device definition.
-- * 'name' - The name of the device definition.
-- * 'tags' - Tag(s) to add to the new resource.
mkCreateDeviceDefinition ::
  CreateDeviceDefinition
mkCreateDeviceDefinition =
  CreateDeviceDefinition'
    { amznClientToken = Lude.Nothing,
      initialVersion = Lude.Nothing,
      name = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddAmznClientToken :: Lens.Lens' CreateDeviceDefinition (Lude.Maybe Lude.Text)
cddAmznClientToken = Lens.lens (amznClientToken :: CreateDeviceDefinition -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: CreateDeviceDefinition)
{-# DEPRECATED cddAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | Information about the initial version of the device definition.
--
-- /Note:/ Consider using 'initialVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddInitialVersion :: Lens.Lens' CreateDeviceDefinition (Lude.Maybe DeviceDefinitionVersion)
cddInitialVersion = Lens.lens (initialVersion :: CreateDeviceDefinition -> Lude.Maybe DeviceDefinitionVersion) (\s a -> s {initialVersion = a} :: CreateDeviceDefinition)
{-# DEPRECATED cddInitialVersion "Use generic-lens or generic-optics with 'initialVersion' instead." #-}

-- | The name of the device definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddName :: Lens.Lens' CreateDeviceDefinition (Lude.Maybe Lude.Text)
cddName = Lens.lens (name :: CreateDeviceDefinition -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateDeviceDefinition)
{-# DEPRECATED cddName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) to add to the new resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddTags :: Lens.Lens' CreateDeviceDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cddTags = Lens.lens (tags :: CreateDeviceDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateDeviceDefinition)
{-# DEPRECATED cddTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateDeviceDefinition where
  type Rs CreateDeviceDefinition = CreateDeviceDefinitionResponse
  request = Req.postJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDeviceDefinitionResponse'
            Lude.<$> (x Lude..?> "LatestVersionArn")
            Lude.<*> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (x Lude..?> "LatestVersion")
            Lude.<*> (x Lude..?> "LastUpdatedTimestamp")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDeviceDefinition where
  toHeaders CreateDeviceDefinition' {..} =
    Lude.mconcat
      [ "X-Amzn-Client-Token" Lude.=# amznClientToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateDeviceDefinition where
  toJSON CreateDeviceDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InitialVersion" Lude..=) Lude.<$> initialVersion,
            ("Name" Lude..=) Lude.<$> name,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateDeviceDefinition where
  toPath = Lude.const "/greengrass/definition/devices"

instance Lude.ToQuery CreateDeviceDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDeviceDefinitionResponse' smart constructor.
data CreateDeviceDefinitionResponse = CreateDeviceDefinitionResponse'
  { latestVersionARN ::
      Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    creationTimestamp ::
      Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    latestVersion ::
      Lude.Maybe Lude.Text,
    lastUpdatedTimestamp ::
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

-- | Creates a value of 'CreateDeviceDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the definition.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
-- * 'id' - The ID of the definition.
-- * 'lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
-- * 'latestVersion' - The ID of the latest version associated with the definition.
-- * 'latestVersionARN' - The ARN of the latest version associated with the definition.
-- * 'name' - The name of the definition.
-- * 'responseStatus' - The response status code.
mkCreateDeviceDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDeviceDefinitionResponse
mkCreateDeviceDefinitionResponse pResponseStatus_ =
  CreateDeviceDefinitionResponse'
    { latestVersionARN = Lude.Nothing,
      arn = Lude.Nothing,
      name = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      id = Lude.Nothing,
      latestVersion = Lude.Nothing,
      lastUpdatedTimestamp = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddrsLatestVersionARN :: Lens.Lens' CreateDeviceDefinitionResponse (Lude.Maybe Lude.Text)
cddrsLatestVersionARN = Lens.lens (latestVersionARN :: CreateDeviceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersionARN = a} :: CreateDeviceDefinitionResponse)
{-# DEPRECATED cddrsLatestVersionARN "Use generic-lens or generic-optics with 'latestVersionARN' instead." #-}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddrsARN :: Lens.Lens' CreateDeviceDefinitionResponse (Lude.Maybe Lude.Text)
cddrsARN = Lens.lens (arn :: CreateDeviceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateDeviceDefinitionResponse)
{-# DEPRECATED cddrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddrsName :: Lens.Lens' CreateDeviceDefinitionResponse (Lude.Maybe Lude.Text)
cddrsName = Lens.lens (name :: CreateDeviceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateDeviceDefinitionResponse)
{-# DEPRECATED cddrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddrsCreationTimestamp :: Lens.Lens' CreateDeviceDefinitionResponse (Lude.Maybe Lude.Text)
cddrsCreationTimestamp = Lens.lens (creationTimestamp :: CreateDeviceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: CreateDeviceDefinitionResponse)
{-# DEPRECATED cddrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddrsId :: Lens.Lens' CreateDeviceDefinitionResponse (Lude.Maybe Lude.Text)
cddrsId = Lens.lens (id :: CreateDeviceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateDeviceDefinitionResponse)
{-# DEPRECATED cddrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddrsLatestVersion :: Lens.Lens' CreateDeviceDefinitionResponse (Lude.Maybe Lude.Text)
cddrsLatestVersion = Lens.lens (latestVersion :: CreateDeviceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersion = a} :: CreateDeviceDefinitionResponse)
{-# DEPRECATED cddrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddrsLastUpdatedTimestamp :: Lens.Lens' CreateDeviceDefinitionResponse (Lude.Maybe Lude.Text)
cddrsLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: CreateDeviceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedTimestamp = a} :: CreateDeviceDefinitionResponse)
{-# DEPRECATED cddrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddrsResponseStatus :: Lens.Lens' CreateDeviceDefinitionResponse Lude.Int
cddrsResponseStatus = Lens.lens (responseStatus :: CreateDeviceDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDeviceDefinitionResponse)
{-# DEPRECATED cddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
