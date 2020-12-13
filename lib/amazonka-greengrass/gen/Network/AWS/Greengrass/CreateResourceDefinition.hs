{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateResourceDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a resource definition which contains a list of resources to be used in a group. You can create an initial version of the definition by providing a list of resources now, or use ''CreateResourceDefinitionVersion'' later.
module Network.AWS.Greengrass.CreateResourceDefinition
  ( -- * Creating a request
    CreateResourceDefinition (..),
    mkCreateResourceDefinition,

    -- ** Request lenses
    crdAmznClientToken,
    crdInitialVersion,
    crdName,
    crdTags,

    -- * Destructuring the response
    CreateResourceDefinitionResponse (..),
    mkCreateResourceDefinitionResponse,

    -- ** Response lenses
    crdrsLatestVersionARN,
    crdrsARN,
    crdrsName,
    crdrsCreationTimestamp,
    crdrsId,
    crdrsLatestVersion,
    crdrsLastUpdatedTimestamp,
    crdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateResourceDefinition' smart constructor.
data CreateResourceDefinition = CreateResourceDefinition'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Lude.Maybe Lude.Text,
    -- | Information about the initial version of the resource definition.
    initialVersion :: Lude.Maybe ResourceDefinitionVersion,
    -- | The name of the resource definition.
    name :: Lude.Maybe Lude.Text,
    -- | Tag(s) to add to the new resource.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateResourceDefinition' with the minimum fields required to make a request.
--
-- * 'amznClientToken' - A client token used to correlate requests and responses.
-- * 'initialVersion' - Information about the initial version of the resource definition.
-- * 'name' - The name of the resource definition.
-- * 'tags' - Tag(s) to add to the new resource.
mkCreateResourceDefinition ::
  CreateResourceDefinition
mkCreateResourceDefinition =
  CreateResourceDefinition'
    { amznClientToken = Lude.Nothing,
      initialVersion = Lude.Nothing,
      name = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdAmznClientToken :: Lens.Lens' CreateResourceDefinition (Lude.Maybe Lude.Text)
crdAmznClientToken = Lens.lens (amznClientToken :: CreateResourceDefinition -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: CreateResourceDefinition)
{-# DEPRECATED crdAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | Information about the initial version of the resource definition.
--
-- /Note:/ Consider using 'initialVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdInitialVersion :: Lens.Lens' CreateResourceDefinition (Lude.Maybe ResourceDefinitionVersion)
crdInitialVersion = Lens.lens (initialVersion :: CreateResourceDefinition -> Lude.Maybe ResourceDefinitionVersion) (\s a -> s {initialVersion = a} :: CreateResourceDefinition)
{-# DEPRECATED crdInitialVersion "Use generic-lens or generic-optics with 'initialVersion' instead." #-}

-- | The name of the resource definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdName :: Lens.Lens' CreateResourceDefinition (Lude.Maybe Lude.Text)
crdName = Lens.lens (name :: CreateResourceDefinition -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateResourceDefinition)
{-# DEPRECATED crdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) to add to the new resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdTags :: Lens.Lens' CreateResourceDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
crdTags = Lens.lens (tags :: CreateResourceDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateResourceDefinition)
{-# DEPRECATED crdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateResourceDefinition where
  type Rs CreateResourceDefinition = CreateResourceDefinitionResponse
  request = Req.postJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateResourceDefinitionResponse'
            Lude.<$> (x Lude..?> "LatestVersionArn")
            Lude.<*> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (x Lude..?> "LatestVersion")
            Lude.<*> (x Lude..?> "LastUpdatedTimestamp")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateResourceDefinition where
  toHeaders CreateResourceDefinition' {..} =
    Lude.mconcat
      [ "X-Amzn-Client-Token" Lude.=# amznClientToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateResourceDefinition where
  toJSON CreateResourceDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InitialVersion" Lude..=) Lude.<$> initialVersion,
            ("Name" Lude..=) Lude.<$> name,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateResourceDefinition where
  toPath = Lude.const "/greengrass/definition/resources"

instance Lude.ToQuery CreateResourceDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateResourceDefinitionResponse' smart constructor.
data CreateResourceDefinitionResponse = CreateResourceDefinitionResponse'
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
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateResourceDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'latestVersionARN' - The ARN of the latest version associated with the definition.
-- * 'arn' - The ARN of the definition.
-- * 'name' - The name of the definition.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
-- * 'id' - The ID of the definition.
-- * 'latestVersion' - The ID of the latest version associated with the definition.
-- * 'lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
-- * 'responseStatus' - The response status code.
mkCreateResourceDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateResourceDefinitionResponse
mkCreateResourceDefinitionResponse pResponseStatus_ =
  CreateResourceDefinitionResponse'
    { latestVersionARN =
        Lude.Nothing,
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
crdrsLatestVersionARN :: Lens.Lens' CreateResourceDefinitionResponse (Lude.Maybe Lude.Text)
crdrsLatestVersionARN = Lens.lens (latestVersionARN :: CreateResourceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersionARN = a} :: CreateResourceDefinitionResponse)
{-# DEPRECATED crdrsLatestVersionARN "Use generic-lens or generic-optics with 'latestVersionARN' instead." #-}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdrsARN :: Lens.Lens' CreateResourceDefinitionResponse (Lude.Maybe Lude.Text)
crdrsARN = Lens.lens (arn :: CreateResourceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateResourceDefinitionResponse)
{-# DEPRECATED crdrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdrsName :: Lens.Lens' CreateResourceDefinitionResponse (Lude.Maybe Lude.Text)
crdrsName = Lens.lens (name :: CreateResourceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateResourceDefinitionResponse)
{-# DEPRECATED crdrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdrsCreationTimestamp :: Lens.Lens' CreateResourceDefinitionResponse (Lude.Maybe Lude.Text)
crdrsCreationTimestamp = Lens.lens (creationTimestamp :: CreateResourceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: CreateResourceDefinitionResponse)
{-# DEPRECATED crdrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdrsId :: Lens.Lens' CreateResourceDefinitionResponse (Lude.Maybe Lude.Text)
crdrsId = Lens.lens (id :: CreateResourceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateResourceDefinitionResponse)
{-# DEPRECATED crdrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdrsLatestVersion :: Lens.Lens' CreateResourceDefinitionResponse (Lude.Maybe Lude.Text)
crdrsLatestVersion = Lens.lens (latestVersion :: CreateResourceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersion = a} :: CreateResourceDefinitionResponse)
{-# DEPRECATED crdrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdrsLastUpdatedTimestamp :: Lens.Lens' CreateResourceDefinitionResponse (Lude.Maybe Lude.Text)
crdrsLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: CreateResourceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedTimestamp = a} :: CreateResourceDefinitionResponse)
{-# DEPRECATED crdrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdrsResponseStatus :: Lens.Lens' CreateResourceDefinitionResponse Lude.Int
crdrsResponseStatus = Lens.lens (responseStatus :: CreateResourceDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateResourceDefinitionResponse)
{-# DEPRECATED crdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
