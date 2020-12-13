{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateCoreDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a core definition. You may provide the initial version of the core definition now or use ''CreateCoreDefinitionVersion'' at a later time. Greengrass groups must each contain exactly one Greengrass core.
module Network.AWS.Greengrass.CreateCoreDefinition
  ( -- * Creating a request
    CreateCoreDefinition (..),
    mkCreateCoreDefinition,

    -- ** Request lenses
    cAmznClientToken,
    cInitialVersion,
    cName,
    cTags,

    -- * Destructuring the response
    CreateCoreDefinitionResponse (..),
    mkCreateCoreDefinitionResponse,

    -- ** Response lenses
    ccdrsLatestVersionARN,
    ccdrsARN,
    ccdrsName,
    ccdrsCreationTimestamp,
    ccdrsId,
    ccdrsLatestVersion,
    ccdrsLastUpdatedTimestamp,
    ccdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Information needed to create a core definition.
--
-- /See:/ 'mkCreateCoreDefinition' smart constructor.
data CreateCoreDefinition = CreateCoreDefinition'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Lude.Maybe Lude.Text,
    -- | Information about the initial version of the core definition.
    initialVersion :: Lude.Maybe CoreDefinitionVersion,
    -- | The name of the core definition.
    name :: Lude.Maybe Lude.Text,
    -- | Tag(s) to add to the new resource.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCoreDefinition' with the minimum fields required to make a request.
--
-- * 'amznClientToken' - A client token used to correlate requests and responses.
-- * 'initialVersion' - Information about the initial version of the core definition.
-- * 'name' - The name of the core definition.
-- * 'tags' - Tag(s) to add to the new resource.
mkCreateCoreDefinition ::
  CreateCoreDefinition
mkCreateCoreDefinition =
  CreateCoreDefinition'
    { amznClientToken = Lude.Nothing,
      initialVersion = Lude.Nothing,
      name = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAmznClientToken :: Lens.Lens' CreateCoreDefinition (Lude.Maybe Lude.Text)
cAmznClientToken = Lens.lens (amznClientToken :: CreateCoreDefinition -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: CreateCoreDefinition)
{-# DEPRECATED cAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | Information about the initial version of the core definition.
--
-- /Note:/ Consider using 'initialVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cInitialVersion :: Lens.Lens' CreateCoreDefinition (Lude.Maybe CoreDefinitionVersion)
cInitialVersion = Lens.lens (initialVersion :: CreateCoreDefinition -> Lude.Maybe CoreDefinitionVersion) (\s a -> s {initialVersion = a} :: CreateCoreDefinition)
{-# DEPRECATED cInitialVersion "Use generic-lens or generic-optics with 'initialVersion' instead." #-}

-- | The name of the core definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' CreateCoreDefinition (Lude.Maybe Lude.Text)
cName = Lens.lens (name :: CreateCoreDefinition -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateCoreDefinition)
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) to add to the new resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CreateCoreDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cTags = Lens.lens (tags :: CreateCoreDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateCoreDefinition)
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateCoreDefinition where
  type Rs CreateCoreDefinition = CreateCoreDefinitionResponse
  request = Req.postJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCoreDefinitionResponse'
            Lude.<$> (x Lude..?> "LatestVersionArn")
            Lude.<*> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (x Lude..?> "LatestVersion")
            Lude.<*> (x Lude..?> "LastUpdatedTimestamp")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCoreDefinition where
  toHeaders CreateCoreDefinition' {..} =
    Lude.mconcat
      [ "X-Amzn-Client-Token" Lude.=# amznClientToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateCoreDefinition where
  toJSON CreateCoreDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InitialVersion" Lude..=) Lude.<$> initialVersion,
            ("Name" Lude..=) Lude.<$> name,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateCoreDefinition where
  toPath = Lude.const "/greengrass/definition/cores"

instance Lude.ToQuery CreateCoreDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCoreDefinitionResponse' smart constructor.
data CreateCoreDefinitionResponse = CreateCoreDefinitionResponse'
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

-- | Creates a value of 'CreateCoreDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'latestVersionARN' - The ARN of the latest version associated with the definition.
-- * 'arn' - The ARN of the definition.
-- * 'name' - The name of the definition.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
-- * 'id' - The ID of the definition.
-- * 'latestVersion' - The ID of the latest version associated with the definition.
-- * 'lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
-- * 'responseStatus' - The response status code.
mkCreateCoreDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCoreDefinitionResponse
mkCreateCoreDefinitionResponse pResponseStatus_ =
  CreateCoreDefinitionResponse'
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
ccdrsLatestVersionARN :: Lens.Lens' CreateCoreDefinitionResponse (Lude.Maybe Lude.Text)
ccdrsLatestVersionARN = Lens.lens (latestVersionARN :: CreateCoreDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersionARN = a} :: CreateCoreDefinitionResponse)
{-# DEPRECATED ccdrsLatestVersionARN "Use generic-lens or generic-optics with 'latestVersionARN' instead." #-}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrsARN :: Lens.Lens' CreateCoreDefinitionResponse (Lude.Maybe Lude.Text)
ccdrsARN = Lens.lens (arn :: CreateCoreDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateCoreDefinitionResponse)
{-# DEPRECATED ccdrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrsName :: Lens.Lens' CreateCoreDefinitionResponse (Lude.Maybe Lude.Text)
ccdrsName = Lens.lens (name :: CreateCoreDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateCoreDefinitionResponse)
{-# DEPRECATED ccdrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrsCreationTimestamp :: Lens.Lens' CreateCoreDefinitionResponse (Lude.Maybe Lude.Text)
ccdrsCreationTimestamp = Lens.lens (creationTimestamp :: CreateCoreDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: CreateCoreDefinitionResponse)
{-# DEPRECATED ccdrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrsId :: Lens.Lens' CreateCoreDefinitionResponse (Lude.Maybe Lude.Text)
ccdrsId = Lens.lens (id :: CreateCoreDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateCoreDefinitionResponse)
{-# DEPRECATED ccdrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrsLatestVersion :: Lens.Lens' CreateCoreDefinitionResponse (Lude.Maybe Lude.Text)
ccdrsLatestVersion = Lens.lens (latestVersion :: CreateCoreDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersion = a} :: CreateCoreDefinitionResponse)
{-# DEPRECATED ccdrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrsLastUpdatedTimestamp :: Lens.Lens' CreateCoreDefinitionResponse (Lude.Maybe Lude.Text)
ccdrsLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: CreateCoreDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedTimestamp = a} :: CreateCoreDefinitionResponse)
{-# DEPRECATED ccdrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrsResponseStatus :: Lens.Lens' CreateCoreDefinitionResponse Lude.Int
ccdrsResponseStatus = Lens.lens (responseStatus :: CreateCoreDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCoreDefinitionResponse)
{-# DEPRECATED ccdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
