{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateLoggerDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a logger definition. You may provide the initial version of the logger definition now or use ''CreateLoggerDefinitionVersion'' at a later time.
module Network.AWS.Greengrass.CreateLoggerDefinition
  ( -- * Creating a request
    CreateLoggerDefinition (..),
    mkCreateLoggerDefinition,

    -- ** Request lenses
    cldAmznClientToken,
    cldInitialVersion,
    cldName,
    cldTags,

    -- * Destructuring the response
    CreateLoggerDefinitionResponse (..),
    mkCreateLoggerDefinitionResponse,

    -- ** Response lenses
    cldrsLatestVersionARN,
    cldrsARN,
    cldrsName,
    cldrsCreationTimestamp,
    cldrsId,
    cldrsLatestVersion,
    cldrsLastUpdatedTimestamp,
    cldrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateLoggerDefinition' smart constructor.
data CreateLoggerDefinition = CreateLoggerDefinition'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Lude.Maybe Lude.Text,
    -- | Information about the initial version of the logger definition.
    initialVersion :: Lude.Maybe LoggerDefinitionVersion,
    -- | The name of the logger definition.
    name :: Lude.Maybe Lude.Text,
    -- | Tag(s) to add to the new resource.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLoggerDefinition' with the minimum fields required to make a request.
--
-- * 'amznClientToken' - A client token used to correlate requests and responses.
-- * 'initialVersion' - Information about the initial version of the logger definition.
-- * 'name' - The name of the logger definition.
-- * 'tags' - Tag(s) to add to the new resource.
mkCreateLoggerDefinition ::
  CreateLoggerDefinition
mkCreateLoggerDefinition =
  CreateLoggerDefinition'
    { amznClientToken = Lude.Nothing,
      initialVersion = Lude.Nothing,
      name = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldAmznClientToken :: Lens.Lens' CreateLoggerDefinition (Lude.Maybe Lude.Text)
cldAmznClientToken = Lens.lens (amznClientToken :: CreateLoggerDefinition -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: CreateLoggerDefinition)
{-# DEPRECATED cldAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | Information about the initial version of the logger definition.
--
-- /Note:/ Consider using 'initialVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldInitialVersion :: Lens.Lens' CreateLoggerDefinition (Lude.Maybe LoggerDefinitionVersion)
cldInitialVersion = Lens.lens (initialVersion :: CreateLoggerDefinition -> Lude.Maybe LoggerDefinitionVersion) (\s a -> s {initialVersion = a} :: CreateLoggerDefinition)
{-# DEPRECATED cldInitialVersion "Use generic-lens or generic-optics with 'initialVersion' instead." #-}

-- | The name of the logger definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldName :: Lens.Lens' CreateLoggerDefinition (Lude.Maybe Lude.Text)
cldName = Lens.lens (name :: CreateLoggerDefinition -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateLoggerDefinition)
{-# DEPRECATED cldName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) to add to the new resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldTags :: Lens.Lens' CreateLoggerDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cldTags = Lens.lens (tags :: CreateLoggerDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateLoggerDefinition)
{-# DEPRECATED cldTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateLoggerDefinition where
  type Rs CreateLoggerDefinition = CreateLoggerDefinitionResponse
  request = Req.postJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateLoggerDefinitionResponse'
            Lude.<$> (x Lude..?> "LatestVersionArn")
            Lude.<*> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (x Lude..?> "LatestVersion")
            Lude.<*> (x Lude..?> "LastUpdatedTimestamp")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateLoggerDefinition where
  toHeaders CreateLoggerDefinition' {..} =
    Lude.mconcat
      [ "X-Amzn-Client-Token" Lude.=# amznClientToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateLoggerDefinition where
  toJSON CreateLoggerDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InitialVersion" Lude..=) Lude.<$> initialVersion,
            ("Name" Lude..=) Lude.<$> name,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateLoggerDefinition where
  toPath = Lude.const "/greengrass/definition/loggers"

instance Lude.ToQuery CreateLoggerDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateLoggerDefinitionResponse' smart constructor.
data CreateLoggerDefinitionResponse = CreateLoggerDefinitionResponse'
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

-- | Creates a value of 'CreateLoggerDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'latestVersionARN' - The ARN of the latest version associated with the definition.
-- * 'arn' - The ARN of the definition.
-- * 'name' - The name of the definition.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
-- * 'id' - The ID of the definition.
-- * 'latestVersion' - The ID of the latest version associated with the definition.
-- * 'lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
-- * 'responseStatus' - The response status code.
mkCreateLoggerDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateLoggerDefinitionResponse
mkCreateLoggerDefinitionResponse pResponseStatus_ =
  CreateLoggerDefinitionResponse'
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
cldrsLatestVersionARN :: Lens.Lens' CreateLoggerDefinitionResponse (Lude.Maybe Lude.Text)
cldrsLatestVersionARN = Lens.lens (latestVersionARN :: CreateLoggerDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersionARN = a} :: CreateLoggerDefinitionResponse)
{-# DEPRECATED cldrsLatestVersionARN "Use generic-lens or generic-optics with 'latestVersionARN' instead." #-}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldrsARN :: Lens.Lens' CreateLoggerDefinitionResponse (Lude.Maybe Lude.Text)
cldrsARN = Lens.lens (arn :: CreateLoggerDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateLoggerDefinitionResponse)
{-# DEPRECATED cldrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldrsName :: Lens.Lens' CreateLoggerDefinitionResponse (Lude.Maybe Lude.Text)
cldrsName = Lens.lens (name :: CreateLoggerDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateLoggerDefinitionResponse)
{-# DEPRECATED cldrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldrsCreationTimestamp :: Lens.Lens' CreateLoggerDefinitionResponse (Lude.Maybe Lude.Text)
cldrsCreationTimestamp = Lens.lens (creationTimestamp :: CreateLoggerDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: CreateLoggerDefinitionResponse)
{-# DEPRECATED cldrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldrsId :: Lens.Lens' CreateLoggerDefinitionResponse (Lude.Maybe Lude.Text)
cldrsId = Lens.lens (id :: CreateLoggerDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateLoggerDefinitionResponse)
{-# DEPRECATED cldrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldrsLatestVersion :: Lens.Lens' CreateLoggerDefinitionResponse (Lude.Maybe Lude.Text)
cldrsLatestVersion = Lens.lens (latestVersion :: CreateLoggerDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersion = a} :: CreateLoggerDefinitionResponse)
{-# DEPRECATED cldrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldrsLastUpdatedTimestamp :: Lens.Lens' CreateLoggerDefinitionResponse (Lude.Maybe Lude.Text)
cldrsLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: CreateLoggerDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedTimestamp = a} :: CreateLoggerDefinitionResponse)
{-# DEPRECATED cldrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldrsResponseStatus :: Lens.Lens' CreateLoggerDefinitionResponse Lude.Int
cldrsResponseStatus = Lens.lens (responseStatus :: CreateLoggerDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateLoggerDefinitionResponse)
{-# DEPRECATED cldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
