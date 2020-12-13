{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateFunctionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Lambda function definition which contains a list of Lambda functions and their configurations to be used in a group. You can create an initial version of the definition by providing a list of Lambda functions and their configurations now, or use ''CreateFunctionDefinitionVersion'' later.
module Network.AWS.Greengrass.CreateFunctionDefinition
  ( -- * Creating a request
    CreateFunctionDefinition (..),
    mkCreateFunctionDefinition,

    -- ** Request lenses
    cfdAmznClientToken,
    cfdInitialVersion,
    cfdName,
    cfdTags,

    -- * Destructuring the response
    CreateFunctionDefinitionResponse (..),
    mkCreateFunctionDefinitionResponse,

    -- ** Response lenses
    cfdrsLatestVersionARN,
    cfdrsARN,
    cfdrsName,
    cfdrsCreationTimestamp,
    cfdrsId,
    cfdrsLatestVersion,
    cfdrsLastUpdatedTimestamp,
    cfdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateFunctionDefinition' smart constructor.
data CreateFunctionDefinition = CreateFunctionDefinition'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Lude.Maybe Lude.Text,
    -- | Information about the initial version of the function definition.
    initialVersion :: Lude.Maybe FunctionDefinitionVersion,
    -- | The name of the function definition.
    name :: Lude.Maybe Lude.Text,
    -- | Tag(s) to add to the new resource.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFunctionDefinition' with the minimum fields required to make a request.
--
-- * 'amznClientToken' - A client token used to correlate requests and responses.
-- * 'initialVersion' - Information about the initial version of the function definition.
-- * 'name' - The name of the function definition.
-- * 'tags' - Tag(s) to add to the new resource.
mkCreateFunctionDefinition ::
  CreateFunctionDefinition
mkCreateFunctionDefinition =
  CreateFunctionDefinition'
    { amznClientToken = Lude.Nothing,
      initialVersion = Lude.Nothing,
      name = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdAmznClientToken :: Lens.Lens' CreateFunctionDefinition (Lude.Maybe Lude.Text)
cfdAmznClientToken = Lens.lens (amznClientToken :: CreateFunctionDefinition -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: CreateFunctionDefinition)
{-# DEPRECATED cfdAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | Information about the initial version of the function definition.
--
-- /Note:/ Consider using 'initialVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdInitialVersion :: Lens.Lens' CreateFunctionDefinition (Lude.Maybe FunctionDefinitionVersion)
cfdInitialVersion = Lens.lens (initialVersion :: CreateFunctionDefinition -> Lude.Maybe FunctionDefinitionVersion) (\s a -> s {initialVersion = a} :: CreateFunctionDefinition)
{-# DEPRECATED cfdInitialVersion "Use generic-lens or generic-optics with 'initialVersion' instead." #-}

-- | The name of the function definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdName :: Lens.Lens' CreateFunctionDefinition (Lude.Maybe Lude.Text)
cfdName = Lens.lens (name :: CreateFunctionDefinition -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateFunctionDefinition)
{-# DEPRECATED cfdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) to add to the new resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdTags :: Lens.Lens' CreateFunctionDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cfdTags = Lens.lens (tags :: CreateFunctionDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateFunctionDefinition)
{-# DEPRECATED cfdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateFunctionDefinition where
  type Rs CreateFunctionDefinition = CreateFunctionDefinitionResponse
  request = Req.postJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateFunctionDefinitionResponse'
            Lude.<$> (x Lude..?> "LatestVersionArn")
            Lude.<*> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (x Lude..?> "LatestVersion")
            Lude.<*> (x Lude..?> "LastUpdatedTimestamp")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateFunctionDefinition where
  toHeaders CreateFunctionDefinition' {..} =
    Lude.mconcat
      [ "X-Amzn-Client-Token" Lude.=# amznClientToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateFunctionDefinition where
  toJSON CreateFunctionDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InitialVersion" Lude..=) Lude.<$> initialVersion,
            ("Name" Lude..=) Lude.<$> name,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateFunctionDefinition where
  toPath = Lude.const "/greengrass/definition/functions"

instance Lude.ToQuery CreateFunctionDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateFunctionDefinitionResponse' smart constructor.
data CreateFunctionDefinitionResponse = CreateFunctionDefinitionResponse'
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

-- | Creates a value of 'CreateFunctionDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'latestVersionARN' - The ARN of the latest version associated with the definition.
-- * 'arn' - The ARN of the definition.
-- * 'name' - The name of the definition.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
-- * 'id' - The ID of the definition.
-- * 'latestVersion' - The ID of the latest version associated with the definition.
-- * 'lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
-- * 'responseStatus' - The response status code.
mkCreateFunctionDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateFunctionDefinitionResponse
mkCreateFunctionDefinitionResponse pResponseStatus_ =
  CreateFunctionDefinitionResponse'
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
cfdrsLatestVersionARN :: Lens.Lens' CreateFunctionDefinitionResponse (Lude.Maybe Lude.Text)
cfdrsLatestVersionARN = Lens.lens (latestVersionARN :: CreateFunctionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersionARN = a} :: CreateFunctionDefinitionResponse)
{-# DEPRECATED cfdrsLatestVersionARN "Use generic-lens or generic-optics with 'latestVersionARN' instead." #-}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrsARN :: Lens.Lens' CreateFunctionDefinitionResponse (Lude.Maybe Lude.Text)
cfdrsARN = Lens.lens (arn :: CreateFunctionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateFunctionDefinitionResponse)
{-# DEPRECATED cfdrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrsName :: Lens.Lens' CreateFunctionDefinitionResponse (Lude.Maybe Lude.Text)
cfdrsName = Lens.lens (name :: CreateFunctionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateFunctionDefinitionResponse)
{-# DEPRECATED cfdrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrsCreationTimestamp :: Lens.Lens' CreateFunctionDefinitionResponse (Lude.Maybe Lude.Text)
cfdrsCreationTimestamp = Lens.lens (creationTimestamp :: CreateFunctionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: CreateFunctionDefinitionResponse)
{-# DEPRECATED cfdrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrsId :: Lens.Lens' CreateFunctionDefinitionResponse (Lude.Maybe Lude.Text)
cfdrsId = Lens.lens (id :: CreateFunctionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateFunctionDefinitionResponse)
{-# DEPRECATED cfdrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrsLatestVersion :: Lens.Lens' CreateFunctionDefinitionResponse (Lude.Maybe Lude.Text)
cfdrsLatestVersion = Lens.lens (latestVersion :: CreateFunctionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersion = a} :: CreateFunctionDefinitionResponse)
{-# DEPRECATED cfdrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrsLastUpdatedTimestamp :: Lens.Lens' CreateFunctionDefinitionResponse (Lude.Maybe Lude.Text)
cfdrsLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: CreateFunctionDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedTimestamp = a} :: CreateFunctionDefinitionResponse)
{-# DEPRECATED cfdrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrsResponseStatus :: Lens.Lens' CreateFunctionDefinitionResponse Lude.Int
cfdrsResponseStatus = Lens.lens (responseStatus :: CreateFunctionDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateFunctionDefinitionResponse)
{-# DEPRECATED cfdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
