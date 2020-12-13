{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateConnectorDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connector definition. You may provide the initial version of the connector definition now or use ''CreateConnectorDefinitionVersion'' at a later time.
module Network.AWS.Greengrass.CreateConnectorDefinition
  ( -- * Creating a request
    CreateConnectorDefinition (..),
    mkCreateConnectorDefinition,

    -- ** Request lenses
    ccdAmznClientToken,
    ccdInitialVersion,
    ccdName,
    ccdTags,

    -- * Destructuring the response
    CreateConnectorDefinitionResponse (..),
    mkCreateConnectorDefinitionResponse,

    -- ** Response lenses
    ccdfrsLatestVersionARN,
    ccdfrsARN,
    ccdfrsName,
    ccdfrsCreationTimestamp,
    ccdfrsId,
    ccdfrsLatestVersion,
    ccdfrsLastUpdatedTimestamp,
    ccdfrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateConnectorDefinition' smart constructor.
data CreateConnectorDefinition = CreateConnectorDefinition'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Lude.Maybe Lude.Text,
    -- | Information about the initial version of the connector definition.
    initialVersion :: Lude.Maybe ConnectorDefinitionVersion,
    -- | The name of the connector definition.
    name :: Lude.Maybe Lude.Text,
    -- | Tag(s) to add to the new resource.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateConnectorDefinition' with the minimum fields required to make a request.
--
-- * 'amznClientToken' - A client token used to correlate requests and responses.
-- * 'initialVersion' - Information about the initial version of the connector definition.
-- * 'name' - The name of the connector definition.
-- * 'tags' - Tag(s) to add to the new resource.
mkCreateConnectorDefinition ::
  CreateConnectorDefinition
mkCreateConnectorDefinition =
  CreateConnectorDefinition'
    { amznClientToken = Lude.Nothing,
      initialVersion = Lude.Nothing,
      name = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdAmznClientToken :: Lens.Lens' CreateConnectorDefinition (Lude.Maybe Lude.Text)
ccdAmznClientToken = Lens.lens (amznClientToken :: CreateConnectorDefinition -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: CreateConnectorDefinition)
{-# DEPRECATED ccdAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | Information about the initial version of the connector definition.
--
-- /Note:/ Consider using 'initialVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdInitialVersion :: Lens.Lens' CreateConnectorDefinition (Lude.Maybe ConnectorDefinitionVersion)
ccdInitialVersion = Lens.lens (initialVersion :: CreateConnectorDefinition -> Lude.Maybe ConnectorDefinitionVersion) (\s a -> s {initialVersion = a} :: CreateConnectorDefinition)
{-# DEPRECATED ccdInitialVersion "Use generic-lens or generic-optics with 'initialVersion' instead." #-}

-- | The name of the connector definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdName :: Lens.Lens' CreateConnectorDefinition (Lude.Maybe Lude.Text)
ccdName = Lens.lens (name :: CreateConnectorDefinition -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateConnectorDefinition)
{-# DEPRECATED ccdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) to add to the new resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdTags :: Lens.Lens' CreateConnectorDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ccdTags = Lens.lens (tags :: CreateConnectorDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateConnectorDefinition)
{-# DEPRECATED ccdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateConnectorDefinition where
  type
    Rs CreateConnectorDefinition =
      CreateConnectorDefinitionResponse
  request = Req.postJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateConnectorDefinitionResponse'
            Lude.<$> (x Lude..?> "LatestVersionArn")
            Lude.<*> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (x Lude..?> "LatestVersion")
            Lude.<*> (x Lude..?> "LastUpdatedTimestamp")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateConnectorDefinition where
  toHeaders CreateConnectorDefinition' {..} =
    Lude.mconcat
      [ "X-Amzn-Client-Token" Lude.=# amznClientToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateConnectorDefinition where
  toJSON CreateConnectorDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InitialVersion" Lude..=) Lude.<$> initialVersion,
            ("Name" Lude..=) Lude.<$> name,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateConnectorDefinition where
  toPath = Lude.const "/greengrass/definition/connectors"

instance Lude.ToQuery CreateConnectorDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateConnectorDefinitionResponse' smart constructor.
data CreateConnectorDefinitionResponse = CreateConnectorDefinitionResponse'
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

-- | Creates a value of 'CreateConnectorDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'latestVersionARN' - The ARN of the latest version associated with the definition.
-- * 'arn' - The ARN of the definition.
-- * 'name' - The name of the definition.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
-- * 'id' - The ID of the definition.
-- * 'latestVersion' - The ID of the latest version associated with the definition.
-- * 'lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
-- * 'responseStatus' - The response status code.
mkCreateConnectorDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateConnectorDefinitionResponse
mkCreateConnectorDefinitionResponse pResponseStatus_ =
  CreateConnectorDefinitionResponse'
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
ccdfrsLatestVersionARN :: Lens.Lens' CreateConnectorDefinitionResponse (Lude.Maybe Lude.Text)
ccdfrsLatestVersionARN = Lens.lens (latestVersionARN :: CreateConnectorDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersionARN = a} :: CreateConnectorDefinitionResponse)
{-# DEPRECATED ccdfrsLatestVersionARN "Use generic-lens or generic-optics with 'latestVersionARN' instead." #-}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdfrsARN :: Lens.Lens' CreateConnectorDefinitionResponse (Lude.Maybe Lude.Text)
ccdfrsARN = Lens.lens (arn :: CreateConnectorDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateConnectorDefinitionResponse)
{-# DEPRECATED ccdfrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdfrsName :: Lens.Lens' CreateConnectorDefinitionResponse (Lude.Maybe Lude.Text)
ccdfrsName = Lens.lens (name :: CreateConnectorDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateConnectorDefinitionResponse)
{-# DEPRECATED ccdfrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdfrsCreationTimestamp :: Lens.Lens' CreateConnectorDefinitionResponse (Lude.Maybe Lude.Text)
ccdfrsCreationTimestamp = Lens.lens (creationTimestamp :: CreateConnectorDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: CreateConnectorDefinitionResponse)
{-# DEPRECATED ccdfrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdfrsId :: Lens.Lens' CreateConnectorDefinitionResponse (Lude.Maybe Lude.Text)
ccdfrsId = Lens.lens (id :: CreateConnectorDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateConnectorDefinitionResponse)
{-# DEPRECATED ccdfrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdfrsLatestVersion :: Lens.Lens' CreateConnectorDefinitionResponse (Lude.Maybe Lude.Text)
ccdfrsLatestVersion = Lens.lens (latestVersion :: CreateConnectorDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersion = a} :: CreateConnectorDefinitionResponse)
{-# DEPRECATED ccdfrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdfrsLastUpdatedTimestamp :: Lens.Lens' CreateConnectorDefinitionResponse (Lude.Maybe Lude.Text)
ccdfrsLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: CreateConnectorDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedTimestamp = a} :: CreateConnectorDefinitionResponse)
{-# DEPRECATED ccdfrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdfrsResponseStatus :: Lens.Lens' CreateConnectorDefinitionResponse Lude.Int
ccdfrsResponseStatus = Lens.lens (responseStatus :: CreateConnectorDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateConnectorDefinitionResponse)
{-# DEPRECATED ccdfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
