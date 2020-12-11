{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    cAmznClientToken,
    cInitialVersion,
    cName,
    cTags,

    -- * Destructuring the response
    CreateConnectorDefinitionResponse (..),
    mkCreateConnectorDefinitionResponse,

    -- ** Response lenses
    crersLatestVersionARN,
    crersARN,
    crersName,
    crersCreationTimestamp,
    crersId,
    crersLatestVersion,
    crersLastUpdatedTimestamp,
    crersResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateConnectorDefinition' smart constructor.
data CreateConnectorDefinition = CreateConnectorDefinition'
  { amznClientToken ::
      Lude.Maybe Lude.Text,
    initialVersion ::
      Lude.Maybe ConnectorDefinitionVersion,
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
cAmznClientToken :: Lens.Lens' CreateConnectorDefinition (Lude.Maybe Lude.Text)
cAmznClientToken = Lens.lens (amznClientToken :: CreateConnectorDefinition -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: CreateConnectorDefinition)
{-# DEPRECATED cAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | Information about the initial version of the connector definition.
--
-- /Note:/ Consider using 'initialVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cInitialVersion :: Lens.Lens' CreateConnectorDefinition (Lude.Maybe ConnectorDefinitionVersion)
cInitialVersion = Lens.lens (initialVersion :: CreateConnectorDefinition -> Lude.Maybe ConnectorDefinitionVersion) (\s a -> s {initialVersion = a} :: CreateConnectorDefinition)
{-# DEPRECATED cInitialVersion "Use generic-lens or generic-optics with 'initialVersion' instead." #-}

-- | The name of the connector definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' CreateConnectorDefinition (Lude.Maybe Lude.Text)
cName = Lens.lens (name :: CreateConnectorDefinition -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateConnectorDefinition)
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) to add to the new resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CreateConnectorDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cTags = Lens.lens (tags :: CreateConnectorDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateConnectorDefinition)
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

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
  { latestVersionARN ::
      Lude.Maybe Lude.Text,
    arn ::
      Lude.Maybe Lude.Text,
    name ::
      Lude.Maybe Lude.Text,
    creationTimestamp ::
      Lude.Maybe Lude.Text,
    id ::
      Lude.Maybe Lude.Text,
    latestVersion ::
      Lude.Maybe Lude.Text,
    lastUpdatedTimestamp ::
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

-- | Creates a value of 'CreateConnectorDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the definition.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
-- * 'id' - The ID of the definition.
-- * 'lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
-- * 'latestVersion' - The ID of the latest version associated with the definition.
-- * 'latestVersionARN' - The ARN of the latest version associated with the definition.
-- * 'name' - The name of the definition.
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
crersLatestVersionARN :: Lens.Lens' CreateConnectorDefinitionResponse (Lude.Maybe Lude.Text)
crersLatestVersionARN = Lens.lens (latestVersionARN :: CreateConnectorDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersionARN = a} :: CreateConnectorDefinitionResponse)
{-# DEPRECATED crersLatestVersionARN "Use generic-lens or generic-optics with 'latestVersionARN' instead." #-}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crersARN :: Lens.Lens' CreateConnectorDefinitionResponse (Lude.Maybe Lude.Text)
crersARN = Lens.lens (arn :: CreateConnectorDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateConnectorDefinitionResponse)
{-# DEPRECATED crersARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crersName :: Lens.Lens' CreateConnectorDefinitionResponse (Lude.Maybe Lude.Text)
crersName = Lens.lens (name :: CreateConnectorDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateConnectorDefinitionResponse)
{-# DEPRECATED crersName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crersCreationTimestamp :: Lens.Lens' CreateConnectorDefinitionResponse (Lude.Maybe Lude.Text)
crersCreationTimestamp = Lens.lens (creationTimestamp :: CreateConnectorDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: CreateConnectorDefinitionResponse)
{-# DEPRECATED crersCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crersId :: Lens.Lens' CreateConnectorDefinitionResponse (Lude.Maybe Lude.Text)
crersId = Lens.lens (id :: CreateConnectorDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateConnectorDefinitionResponse)
{-# DEPRECATED crersId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crersLatestVersion :: Lens.Lens' CreateConnectorDefinitionResponse (Lude.Maybe Lude.Text)
crersLatestVersion = Lens.lens (latestVersion :: CreateConnectorDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersion = a} :: CreateConnectorDefinitionResponse)
{-# DEPRECATED crersLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crersLastUpdatedTimestamp :: Lens.Lens' CreateConnectorDefinitionResponse (Lude.Maybe Lude.Text)
crersLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: CreateConnectorDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedTimestamp = a} :: CreateConnectorDefinitionResponse)
{-# DEPRECATED crersLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crersResponseStatus :: Lens.Lens' CreateConnectorDefinitionResponse Lude.Int
crersResponseStatus = Lens.lens (responseStatus :: CreateConnectorDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateConnectorDefinitionResponse)
{-# DEPRECATED crersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
