{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateConnectorDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a connector definition which has already been defined.
module Network.AWS.Greengrass.CreateConnectorDefinitionVersion
  ( -- * Creating a request
    CreateConnectorDefinitionVersion (..),
    mkCreateConnectorDefinitionVersion,

    -- ** Request lenses
    ccdvfAmznClientToken,
    ccdvfConnectors,
    ccdvfConnectorDefinitionId,

    -- * Destructuring the response
    CreateConnectorDefinitionVersionResponse (..),
    mkCreateConnectorDefinitionVersionResponse,

    -- ** Response lenses
    ccdvrsARN,
    ccdvrsCreationTimestamp,
    ccdvrsVersion,
    ccdvrsId,
    ccdvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateConnectorDefinitionVersion' smart constructor.
data CreateConnectorDefinitionVersion = CreateConnectorDefinitionVersion'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Lude.Maybe Lude.Text,
    -- | A list of references to connectors in this version, with their corresponding configuration settings.
    connectors :: Lude.Maybe [Connector],
    -- | The ID of the connector definition.
    connectorDefinitionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateConnectorDefinitionVersion' with the minimum fields required to make a request.
--
-- * 'amznClientToken' - A client token used to correlate requests and responses.
-- * 'connectors' - A list of references to connectors in this version, with their corresponding configuration settings.
-- * 'connectorDefinitionId' - The ID of the connector definition.
mkCreateConnectorDefinitionVersion ::
  -- | 'connectorDefinitionId'
  Lude.Text ->
  CreateConnectorDefinitionVersion
mkCreateConnectorDefinitionVersion pConnectorDefinitionId_ =
  CreateConnectorDefinitionVersion'
    { amznClientToken = Lude.Nothing,
      connectors = Lude.Nothing,
      connectorDefinitionId = pConnectorDefinitionId_
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdvfAmznClientToken :: Lens.Lens' CreateConnectorDefinitionVersion (Lude.Maybe Lude.Text)
ccdvfAmznClientToken = Lens.lens (amznClientToken :: CreateConnectorDefinitionVersion -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: CreateConnectorDefinitionVersion)
{-# DEPRECATED ccdvfAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | A list of references to connectors in this version, with their corresponding configuration settings.
--
-- /Note:/ Consider using 'connectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdvfConnectors :: Lens.Lens' CreateConnectorDefinitionVersion (Lude.Maybe [Connector])
ccdvfConnectors = Lens.lens (connectors :: CreateConnectorDefinitionVersion -> Lude.Maybe [Connector]) (\s a -> s {connectors = a} :: CreateConnectorDefinitionVersion)
{-# DEPRECATED ccdvfConnectors "Use generic-lens or generic-optics with 'connectors' instead." #-}

-- | The ID of the connector definition.
--
-- /Note:/ Consider using 'connectorDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdvfConnectorDefinitionId :: Lens.Lens' CreateConnectorDefinitionVersion Lude.Text
ccdvfConnectorDefinitionId = Lens.lens (connectorDefinitionId :: CreateConnectorDefinitionVersion -> Lude.Text) (\s a -> s {connectorDefinitionId = a} :: CreateConnectorDefinitionVersion)
{-# DEPRECATED ccdvfConnectorDefinitionId "Use generic-lens or generic-optics with 'connectorDefinitionId' instead." #-}

instance Lude.AWSRequest CreateConnectorDefinitionVersion where
  type
    Rs CreateConnectorDefinitionVersion =
      CreateConnectorDefinitionVersionResponse
  request = Req.postJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateConnectorDefinitionVersionResponse'
            Lude.<$> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Version")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateConnectorDefinitionVersion where
  toHeaders CreateConnectorDefinitionVersion' {..} =
    Lude.mconcat
      [ "X-Amzn-Client-Token" Lude.=# amznClientToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateConnectorDefinitionVersion where
  toJSON CreateConnectorDefinitionVersion' {..} =
    Lude.object
      (Lude.catMaybes [("Connectors" Lude..=) Lude.<$> connectors])

instance Lude.ToPath CreateConnectorDefinitionVersion where
  toPath CreateConnectorDefinitionVersion' {..} =
    Lude.mconcat
      [ "/greengrass/definition/connectors/",
        Lude.toBS connectorDefinitionId,
        "/versions"
      ]

instance Lude.ToQuery CreateConnectorDefinitionVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateConnectorDefinitionVersionResponse' smart constructor.
data CreateConnectorDefinitionVersionResponse = CreateConnectorDefinitionVersionResponse'
  { -- | The ARN of the version.
    arn :: Lude.Maybe Lude.Text,
    -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Lude.Maybe Lude.Text,
    -- | The ID of the version.
    version :: Lude.Maybe Lude.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateConnectorDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the version.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
-- * 'version' - The ID of the version.
-- * 'id' - The ID of the parent definition that the version is associated with.
-- * 'responseStatus' - The response status code.
mkCreateConnectorDefinitionVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateConnectorDefinitionVersionResponse
mkCreateConnectorDefinitionVersionResponse pResponseStatus_ =
  CreateConnectorDefinitionVersionResponse'
    { arn = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdvrsARN :: Lens.Lens' CreateConnectorDefinitionVersionResponse (Lude.Maybe Lude.Text)
ccdvrsARN = Lens.lens (arn :: CreateConnectorDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateConnectorDefinitionVersionResponse)
{-# DEPRECATED ccdvrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdvrsCreationTimestamp :: Lens.Lens' CreateConnectorDefinitionVersionResponse (Lude.Maybe Lude.Text)
ccdvrsCreationTimestamp = Lens.lens (creationTimestamp :: CreateConnectorDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: CreateConnectorDefinitionVersionResponse)
{-# DEPRECATED ccdvrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdvrsVersion :: Lens.Lens' CreateConnectorDefinitionVersionResponse (Lude.Maybe Lude.Text)
ccdvrsVersion = Lens.lens (version :: CreateConnectorDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: CreateConnectorDefinitionVersionResponse)
{-# DEPRECATED ccdvrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The ID of the parent definition that the version is associated with.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdvrsId :: Lens.Lens' CreateConnectorDefinitionVersionResponse (Lude.Maybe Lude.Text)
ccdvrsId = Lens.lens (id :: CreateConnectorDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateConnectorDefinitionVersionResponse)
{-# DEPRECATED ccdvrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdvrsResponseStatus :: Lens.Lens' CreateConnectorDefinitionVersionResponse Lude.Int
ccdvrsResponseStatus = Lens.lens (responseStatus :: CreateConnectorDefinitionVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateConnectorDefinitionVersionResponse)
{-# DEPRECATED ccdvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
