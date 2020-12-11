{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetConnectorDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a connector definition version, including the connectors that the version contains. Connectors are prebuilt modules that interact with local infrastructure, device protocols, AWS, and other cloud services.
module Network.AWS.Greengrass.GetConnectorDefinitionVersion
  ( -- * Creating a request
    GetConnectorDefinitionVersion (..),
    mkGetConnectorDefinitionVersion,

    -- ** Request lenses
    gcdvNextToken,
    gcdvConnectorDefinitionId,
    gcdvConnectorDefinitionVersionId,

    -- * Destructuring the response
    GetConnectorDefinitionVersionResponse (..),
    mkGetConnectorDefinitionVersionResponse,

    -- ** Response lenses
    gcdvrsDefinition,
    gcdvrsARN,
    gcdvrsNextToken,
    gcdvrsCreationTimestamp,
    gcdvrsVersion,
    gcdvrsId,
    gcdvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetConnectorDefinitionVersion' smart constructor.
data GetConnectorDefinitionVersion = GetConnectorDefinitionVersion'
  { nextToken ::
      Lude.Maybe Lude.Text,
    connectorDefinitionId ::
      Lude.Text,
    connectorDefinitionVersionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConnectorDefinitionVersion' with the minimum fields required to make a request.
--
-- * 'connectorDefinitionId' - The ID of the connector definition.
-- * 'connectorDefinitionVersionId' - The ID of the connector definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListConnectorDefinitionVersions'' requests. If the version is the last one that was associated with a connector definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
mkGetConnectorDefinitionVersion ::
  -- | 'connectorDefinitionId'
  Lude.Text ->
  -- | 'connectorDefinitionVersionId'
  Lude.Text ->
  GetConnectorDefinitionVersion
mkGetConnectorDefinitionVersion
  pConnectorDefinitionId_
  pConnectorDefinitionVersionId_ =
    GetConnectorDefinitionVersion'
      { nextToken = Lude.Nothing,
        connectorDefinitionId = pConnectorDefinitionId_,
        connectorDefinitionVersionId = pConnectorDefinitionVersionId_
      }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvNextToken :: Lens.Lens' GetConnectorDefinitionVersion (Lude.Maybe Lude.Text)
gcdvNextToken = Lens.lens (nextToken :: GetConnectorDefinitionVersion -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetConnectorDefinitionVersion)
{-# DEPRECATED gcdvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the connector definition.
--
-- /Note:/ Consider using 'connectorDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvConnectorDefinitionId :: Lens.Lens' GetConnectorDefinitionVersion Lude.Text
gcdvConnectorDefinitionId = Lens.lens (connectorDefinitionId :: GetConnectorDefinitionVersion -> Lude.Text) (\s a -> s {connectorDefinitionId = a} :: GetConnectorDefinitionVersion)
{-# DEPRECATED gcdvConnectorDefinitionId "Use generic-lens or generic-optics with 'connectorDefinitionId' instead." #-}

-- | The ID of the connector definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListConnectorDefinitionVersions'' requests. If the version is the last one that was associated with a connector definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
--
-- /Note:/ Consider using 'connectorDefinitionVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvConnectorDefinitionVersionId :: Lens.Lens' GetConnectorDefinitionVersion Lude.Text
gcdvConnectorDefinitionVersionId = Lens.lens (connectorDefinitionVersionId :: GetConnectorDefinitionVersion -> Lude.Text) (\s a -> s {connectorDefinitionVersionId = a} :: GetConnectorDefinitionVersion)
{-# DEPRECATED gcdvConnectorDefinitionVersionId "Use generic-lens or generic-optics with 'connectorDefinitionVersionId' instead." #-}

instance Lude.AWSRequest GetConnectorDefinitionVersion where
  type
    Rs GetConnectorDefinitionVersion =
      GetConnectorDefinitionVersionResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetConnectorDefinitionVersionResponse'
            Lude.<$> (x Lude..?> "Definition")
            Lude.<*> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Version")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetConnectorDefinitionVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetConnectorDefinitionVersion where
  toPath GetConnectorDefinitionVersion' {..} =
    Lude.mconcat
      [ "/greengrass/definition/connectors/",
        Lude.toBS connectorDefinitionId,
        "/versions/",
        Lude.toBS connectorDefinitionVersionId
      ]

instance Lude.ToQuery GetConnectorDefinitionVersion where
  toQuery GetConnectorDefinitionVersion' {..} =
    Lude.mconcat ["NextToken" Lude.=: nextToken]

-- | /See:/ 'mkGetConnectorDefinitionVersionResponse' smart constructor.
data GetConnectorDefinitionVersionResponse = GetConnectorDefinitionVersionResponse'
  { definition ::
      Lude.Maybe
        ConnectorDefinitionVersion,
    arn ::
      Lude.Maybe
        Lude.Text,
    nextToken ::
      Lude.Maybe
        Lude.Text,
    creationTimestamp ::
      Lude.Maybe
        Lude.Text,
    version ::
      Lude.Maybe
        Lude.Text,
    id ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'GetConnectorDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the connector definition version.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the connector definition version was created.
-- * 'definition' - Information about the connector definition version.
-- * 'id' - The ID of the connector definition version.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'responseStatus' - The response status code.
-- * 'version' - The version of the connector definition version.
mkGetConnectorDefinitionVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetConnectorDefinitionVersionResponse
mkGetConnectorDefinitionVersionResponse pResponseStatus_ =
  GetConnectorDefinitionVersionResponse'
    { definition = Lude.Nothing,
      arn = Lude.Nothing,
      nextToken = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the connector definition version.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrsDefinition :: Lens.Lens' GetConnectorDefinitionVersionResponse (Lude.Maybe ConnectorDefinitionVersion)
gcdvrsDefinition = Lens.lens (definition :: GetConnectorDefinitionVersionResponse -> Lude.Maybe ConnectorDefinitionVersion) (\s a -> s {definition = a} :: GetConnectorDefinitionVersionResponse)
{-# DEPRECATED gcdvrsDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The ARN of the connector definition version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrsARN :: Lens.Lens' GetConnectorDefinitionVersionResponse (Lude.Maybe Lude.Text)
gcdvrsARN = Lens.lens (arn :: GetConnectorDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GetConnectorDefinitionVersionResponse)
{-# DEPRECATED gcdvrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrsNextToken :: Lens.Lens' GetConnectorDefinitionVersionResponse (Lude.Maybe Lude.Text)
gcdvrsNextToken = Lens.lens (nextToken :: GetConnectorDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetConnectorDefinitionVersionResponse)
{-# DEPRECATED gcdvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The time, in milliseconds since the epoch, when the connector definition version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrsCreationTimestamp :: Lens.Lens' GetConnectorDefinitionVersionResponse (Lude.Maybe Lude.Text)
gcdvrsCreationTimestamp = Lens.lens (creationTimestamp :: GetConnectorDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: GetConnectorDefinitionVersionResponse)
{-# DEPRECATED gcdvrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The version of the connector definition version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrsVersion :: Lens.Lens' GetConnectorDefinitionVersionResponse (Lude.Maybe Lude.Text)
gcdvrsVersion = Lens.lens (version :: GetConnectorDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: GetConnectorDefinitionVersionResponse)
{-# DEPRECATED gcdvrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The ID of the connector definition version.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrsId :: Lens.Lens' GetConnectorDefinitionVersionResponse (Lude.Maybe Lude.Text)
gcdvrsId = Lens.lens (id :: GetConnectorDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: GetConnectorDefinitionVersionResponse)
{-# DEPRECATED gcdvrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrsResponseStatus :: Lens.Lens' GetConnectorDefinitionVersionResponse Lude.Int
gcdvrsResponseStatus = Lens.lens (responseStatus :: GetConnectorDefinitionVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetConnectorDefinitionVersionResponse)
{-# DEPRECATED gcdvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
