{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetConnectorDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a connector definition.
module Network.AWS.Greengrass.GetConnectorDefinition
  ( -- * Creating a request
    GetConnectorDefinition (..),
    mkGetConnectorDefinition,

    -- ** Request lenses
    gcdConnectorDefinitionId,

    -- * Destructuring the response
    GetConnectorDefinitionResponse (..),
    mkGetConnectorDefinitionResponse,

    -- ** Response lenses
    grsLatestVersionARN,
    grsARN,
    grsName,
    grsCreationTimestamp,
    grsId,
    grsLatestVersion,
    grsLastUpdatedTimestamp,
    grsTags,
    grsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetConnectorDefinition' smart constructor.
newtype GetConnectorDefinition = GetConnectorDefinition'
  { connectorDefinitionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConnectorDefinition' with the minimum fields required to make a request.
--
-- * 'connectorDefinitionId' - The ID of the connector definition.
mkGetConnectorDefinition ::
  -- | 'connectorDefinitionId'
  Lude.Text ->
  GetConnectorDefinition
mkGetConnectorDefinition pConnectorDefinitionId_ =
  GetConnectorDefinition'
    { connectorDefinitionId =
        pConnectorDefinitionId_
    }

-- | The ID of the connector definition.
--
-- /Note:/ Consider using 'connectorDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdConnectorDefinitionId :: Lens.Lens' GetConnectorDefinition Lude.Text
gcdConnectorDefinitionId = Lens.lens (connectorDefinitionId :: GetConnectorDefinition -> Lude.Text) (\s a -> s {connectorDefinitionId = a} :: GetConnectorDefinition)
{-# DEPRECATED gcdConnectorDefinitionId "Use generic-lens or generic-optics with 'connectorDefinitionId' instead." #-}

instance Lude.AWSRequest GetConnectorDefinition where
  type Rs GetConnectorDefinition = GetConnectorDefinitionResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetConnectorDefinitionResponse'
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

instance Lude.ToHeaders GetConnectorDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetConnectorDefinition where
  toPath GetConnectorDefinition' {..} =
    Lude.mconcat
      [ "/greengrass/definition/connectors/",
        Lude.toBS connectorDefinitionId
      ]

instance Lude.ToQuery GetConnectorDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetConnectorDefinitionResponse' smart constructor.
data GetConnectorDefinitionResponse = GetConnectorDefinitionResponse'
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
    tags ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
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

-- | Creates a value of 'GetConnectorDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the definition.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
-- * 'id' - The ID of the definition.
-- * 'lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
-- * 'latestVersion' - The ID of the latest version associated with the definition.
-- * 'latestVersionARN' - The ARN of the latest version associated with the definition.
-- * 'name' - The name of the definition.
-- * 'responseStatus' - The response status code.
-- * 'tags' - Tag(s) attached to the resource arn.
mkGetConnectorDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetConnectorDefinitionResponse
mkGetConnectorDefinitionResponse pResponseStatus_ =
  GetConnectorDefinitionResponse'
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
grsLatestVersionARN :: Lens.Lens' GetConnectorDefinitionResponse (Lude.Maybe Lude.Text)
grsLatestVersionARN = Lens.lens (latestVersionARN :: GetConnectorDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersionARN = a} :: GetConnectorDefinitionResponse)
{-# DEPRECATED grsLatestVersionARN "Use generic-lens or generic-optics with 'latestVersionARN' instead." #-}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsARN :: Lens.Lens' GetConnectorDefinitionResponse (Lude.Maybe Lude.Text)
grsARN = Lens.lens (arn :: GetConnectorDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GetConnectorDefinitionResponse)
{-# DEPRECATED grsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsName :: Lens.Lens' GetConnectorDefinitionResponse (Lude.Maybe Lude.Text)
grsName = Lens.lens (name :: GetConnectorDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetConnectorDefinitionResponse)
{-# DEPRECATED grsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsCreationTimestamp :: Lens.Lens' GetConnectorDefinitionResponse (Lude.Maybe Lude.Text)
grsCreationTimestamp = Lens.lens (creationTimestamp :: GetConnectorDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: GetConnectorDefinitionResponse)
{-# DEPRECATED grsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsId :: Lens.Lens' GetConnectorDefinitionResponse (Lude.Maybe Lude.Text)
grsId = Lens.lens (id :: GetConnectorDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: GetConnectorDefinitionResponse)
{-# DEPRECATED grsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsLatestVersion :: Lens.Lens' GetConnectorDefinitionResponse (Lude.Maybe Lude.Text)
grsLatestVersion = Lens.lens (latestVersion :: GetConnectorDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersion = a} :: GetConnectorDefinitionResponse)
{-# DEPRECATED grsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsLastUpdatedTimestamp :: Lens.Lens' GetConnectorDefinitionResponse (Lude.Maybe Lude.Text)
grsLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: GetConnectorDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedTimestamp = a} :: GetConnectorDefinitionResponse)
{-# DEPRECATED grsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsTags :: Lens.Lens' GetConnectorDefinitionResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
grsTags = Lens.lens (tags :: GetConnectorDefinitionResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: GetConnectorDefinitionResponse)
{-# DEPRECATED grsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetConnectorDefinitionResponse Lude.Int
grsResponseStatus = Lens.lens (responseStatus :: GetConnectorDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetConnectorDefinitionResponse)
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
