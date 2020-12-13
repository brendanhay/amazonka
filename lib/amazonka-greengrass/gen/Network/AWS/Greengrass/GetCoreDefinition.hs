{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetCoreDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a core definition version.
module Network.AWS.Greengrass.GetCoreDefinition
  ( -- * Creating a request
    GetCoreDefinition (..),
    mkGetCoreDefinition,

    -- ** Request lenses
    gcdCoreDefinitionId,

    -- * Destructuring the response
    GetCoreDefinitionResponse (..),
    mkGetCoreDefinitionResponse,

    -- ** Response lenses
    gcdrsLatestVersionARN,
    gcdrsARN,
    gcdrsName,
    gcdrsCreationTimestamp,
    gcdrsId,
    gcdrsLatestVersion,
    gcdrsLastUpdatedTimestamp,
    gcdrsTags,
    gcdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCoreDefinition' smart constructor.
newtype GetCoreDefinition = GetCoreDefinition'
  { -- | The ID of the core definition.
    coreDefinitionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCoreDefinition' with the minimum fields required to make a request.
--
-- * 'coreDefinitionId' - The ID of the core definition.
mkGetCoreDefinition ::
  -- | 'coreDefinitionId'
  Lude.Text ->
  GetCoreDefinition
mkGetCoreDefinition pCoreDefinitionId_ =
  GetCoreDefinition' {coreDefinitionId = pCoreDefinitionId_}

-- | The ID of the core definition.
--
-- /Note:/ Consider using 'coreDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdCoreDefinitionId :: Lens.Lens' GetCoreDefinition Lude.Text
gcdCoreDefinitionId = Lens.lens (coreDefinitionId :: GetCoreDefinition -> Lude.Text) (\s a -> s {coreDefinitionId = a} :: GetCoreDefinition)
{-# DEPRECATED gcdCoreDefinitionId "Use generic-lens or generic-optics with 'coreDefinitionId' instead." #-}

instance Lude.AWSRequest GetCoreDefinition where
  type Rs GetCoreDefinition = GetCoreDefinitionResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCoreDefinitionResponse'
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

instance Lude.ToHeaders GetCoreDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetCoreDefinition where
  toPath GetCoreDefinition' {..} =
    Lude.mconcat
      ["/greengrass/definition/cores/", Lude.toBS coreDefinitionId]

instance Lude.ToQuery GetCoreDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCoreDefinitionResponse' smart constructor.
data GetCoreDefinitionResponse = GetCoreDefinitionResponse'
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
    -- | Tag(s) attached to the resource arn.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCoreDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'latestVersionARN' - The ARN of the latest version associated with the definition.
-- * 'arn' - The ARN of the definition.
-- * 'name' - The name of the definition.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
-- * 'id' - The ID of the definition.
-- * 'latestVersion' - The ID of the latest version associated with the definition.
-- * 'lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
-- * 'tags' - Tag(s) attached to the resource arn.
-- * 'responseStatus' - The response status code.
mkGetCoreDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCoreDefinitionResponse
mkGetCoreDefinitionResponse pResponseStatus_ =
  GetCoreDefinitionResponse'
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
gcdrsLatestVersionARN :: Lens.Lens' GetCoreDefinitionResponse (Lude.Maybe Lude.Text)
gcdrsLatestVersionARN = Lens.lens (latestVersionARN :: GetCoreDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersionARN = a} :: GetCoreDefinitionResponse)
{-# DEPRECATED gcdrsLatestVersionARN "Use generic-lens or generic-optics with 'latestVersionARN' instead." #-}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrsARN :: Lens.Lens' GetCoreDefinitionResponse (Lude.Maybe Lude.Text)
gcdrsARN = Lens.lens (arn :: GetCoreDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GetCoreDefinitionResponse)
{-# DEPRECATED gcdrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrsName :: Lens.Lens' GetCoreDefinitionResponse (Lude.Maybe Lude.Text)
gcdrsName = Lens.lens (name :: GetCoreDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetCoreDefinitionResponse)
{-# DEPRECATED gcdrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrsCreationTimestamp :: Lens.Lens' GetCoreDefinitionResponse (Lude.Maybe Lude.Text)
gcdrsCreationTimestamp = Lens.lens (creationTimestamp :: GetCoreDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: GetCoreDefinitionResponse)
{-# DEPRECATED gcdrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrsId :: Lens.Lens' GetCoreDefinitionResponse (Lude.Maybe Lude.Text)
gcdrsId = Lens.lens (id :: GetCoreDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: GetCoreDefinitionResponse)
{-# DEPRECATED gcdrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrsLatestVersion :: Lens.Lens' GetCoreDefinitionResponse (Lude.Maybe Lude.Text)
gcdrsLatestVersion = Lens.lens (latestVersion :: GetCoreDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersion = a} :: GetCoreDefinitionResponse)
{-# DEPRECATED gcdrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrsLastUpdatedTimestamp :: Lens.Lens' GetCoreDefinitionResponse (Lude.Maybe Lude.Text)
gcdrsLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: GetCoreDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedTimestamp = a} :: GetCoreDefinitionResponse)
{-# DEPRECATED gcdrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrsTags :: Lens.Lens' GetCoreDefinitionResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gcdrsTags = Lens.lens (tags :: GetCoreDefinitionResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: GetCoreDefinitionResponse)
{-# DEPRECATED gcdrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrsResponseStatus :: Lens.Lens' GetCoreDefinitionResponse Lude.Int
gcdrsResponseStatus = Lens.lens (responseStatus :: GetCoreDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCoreDefinitionResponse)
{-# DEPRECATED gcdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
