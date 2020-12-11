{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetResourceDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a resource definition, including its creation time and latest version.
module Network.AWS.Greengrass.GetResourceDefinition
  ( -- * Creating a request
    GetResourceDefinition (..),
    mkGetResourceDefinition,

    -- ** Request lenses
    grdResourceDefinitionId,

    -- * Destructuring the response
    GetResourceDefinitionResponse (..),
    mkGetResourceDefinitionResponse,

    -- ** Response lenses
    grdrsLatestVersionARN,
    grdrsARN,
    grdrsName,
    grdrsCreationTimestamp,
    grdrsId,
    grdrsLatestVersion,
    grdrsLastUpdatedTimestamp,
    grdrsTags,
    grdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetResourceDefinition' smart constructor.
newtype GetResourceDefinition = GetResourceDefinition'
  { resourceDefinitionId ::
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

-- | Creates a value of 'GetResourceDefinition' with the minimum fields required to make a request.
--
-- * 'resourceDefinitionId' - The ID of the resource definition.
mkGetResourceDefinition ::
  -- | 'resourceDefinitionId'
  Lude.Text ->
  GetResourceDefinition
mkGetResourceDefinition pResourceDefinitionId_ =
  GetResourceDefinition'
    { resourceDefinitionId =
        pResourceDefinitionId_
    }

-- | The ID of the resource definition.
--
-- /Note:/ Consider using 'resourceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdResourceDefinitionId :: Lens.Lens' GetResourceDefinition Lude.Text
grdResourceDefinitionId = Lens.lens (resourceDefinitionId :: GetResourceDefinition -> Lude.Text) (\s a -> s {resourceDefinitionId = a} :: GetResourceDefinition)
{-# DEPRECATED grdResourceDefinitionId "Use generic-lens or generic-optics with 'resourceDefinitionId' instead." #-}

instance Lude.AWSRequest GetResourceDefinition where
  type Rs GetResourceDefinition = GetResourceDefinitionResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetResourceDefinitionResponse'
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

instance Lude.ToHeaders GetResourceDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetResourceDefinition where
  toPath GetResourceDefinition' {..} =
    Lude.mconcat
      [ "/greengrass/definition/resources/",
        Lude.toBS resourceDefinitionId
      ]

instance Lude.ToQuery GetResourceDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetResourceDefinitionResponse' smart constructor.
data GetResourceDefinitionResponse = GetResourceDefinitionResponse'
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

-- | Creates a value of 'GetResourceDefinitionResponse' with the minimum fields required to make a request.
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
mkGetResourceDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetResourceDefinitionResponse
mkGetResourceDefinitionResponse pResponseStatus_ =
  GetResourceDefinitionResponse'
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
grdrsLatestVersionARN :: Lens.Lens' GetResourceDefinitionResponse (Lude.Maybe Lude.Text)
grdrsLatestVersionARN = Lens.lens (latestVersionARN :: GetResourceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersionARN = a} :: GetResourceDefinitionResponse)
{-# DEPRECATED grdrsLatestVersionARN "Use generic-lens or generic-optics with 'latestVersionARN' instead." #-}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrsARN :: Lens.Lens' GetResourceDefinitionResponse (Lude.Maybe Lude.Text)
grdrsARN = Lens.lens (arn :: GetResourceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GetResourceDefinitionResponse)
{-# DEPRECATED grdrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrsName :: Lens.Lens' GetResourceDefinitionResponse (Lude.Maybe Lude.Text)
grdrsName = Lens.lens (name :: GetResourceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetResourceDefinitionResponse)
{-# DEPRECATED grdrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrsCreationTimestamp :: Lens.Lens' GetResourceDefinitionResponse (Lude.Maybe Lude.Text)
grdrsCreationTimestamp = Lens.lens (creationTimestamp :: GetResourceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: GetResourceDefinitionResponse)
{-# DEPRECATED grdrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrsId :: Lens.Lens' GetResourceDefinitionResponse (Lude.Maybe Lude.Text)
grdrsId = Lens.lens (id :: GetResourceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: GetResourceDefinitionResponse)
{-# DEPRECATED grdrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrsLatestVersion :: Lens.Lens' GetResourceDefinitionResponse (Lude.Maybe Lude.Text)
grdrsLatestVersion = Lens.lens (latestVersion :: GetResourceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersion = a} :: GetResourceDefinitionResponse)
{-# DEPRECATED grdrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrsLastUpdatedTimestamp :: Lens.Lens' GetResourceDefinitionResponse (Lude.Maybe Lude.Text)
grdrsLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: GetResourceDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedTimestamp = a} :: GetResourceDefinitionResponse)
{-# DEPRECATED grdrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrsTags :: Lens.Lens' GetResourceDefinitionResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
grdrsTags = Lens.lens (tags :: GetResourceDefinitionResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: GetResourceDefinitionResponse)
{-# DEPRECATED grdrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrsResponseStatus :: Lens.Lens' GetResourceDefinitionResponse Lude.Int
grdrsResponseStatus = Lens.lens (responseStatus :: GetResourceDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetResourceDefinitionResponse)
{-# DEPRECATED grdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
