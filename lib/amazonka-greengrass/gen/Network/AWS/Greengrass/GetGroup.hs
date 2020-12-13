{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a group.
module Network.AWS.Greengrass.GetGroup
  ( -- * Creating a request
    GetGroup (..),
    mkGetGroup,

    -- ** Request lenses
    ggGroupId,

    -- * Destructuring the response
    GetGroupResponse (..),
    mkGetGroupResponse,

    -- ** Response lenses
    ggrsLatestVersionARN,
    ggrsARN,
    ggrsName,
    ggrsCreationTimestamp,
    ggrsId,
    ggrsLatestVersion,
    ggrsLastUpdatedTimestamp,
    ggrsTags,
    ggrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetGroup' smart constructor.
newtype GetGroup = GetGroup'
  { -- | The ID of the Greengrass group.
    groupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroup' with the minimum fields required to make a request.
--
-- * 'groupId' - The ID of the Greengrass group.
mkGetGroup ::
  -- | 'groupId'
  Lude.Text ->
  GetGroup
mkGetGroup pGroupId_ = GetGroup' {groupId = pGroupId_}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggGroupId :: Lens.Lens' GetGroup Lude.Text
ggGroupId = Lens.lens (groupId :: GetGroup -> Lude.Text) (\s a -> s {groupId = a} :: GetGroup)
{-# DEPRECATED ggGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

instance Lude.AWSRequest GetGroup where
  type Rs GetGroup = GetGroupResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetGroupResponse'
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

instance Lude.ToHeaders GetGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetGroup where
  toPath GetGroup' {..} =
    Lude.mconcat ["/greengrass/groups/", Lude.toBS groupId]

instance Lude.ToQuery GetGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
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

-- | Creates a value of 'GetGroupResponse' with the minimum fields required to make a request.
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
mkGetGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetGroupResponse
mkGetGroupResponse pResponseStatus_ =
  GetGroupResponse'
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
ggrsLatestVersionARN :: Lens.Lens' GetGroupResponse (Lude.Maybe Lude.Text)
ggrsLatestVersionARN = Lens.lens (latestVersionARN :: GetGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersionARN = a} :: GetGroupResponse)
{-# DEPRECATED ggrsLatestVersionARN "Use generic-lens or generic-optics with 'latestVersionARN' instead." #-}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsARN :: Lens.Lens' GetGroupResponse (Lude.Maybe Lude.Text)
ggrsARN = Lens.lens (arn :: GetGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GetGroupResponse)
{-# DEPRECATED ggrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsName :: Lens.Lens' GetGroupResponse (Lude.Maybe Lude.Text)
ggrsName = Lens.lens (name :: GetGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetGroupResponse)
{-# DEPRECATED ggrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsCreationTimestamp :: Lens.Lens' GetGroupResponse (Lude.Maybe Lude.Text)
ggrsCreationTimestamp = Lens.lens (creationTimestamp :: GetGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: GetGroupResponse)
{-# DEPRECATED ggrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsId :: Lens.Lens' GetGroupResponse (Lude.Maybe Lude.Text)
ggrsId = Lens.lens (id :: GetGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: GetGroupResponse)
{-# DEPRECATED ggrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsLatestVersion :: Lens.Lens' GetGroupResponse (Lude.Maybe Lude.Text)
ggrsLatestVersion = Lens.lens (latestVersion :: GetGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersion = a} :: GetGroupResponse)
{-# DEPRECATED ggrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsLastUpdatedTimestamp :: Lens.Lens' GetGroupResponse (Lude.Maybe Lude.Text)
ggrsLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: GetGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedTimestamp = a} :: GetGroupResponse)
{-# DEPRECATED ggrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsTags :: Lens.Lens' GetGroupResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ggrsTags = Lens.lens (tags :: GetGroupResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: GetGroupResponse)
{-# DEPRECATED ggrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsResponseStatus :: Lens.Lens' GetGroupResponse Lude.Int
ggrsResponseStatus = Lens.lens (responseStatus :: GetGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetGroupResponse)
{-# DEPRECATED ggrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
