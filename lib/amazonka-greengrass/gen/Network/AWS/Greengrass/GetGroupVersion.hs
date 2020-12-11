{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetGroupVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a group version.
module Network.AWS.Greengrass.GetGroupVersion
  ( -- * Creating a request
    GetGroupVersion (..),
    mkGetGroupVersion,

    -- ** Request lenses
    ggvGroupVersionId,
    ggvGroupId,

    -- * Destructuring the response
    GetGroupVersionResponse (..),
    mkGetGroupVersionResponse,

    -- ** Response lenses
    ggvrsDefinition,
    ggvrsARN,
    ggvrsCreationTimestamp,
    ggvrsVersion,
    ggvrsId,
    ggvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetGroupVersion' smart constructor.
data GetGroupVersion = GetGroupVersion'
  { groupVersionId ::
      Lude.Text,
    groupId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroupVersion' with the minimum fields required to make a request.
--
-- * 'groupId' - The ID of the Greengrass group.
-- * 'groupVersionId' - The ID of the group version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListGroupVersions'' requests. If the version is the last one that was associated with a group, the value also maps to the ''LatestVersion'' property of the corresponding ''GroupInformation'' object.
mkGetGroupVersion ::
  -- | 'groupVersionId'
  Lude.Text ->
  -- | 'groupId'
  Lude.Text ->
  GetGroupVersion
mkGetGroupVersion pGroupVersionId_ pGroupId_ =
  GetGroupVersion'
    { groupVersionId = pGroupVersionId_,
      groupId = pGroupId_
    }

-- | The ID of the group version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListGroupVersions'' requests. If the version is the last one that was associated with a group, the value also maps to the ''LatestVersion'' property of the corresponding ''GroupInformation'' object.
--
-- /Note:/ Consider using 'groupVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggvGroupVersionId :: Lens.Lens' GetGroupVersion Lude.Text
ggvGroupVersionId = Lens.lens (groupVersionId :: GetGroupVersion -> Lude.Text) (\s a -> s {groupVersionId = a} :: GetGroupVersion)
{-# DEPRECATED ggvGroupVersionId "Use generic-lens or generic-optics with 'groupVersionId' instead." #-}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggvGroupId :: Lens.Lens' GetGroupVersion Lude.Text
ggvGroupId = Lens.lens (groupId :: GetGroupVersion -> Lude.Text) (\s a -> s {groupId = a} :: GetGroupVersion)
{-# DEPRECATED ggvGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

instance Lude.AWSRequest GetGroupVersion where
  type Rs GetGroupVersion = GetGroupVersionResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetGroupVersionResponse'
            Lude.<$> (x Lude..?> "Definition")
            Lude.<*> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Version")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetGroupVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetGroupVersion where
  toPath GetGroupVersion' {..} =
    Lude.mconcat
      [ "/greengrass/groups/",
        Lude.toBS groupId,
        "/versions/",
        Lude.toBS groupVersionId
      ]

instance Lude.ToQuery GetGroupVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetGroupVersionResponse' smart constructor.
data GetGroupVersionResponse = GetGroupVersionResponse'
  { definition ::
      Lude.Maybe GroupVersion,
    arn :: Lude.Maybe Lude.Text,
    creationTimestamp :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetGroupVersionResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the group version.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the group version was created.
-- * 'definition' - Information about the group version definition.
-- * 'id' - The ID of the group that the version is associated with.
-- * 'responseStatus' - The response status code.
-- * 'version' - The ID of the group version.
mkGetGroupVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetGroupVersionResponse
mkGetGroupVersionResponse pResponseStatus_ =
  GetGroupVersionResponse'
    { definition = Lude.Nothing,
      arn = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the group version definition.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggvrsDefinition :: Lens.Lens' GetGroupVersionResponse (Lude.Maybe GroupVersion)
ggvrsDefinition = Lens.lens (definition :: GetGroupVersionResponse -> Lude.Maybe GroupVersion) (\s a -> s {definition = a} :: GetGroupVersionResponse)
{-# DEPRECATED ggvrsDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The ARN of the group version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggvrsARN :: Lens.Lens' GetGroupVersionResponse (Lude.Maybe Lude.Text)
ggvrsARN = Lens.lens (arn :: GetGroupVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GetGroupVersionResponse)
{-# DEPRECATED ggvrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the group version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggvrsCreationTimestamp :: Lens.Lens' GetGroupVersionResponse (Lude.Maybe Lude.Text)
ggvrsCreationTimestamp = Lens.lens (creationTimestamp :: GetGroupVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: GetGroupVersionResponse)
{-# DEPRECATED ggvrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the group version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggvrsVersion :: Lens.Lens' GetGroupVersionResponse (Lude.Maybe Lude.Text)
ggvrsVersion = Lens.lens (version :: GetGroupVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: GetGroupVersionResponse)
{-# DEPRECATED ggvrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The ID of the group that the version is associated with.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggvrsId :: Lens.Lens' GetGroupVersionResponse (Lude.Maybe Lude.Text)
ggvrsId = Lens.lens (id :: GetGroupVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: GetGroupVersionResponse)
{-# DEPRECATED ggvrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggvrsResponseStatus :: Lens.Lens' GetGroupVersionResponse Lude.Int
ggvrsResponseStatus = Lens.lens (responseStatus :: GetGroupVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetGroupVersionResponse)
{-# DEPRECATED ggvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
