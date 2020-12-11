{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetResourceDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a resource definition version, including which resources are included in the version.
module Network.AWS.Greengrass.GetResourceDefinitionVersion
  ( -- * Creating a request
    GetResourceDefinitionVersion (..),
    mkGetResourceDefinitionVersion,

    -- ** Request lenses
    grdvResourceDefinitionVersionId,
    grdvResourceDefinitionId,

    -- * Destructuring the response
    GetResourceDefinitionVersionResponse (..),
    mkGetResourceDefinitionVersionResponse,

    -- ** Response lenses
    grdvrsDefinition,
    grdvrsARN,
    grdvrsCreationTimestamp,
    grdvrsVersion,
    grdvrsId,
    grdvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetResourceDefinitionVersion' smart constructor.
data GetResourceDefinitionVersion = GetResourceDefinitionVersion'
  { resourceDefinitionVersionId ::
      Lude.Text,
    resourceDefinitionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetResourceDefinitionVersion' with the minimum fields required to make a request.
--
-- * 'resourceDefinitionId' - The ID of the resource definition.
-- * 'resourceDefinitionVersionId' - The ID of the resource definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListResourceDefinitionVersions'' requests. If the version is the last one that was associated with a resource definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
mkGetResourceDefinitionVersion ::
  -- | 'resourceDefinitionVersionId'
  Lude.Text ->
  -- | 'resourceDefinitionId'
  Lude.Text ->
  GetResourceDefinitionVersion
mkGetResourceDefinitionVersion
  pResourceDefinitionVersionId_
  pResourceDefinitionId_ =
    GetResourceDefinitionVersion'
      { resourceDefinitionVersionId =
          pResourceDefinitionVersionId_,
        resourceDefinitionId = pResourceDefinitionId_
      }

-- | The ID of the resource definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListResourceDefinitionVersions'' requests. If the version is the last one that was associated with a resource definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
--
-- /Note:/ Consider using 'resourceDefinitionVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdvResourceDefinitionVersionId :: Lens.Lens' GetResourceDefinitionVersion Lude.Text
grdvResourceDefinitionVersionId = Lens.lens (resourceDefinitionVersionId :: GetResourceDefinitionVersion -> Lude.Text) (\s a -> s {resourceDefinitionVersionId = a} :: GetResourceDefinitionVersion)
{-# DEPRECATED grdvResourceDefinitionVersionId "Use generic-lens or generic-optics with 'resourceDefinitionVersionId' instead." #-}

-- | The ID of the resource definition.
--
-- /Note:/ Consider using 'resourceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdvResourceDefinitionId :: Lens.Lens' GetResourceDefinitionVersion Lude.Text
grdvResourceDefinitionId = Lens.lens (resourceDefinitionId :: GetResourceDefinitionVersion -> Lude.Text) (\s a -> s {resourceDefinitionId = a} :: GetResourceDefinitionVersion)
{-# DEPRECATED grdvResourceDefinitionId "Use generic-lens or generic-optics with 'resourceDefinitionId' instead." #-}

instance Lude.AWSRequest GetResourceDefinitionVersion where
  type
    Rs GetResourceDefinitionVersion =
      GetResourceDefinitionVersionResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetResourceDefinitionVersionResponse'
            Lude.<$> (x Lude..?> "Definition")
            Lude.<*> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Version")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetResourceDefinitionVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetResourceDefinitionVersion where
  toPath GetResourceDefinitionVersion' {..} =
    Lude.mconcat
      [ "/greengrass/definition/resources/",
        Lude.toBS resourceDefinitionId,
        "/versions/",
        Lude.toBS resourceDefinitionVersionId
      ]

instance Lude.ToQuery GetResourceDefinitionVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetResourceDefinitionVersionResponse' smart constructor.
data GetResourceDefinitionVersionResponse = GetResourceDefinitionVersionResponse'
  { definition ::
      Lude.Maybe
        ResourceDefinitionVersion,
    arn ::
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

-- | Creates a value of 'GetResourceDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- * 'arn' - Arn of the resource definition version.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the resource definition version was created.
-- * 'definition' - Information about the definition.
-- * 'id' - The ID of the resource definition version.
-- * 'responseStatus' - The response status code.
-- * 'version' - The version of the resource definition version.
mkGetResourceDefinitionVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetResourceDefinitionVersionResponse
mkGetResourceDefinitionVersionResponse pResponseStatus_ =
  GetResourceDefinitionVersionResponse'
    { definition = Lude.Nothing,
      arn = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the definition.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdvrsDefinition :: Lens.Lens' GetResourceDefinitionVersionResponse (Lude.Maybe ResourceDefinitionVersion)
grdvrsDefinition = Lens.lens (definition :: GetResourceDefinitionVersionResponse -> Lude.Maybe ResourceDefinitionVersion) (\s a -> s {definition = a} :: GetResourceDefinitionVersionResponse)
{-# DEPRECATED grdvrsDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | Arn of the resource definition version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdvrsARN :: Lens.Lens' GetResourceDefinitionVersionResponse (Lude.Maybe Lude.Text)
grdvrsARN = Lens.lens (arn :: GetResourceDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GetResourceDefinitionVersionResponse)
{-# DEPRECATED grdvrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the resource definition version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdvrsCreationTimestamp :: Lens.Lens' GetResourceDefinitionVersionResponse (Lude.Maybe Lude.Text)
grdvrsCreationTimestamp = Lens.lens (creationTimestamp :: GetResourceDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: GetResourceDefinitionVersionResponse)
{-# DEPRECATED grdvrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The version of the resource definition version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdvrsVersion :: Lens.Lens' GetResourceDefinitionVersionResponse (Lude.Maybe Lude.Text)
grdvrsVersion = Lens.lens (version :: GetResourceDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: GetResourceDefinitionVersionResponse)
{-# DEPRECATED grdvrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The ID of the resource definition version.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdvrsId :: Lens.Lens' GetResourceDefinitionVersionResponse (Lude.Maybe Lude.Text)
grdvrsId = Lens.lens (id :: GetResourceDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: GetResourceDefinitionVersionResponse)
{-# DEPRECATED grdvrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdvrsResponseStatus :: Lens.Lens' GetResourceDefinitionVersionResponse Lude.Int
grdvrsResponseStatus = Lens.lens (responseStatus :: GetResourceDefinitionVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetResourceDefinitionVersionResponse)
{-# DEPRECATED grdvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
