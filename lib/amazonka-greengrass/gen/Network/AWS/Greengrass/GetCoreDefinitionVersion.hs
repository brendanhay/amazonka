{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetCoreDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a core definition version.
module Network.AWS.Greengrass.GetCoreDefinitionVersion
  ( -- * Creating a request
    GetCoreDefinitionVersion (..),
    mkGetCoreDefinitionVersion,

    -- ** Request lenses
    gcdvCoreDefinitionId,
    gcdvCoreDefinitionVersionId,

    -- * Destructuring the response
    GetCoreDefinitionVersionResponse (..),
    mkGetCoreDefinitionVersionResponse,

    -- ** Response lenses
    gcdvfrsDefinition,
    gcdvfrsARN,
    gcdvfrsNextToken,
    gcdvfrsCreationTimestamp,
    gcdvfrsVersion,
    gcdvfrsId,
    gcdvfrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCoreDefinitionVersion' smart constructor.
data GetCoreDefinitionVersion = GetCoreDefinitionVersion'
  { -- | The ID of the core definition.
    coreDefinitionId :: Lude.Text,
    -- | The ID of the core definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListCoreDefinitionVersions'' requests. If the version is the last one that was associated with a core definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
    coreDefinitionVersionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCoreDefinitionVersion' with the minimum fields required to make a request.
--
-- * 'coreDefinitionId' - The ID of the core definition.
-- * 'coreDefinitionVersionId' - The ID of the core definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListCoreDefinitionVersions'' requests. If the version is the last one that was associated with a core definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
mkGetCoreDefinitionVersion ::
  -- | 'coreDefinitionId'
  Lude.Text ->
  -- | 'coreDefinitionVersionId'
  Lude.Text ->
  GetCoreDefinitionVersion
mkGetCoreDefinitionVersion
  pCoreDefinitionId_
  pCoreDefinitionVersionId_ =
    GetCoreDefinitionVersion'
      { coreDefinitionId = pCoreDefinitionId_,
        coreDefinitionVersionId = pCoreDefinitionVersionId_
      }

-- | The ID of the core definition.
--
-- /Note:/ Consider using 'coreDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvCoreDefinitionId :: Lens.Lens' GetCoreDefinitionVersion Lude.Text
gcdvCoreDefinitionId = Lens.lens (coreDefinitionId :: GetCoreDefinitionVersion -> Lude.Text) (\s a -> s {coreDefinitionId = a} :: GetCoreDefinitionVersion)
{-# DEPRECATED gcdvCoreDefinitionId "Use generic-lens or generic-optics with 'coreDefinitionId' instead." #-}

-- | The ID of the core definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListCoreDefinitionVersions'' requests. If the version is the last one that was associated with a core definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
--
-- /Note:/ Consider using 'coreDefinitionVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvCoreDefinitionVersionId :: Lens.Lens' GetCoreDefinitionVersion Lude.Text
gcdvCoreDefinitionVersionId = Lens.lens (coreDefinitionVersionId :: GetCoreDefinitionVersion -> Lude.Text) (\s a -> s {coreDefinitionVersionId = a} :: GetCoreDefinitionVersion)
{-# DEPRECATED gcdvCoreDefinitionVersionId "Use generic-lens or generic-optics with 'coreDefinitionVersionId' instead." #-}

instance Lude.AWSRequest GetCoreDefinitionVersion where
  type Rs GetCoreDefinitionVersion = GetCoreDefinitionVersionResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCoreDefinitionVersionResponse'
            Lude.<$> (x Lude..?> "Definition")
            Lude.<*> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Version")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCoreDefinitionVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetCoreDefinitionVersion where
  toPath GetCoreDefinitionVersion' {..} =
    Lude.mconcat
      [ "/greengrass/definition/cores/",
        Lude.toBS coreDefinitionId,
        "/versions/",
        Lude.toBS coreDefinitionVersionId
      ]

instance Lude.ToQuery GetCoreDefinitionVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCoreDefinitionVersionResponse' smart constructor.
data GetCoreDefinitionVersionResponse = GetCoreDefinitionVersionResponse'
  { -- | Information about the core definition version.
    definition :: Lude.Maybe CoreDefinitionVersion,
    -- | The ARN of the core definition version.
    arn :: Lude.Maybe Lude.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The time, in milliseconds since the epoch, when the core definition version was created.
    creationTimestamp :: Lude.Maybe Lude.Text,
    -- | The version of the core definition version.
    version :: Lude.Maybe Lude.Text,
    -- | The ID of the core definition version.
    id :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCoreDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- * 'definition' - Information about the core definition version.
-- * 'arn' - The ARN of the core definition version.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the core definition version was created.
-- * 'version' - The version of the core definition version.
-- * 'id' - The ID of the core definition version.
-- * 'responseStatus' - The response status code.
mkGetCoreDefinitionVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCoreDefinitionVersionResponse
mkGetCoreDefinitionVersionResponse pResponseStatus_ =
  GetCoreDefinitionVersionResponse'
    { definition = Lude.Nothing,
      arn = Lude.Nothing,
      nextToken = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the core definition version.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvfrsDefinition :: Lens.Lens' GetCoreDefinitionVersionResponse (Lude.Maybe CoreDefinitionVersion)
gcdvfrsDefinition = Lens.lens (definition :: GetCoreDefinitionVersionResponse -> Lude.Maybe CoreDefinitionVersion) (\s a -> s {definition = a} :: GetCoreDefinitionVersionResponse)
{-# DEPRECATED gcdvfrsDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The ARN of the core definition version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvfrsARN :: Lens.Lens' GetCoreDefinitionVersionResponse (Lude.Maybe Lude.Text)
gcdvfrsARN = Lens.lens (arn :: GetCoreDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GetCoreDefinitionVersionResponse)
{-# DEPRECATED gcdvfrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvfrsNextToken :: Lens.Lens' GetCoreDefinitionVersionResponse (Lude.Maybe Lude.Text)
gcdvfrsNextToken = Lens.lens (nextToken :: GetCoreDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetCoreDefinitionVersionResponse)
{-# DEPRECATED gcdvfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The time, in milliseconds since the epoch, when the core definition version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvfrsCreationTimestamp :: Lens.Lens' GetCoreDefinitionVersionResponse (Lude.Maybe Lude.Text)
gcdvfrsCreationTimestamp = Lens.lens (creationTimestamp :: GetCoreDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: GetCoreDefinitionVersionResponse)
{-# DEPRECATED gcdvfrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The version of the core definition version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvfrsVersion :: Lens.Lens' GetCoreDefinitionVersionResponse (Lude.Maybe Lude.Text)
gcdvfrsVersion = Lens.lens (version :: GetCoreDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: GetCoreDefinitionVersionResponse)
{-# DEPRECATED gcdvfrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The ID of the core definition version.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvfrsId :: Lens.Lens' GetCoreDefinitionVersionResponse (Lude.Maybe Lude.Text)
gcdvfrsId = Lens.lens (id :: GetCoreDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: GetCoreDefinitionVersionResponse)
{-# DEPRECATED gcdvfrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvfrsResponseStatus :: Lens.Lens' GetCoreDefinitionVersionResponse Lude.Int
gcdvfrsResponseStatus = Lens.lens (responseStatus :: GetCoreDefinitionVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCoreDefinitionVersionResponse)
{-# DEPRECATED gcdvfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
