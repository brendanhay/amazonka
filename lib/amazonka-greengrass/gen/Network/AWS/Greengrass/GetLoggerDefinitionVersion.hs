{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetLoggerDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a logger definition version.
module Network.AWS.Greengrass.GetLoggerDefinitionVersion
  ( -- * Creating a request
    GetLoggerDefinitionVersion (..),
    mkGetLoggerDefinitionVersion,

    -- ** Request lenses
    gldvNextToken,
    gldvLoggerDefinitionVersionId,
    gldvLoggerDefinitionId,

    -- * Destructuring the response
    GetLoggerDefinitionVersionResponse (..),
    mkGetLoggerDefinitionVersionResponse,

    -- ** Response lenses
    gldvrsDefinition,
    gldvrsARN,
    gldvrsCreationTimestamp,
    gldvrsVersion,
    gldvrsId,
    gldvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetLoggerDefinitionVersion' smart constructor.
data GetLoggerDefinitionVersion = GetLoggerDefinitionVersion'
  { nextToken ::
      Lude.Maybe Lude.Text,
    loggerDefinitionVersionId ::
      Lude.Text,
    loggerDefinitionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLoggerDefinitionVersion' with the minimum fields required to make a request.
--
-- * 'loggerDefinitionId' - The ID of the logger definition.
-- * 'loggerDefinitionVersionId' - The ID of the logger definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListLoggerDefinitionVersions'' requests. If the version is the last one that was associated with a logger definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
mkGetLoggerDefinitionVersion ::
  -- | 'loggerDefinitionVersionId'
  Lude.Text ->
  -- | 'loggerDefinitionId'
  Lude.Text ->
  GetLoggerDefinitionVersion
mkGetLoggerDefinitionVersion
  pLoggerDefinitionVersionId_
  pLoggerDefinitionId_ =
    GetLoggerDefinitionVersion'
      { nextToken = Lude.Nothing,
        loggerDefinitionVersionId = pLoggerDefinitionVersionId_,
        loggerDefinitionId = pLoggerDefinitionId_
      }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvNextToken :: Lens.Lens' GetLoggerDefinitionVersion (Lude.Maybe Lude.Text)
gldvNextToken = Lens.lens (nextToken :: GetLoggerDefinitionVersion -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetLoggerDefinitionVersion)
{-# DEPRECATED gldvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the logger definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListLoggerDefinitionVersions'' requests. If the version is the last one that was associated with a logger definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
--
-- /Note:/ Consider using 'loggerDefinitionVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvLoggerDefinitionVersionId :: Lens.Lens' GetLoggerDefinitionVersion Lude.Text
gldvLoggerDefinitionVersionId = Lens.lens (loggerDefinitionVersionId :: GetLoggerDefinitionVersion -> Lude.Text) (\s a -> s {loggerDefinitionVersionId = a} :: GetLoggerDefinitionVersion)
{-# DEPRECATED gldvLoggerDefinitionVersionId "Use generic-lens or generic-optics with 'loggerDefinitionVersionId' instead." #-}

-- | The ID of the logger definition.
--
-- /Note:/ Consider using 'loggerDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvLoggerDefinitionId :: Lens.Lens' GetLoggerDefinitionVersion Lude.Text
gldvLoggerDefinitionId = Lens.lens (loggerDefinitionId :: GetLoggerDefinitionVersion -> Lude.Text) (\s a -> s {loggerDefinitionId = a} :: GetLoggerDefinitionVersion)
{-# DEPRECATED gldvLoggerDefinitionId "Use generic-lens or generic-optics with 'loggerDefinitionId' instead." #-}

instance Lude.AWSRequest GetLoggerDefinitionVersion where
  type
    Rs GetLoggerDefinitionVersion =
      GetLoggerDefinitionVersionResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetLoggerDefinitionVersionResponse'
            Lude.<$> (x Lude..?> "Definition")
            Lude.<*> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Version")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetLoggerDefinitionVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetLoggerDefinitionVersion where
  toPath GetLoggerDefinitionVersion' {..} =
    Lude.mconcat
      [ "/greengrass/definition/loggers/",
        Lude.toBS loggerDefinitionId,
        "/versions/",
        Lude.toBS loggerDefinitionVersionId
      ]

instance Lude.ToQuery GetLoggerDefinitionVersion where
  toQuery GetLoggerDefinitionVersion' {..} =
    Lude.mconcat ["NextToken" Lude.=: nextToken]

-- | /See:/ 'mkGetLoggerDefinitionVersionResponse' smart constructor.
data GetLoggerDefinitionVersionResponse = GetLoggerDefinitionVersionResponse'
  { definition ::
      Lude.Maybe
        LoggerDefinitionVersion,
    arn ::
      Lude.Maybe Lude.Text,
    creationTimestamp ::
      Lude.Maybe Lude.Text,
    version ::
      Lude.Maybe Lude.Text,
    id ::
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

-- | Creates a value of 'GetLoggerDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the logger definition version.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the logger definition version was created.
-- * 'definition' - Information about the logger definition version.
-- * 'id' - The ID of the logger definition version.
-- * 'responseStatus' - The response status code.
-- * 'version' - The version of the logger definition version.
mkGetLoggerDefinitionVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetLoggerDefinitionVersionResponse
mkGetLoggerDefinitionVersionResponse pResponseStatus_ =
  GetLoggerDefinitionVersionResponse'
    { definition = Lude.Nothing,
      arn = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the logger definition version.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvrsDefinition :: Lens.Lens' GetLoggerDefinitionVersionResponse (Lude.Maybe LoggerDefinitionVersion)
gldvrsDefinition = Lens.lens (definition :: GetLoggerDefinitionVersionResponse -> Lude.Maybe LoggerDefinitionVersion) (\s a -> s {definition = a} :: GetLoggerDefinitionVersionResponse)
{-# DEPRECATED gldvrsDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The ARN of the logger definition version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvrsARN :: Lens.Lens' GetLoggerDefinitionVersionResponse (Lude.Maybe Lude.Text)
gldvrsARN = Lens.lens (arn :: GetLoggerDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GetLoggerDefinitionVersionResponse)
{-# DEPRECATED gldvrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the logger definition version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvrsCreationTimestamp :: Lens.Lens' GetLoggerDefinitionVersionResponse (Lude.Maybe Lude.Text)
gldvrsCreationTimestamp = Lens.lens (creationTimestamp :: GetLoggerDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: GetLoggerDefinitionVersionResponse)
{-# DEPRECATED gldvrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The version of the logger definition version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvrsVersion :: Lens.Lens' GetLoggerDefinitionVersionResponse (Lude.Maybe Lude.Text)
gldvrsVersion = Lens.lens (version :: GetLoggerDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: GetLoggerDefinitionVersionResponse)
{-# DEPRECATED gldvrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The ID of the logger definition version.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvrsId :: Lens.Lens' GetLoggerDefinitionVersionResponse (Lude.Maybe Lude.Text)
gldvrsId = Lens.lens (id :: GetLoggerDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: GetLoggerDefinitionVersionResponse)
{-# DEPRECATED gldvrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvrsResponseStatus :: Lens.Lens' GetLoggerDefinitionVersionResponse Lude.Int
gldvrsResponseStatus = Lens.lens (responseStatus :: GetLoggerDefinitionVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetLoggerDefinitionVersionResponse)
{-# DEPRECATED gldvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
