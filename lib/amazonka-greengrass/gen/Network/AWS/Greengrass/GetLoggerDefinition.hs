{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetLoggerDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a logger definition.
module Network.AWS.Greengrass.GetLoggerDefinition
  ( -- * Creating a request
    GetLoggerDefinition (..),
    mkGetLoggerDefinition,

    -- ** Request lenses
    gldLoggerDefinitionId,

    -- * Destructuring the response
    GetLoggerDefinitionResponse (..),
    mkGetLoggerDefinitionResponse,

    -- ** Response lenses
    gldrsLatestVersionARN,
    gldrsARN,
    gldrsName,
    gldrsCreationTimestamp,
    gldrsId,
    gldrsLatestVersion,
    gldrsLastUpdatedTimestamp,
    gldrsTags,
    gldrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetLoggerDefinition' smart constructor.
newtype GetLoggerDefinition = GetLoggerDefinition'
  { loggerDefinitionId ::
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

-- | Creates a value of 'GetLoggerDefinition' with the minimum fields required to make a request.
--
-- * 'loggerDefinitionId' - The ID of the logger definition.
mkGetLoggerDefinition ::
  -- | 'loggerDefinitionId'
  Lude.Text ->
  GetLoggerDefinition
mkGetLoggerDefinition pLoggerDefinitionId_ =
  GetLoggerDefinition' {loggerDefinitionId = pLoggerDefinitionId_}

-- | The ID of the logger definition.
--
-- /Note:/ Consider using 'loggerDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldLoggerDefinitionId :: Lens.Lens' GetLoggerDefinition Lude.Text
gldLoggerDefinitionId = Lens.lens (loggerDefinitionId :: GetLoggerDefinition -> Lude.Text) (\s a -> s {loggerDefinitionId = a} :: GetLoggerDefinition)
{-# DEPRECATED gldLoggerDefinitionId "Use generic-lens or generic-optics with 'loggerDefinitionId' instead." #-}

instance Lude.AWSRequest GetLoggerDefinition where
  type Rs GetLoggerDefinition = GetLoggerDefinitionResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetLoggerDefinitionResponse'
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

instance Lude.ToHeaders GetLoggerDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetLoggerDefinition where
  toPath GetLoggerDefinition' {..} =
    Lude.mconcat
      ["/greengrass/definition/loggers/", Lude.toBS loggerDefinitionId]

instance Lude.ToQuery GetLoggerDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetLoggerDefinitionResponse' smart constructor.
data GetLoggerDefinitionResponse = GetLoggerDefinitionResponse'
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

-- | Creates a value of 'GetLoggerDefinitionResponse' with the minimum fields required to make a request.
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
mkGetLoggerDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetLoggerDefinitionResponse
mkGetLoggerDefinitionResponse pResponseStatus_ =
  GetLoggerDefinitionResponse'
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
gldrsLatestVersionARN :: Lens.Lens' GetLoggerDefinitionResponse (Lude.Maybe Lude.Text)
gldrsLatestVersionARN = Lens.lens (latestVersionARN :: GetLoggerDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersionARN = a} :: GetLoggerDefinitionResponse)
{-# DEPRECATED gldrsLatestVersionARN "Use generic-lens or generic-optics with 'latestVersionARN' instead." #-}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrsARN :: Lens.Lens' GetLoggerDefinitionResponse (Lude.Maybe Lude.Text)
gldrsARN = Lens.lens (arn :: GetLoggerDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GetLoggerDefinitionResponse)
{-# DEPRECATED gldrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrsName :: Lens.Lens' GetLoggerDefinitionResponse (Lude.Maybe Lude.Text)
gldrsName = Lens.lens (name :: GetLoggerDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetLoggerDefinitionResponse)
{-# DEPRECATED gldrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrsCreationTimestamp :: Lens.Lens' GetLoggerDefinitionResponse (Lude.Maybe Lude.Text)
gldrsCreationTimestamp = Lens.lens (creationTimestamp :: GetLoggerDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: GetLoggerDefinitionResponse)
{-# DEPRECATED gldrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrsId :: Lens.Lens' GetLoggerDefinitionResponse (Lude.Maybe Lude.Text)
gldrsId = Lens.lens (id :: GetLoggerDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: GetLoggerDefinitionResponse)
{-# DEPRECATED gldrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrsLatestVersion :: Lens.Lens' GetLoggerDefinitionResponse (Lude.Maybe Lude.Text)
gldrsLatestVersion = Lens.lens (latestVersion :: GetLoggerDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestVersion = a} :: GetLoggerDefinitionResponse)
{-# DEPRECATED gldrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrsLastUpdatedTimestamp :: Lens.Lens' GetLoggerDefinitionResponse (Lude.Maybe Lude.Text)
gldrsLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: GetLoggerDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedTimestamp = a} :: GetLoggerDefinitionResponse)
{-# DEPRECATED gldrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrsTags :: Lens.Lens' GetLoggerDefinitionResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gldrsTags = Lens.lens (tags :: GetLoggerDefinitionResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: GetLoggerDefinitionResponse)
{-# DEPRECATED gldrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrsResponseStatus :: Lens.Lens' GetLoggerDefinitionResponse Lude.Int
gldrsResponseStatus = Lens.lens (responseStatus :: GetLoggerDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetLoggerDefinitionResponse)
{-# DEPRECATED gldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
