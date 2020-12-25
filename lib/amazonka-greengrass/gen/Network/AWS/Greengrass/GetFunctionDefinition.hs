{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetFunctionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a Lambda function definition, including its creation time and latest version.
module Network.AWS.Greengrass.GetFunctionDefinition
  ( -- * Creating a request
    GetFunctionDefinition (..),
    mkGetFunctionDefinition,

    -- ** Request lenses
    gfdFunctionDefinitionId,

    -- * Destructuring the response
    GetFunctionDefinitionResponse (..),
    mkGetFunctionDefinitionResponse,

    -- ** Response lenses
    gfdrrsArn,
    gfdrrsCreationTimestamp,
    gfdrrsId,
    gfdrrsLastUpdatedTimestamp,
    gfdrrsLatestVersion,
    gfdrrsLatestVersionArn,
    gfdrrsName,
    gfdrrsTags,
    gfdrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFunctionDefinition' smart constructor.
newtype GetFunctionDefinition = GetFunctionDefinition'
  { -- | The ID of the Lambda function definition.
    functionDefinitionId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetFunctionDefinition' value with any optional fields omitted.
mkGetFunctionDefinition ::
  -- | 'functionDefinitionId'
  Core.Text ->
  GetFunctionDefinition
mkGetFunctionDefinition functionDefinitionId =
  GetFunctionDefinition' {functionDefinitionId}

-- | The ID of the Lambda function definition.
--
-- /Note:/ Consider using 'functionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdFunctionDefinitionId :: Lens.Lens' GetFunctionDefinition Core.Text
gfdFunctionDefinitionId = Lens.field @"functionDefinitionId"
{-# DEPRECATED gfdFunctionDefinitionId "Use generic-lens or generic-optics with 'functionDefinitionId' instead." #-}

instance Core.AWSRequest GetFunctionDefinition where
  type Rs GetFunctionDefinition = GetFunctionDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/functions/"
                Core.<> (Core.toText functionDefinitionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFunctionDefinitionResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "LastUpdatedTimestamp")
            Core.<*> (x Core..:? "LatestVersion")
            Core.<*> (x Core..:? "LatestVersionArn")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetFunctionDefinitionResponse' smart constructor.
data GetFunctionDefinitionResponse = GetFunctionDefinitionResponse'
  { -- | The ARN of the definition.
    arn :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the definition was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | The ID of the definition.
    id :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the definition was last updated.
    lastUpdatedTimestamp :: Core.Maybe Core.Text,
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Core.Maybe Core.Text,
    -- | The ARN of the latest version associated with the definition.
    latestVersionArn :: Core.Maybe Core.Text,
    -- | The name of the definition.
    name :: Core.Maybe Core.Text,
    -- | Tag(s) attached to the resource arn.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFunctionDefinitionResponse' value with any optional fields omitted.
mkGetFunctionDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetFunctionDefinitionResponse
mkGetFunctionDefinitionResponse responseStatus =
  GetFunctionDefinitionResponse'
    { arn = Core.Nothing,
      creationTimestamp = Core.Nothing,
      id = Core.Nothing,
      lastUpdatedTimestamp = Core.Nothing,
      latestVersion = Core.Nothing,
      latestVersionArn = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrrsArn :: Lens.Lens' GetFunctionDefinitionResponse (Core.Maybe Core.Text)
gfdrrsArn = Lens.field @"arn"
{-# DEPRECATED gfdrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrrsCreationTimestamp :: Lens.Lens' GetFunctionDefinitionResponse (Core.Maybe Core.Text)
gfdrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED gfdrrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrrsId :: Lens.Lens' GetFunctionDefinitionResponse (Core.Maybe Core.Text)
gfdrrsId = Lens.field @"id"
{-# DEPRECATED gfdrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrrsLastUpdatedTimestamp :: Lens.Lens' GetFunctionDefinitionResponse (Core.Maybe Core.Text)
gfdrrsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# DEPRECATED gfdrrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrrsLatestVersion :: Lens.Lens' GetFunctionDefinitionResponse (Core.Maybe Core.Text)
gfdrrsLatestVersion = Lens.field @"latestVersion"
{-# DEPRECATED gfdrrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrrsLatestVersionArn :: Lens.Lens' GetFunctionDefinitionResponse (Core.Maybe Core.Text)
gfdrrsLatestVersionArn = Lens.field @"latestVersionArn"
{-# DEPRECATED gfdrrsLatestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrrsName :: Lens.Lens' GetFunctionDefinitionResponse (Core.Maybe Core.Text)
gfdrrsName = Lens.field @"name"
{-# DEPRECATED gfdrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrrsTags :: Lens.Lens' GetFunctionDefinitionResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
gfdrrsTags = Lens.field @"tags"
{-# DEPRECATED gfdrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrrsResponseStatus :: Lens.Lens' GetFunctionDefinitionResponse Core.Int
gfdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gfdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
