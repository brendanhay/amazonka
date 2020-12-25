{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.PublishSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Publishes a development schema with a major version and a recommended minor version.
module Network.AWS.CloudDirectory.PublishSchema
  ( -- * Creating a request
    PublishSchema (..),
    mkPublishSchema,

    -- ** Request lenses
    psDevelopmentSchemaArn,
    psVersion,
    psMinorVersion,
    psName,

    -- * Destructuring the response
    PublishSchemaResponse (..),
    mkPublishSchemaResponse,

    -- ** Response lenses
    psrrsPublishedSchemaArn,
    psrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPublishSchema' smart constructor.
data PublishSchema = PublishSchema'
  { -- | The Amazon Resource Name (ARN) that is associated with the development schema. For more information, see 'arns' .
    developmentSchemaArn :: Types.Arn,
    -- | The major version under which the schema will be published. Schemas have both a major and minor version associated with them.
    version :: Types.Version,
    -- | The minor version under which the schema will be published. This parameter is recommended. Schemas have both a major and minor version associated with them.
    minorVersion :: Core.Maybe Types.MinorVersion,
    -- | The new name under which the schema will be published. If this is not provided, the development schema is considered.
    name :: Core.Maybe Types.SchemaName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PublishSchema' value with any optional fields omitted.
mkPublishSchema ::
  -- | 'developmentSchemaArn'
  Types.Arn ->
  -- | 'version'
  Types.Version ->
  PublishSchema
mkPublishSchema developmentSchemaArn version =
  PublishSchema'
    { developmentSchemaArn,
      version,
      minorVersion = Core.Nothing,
      name = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) that is associated with the development schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'developmentSchemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psDevelopmentSchemaArn :: Lens.Lens' PublishSchema Types.Arn
psDevelopmentSchemaArn = Lens.field @"developmentSchemaArn"
{-# DEPRECATED psDevelopmentSchemaArn "Use generic-lens or generic-optics with 'developmentSchemaArn' instead." #-}

-- | The major version under which the schema will be published. Schemas have both a major and minor version associated with them.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psVersion :: Lens.Lens' PublishSchema Types.Version
psVersion = Lens.field @"version"
{-# DEPRECATED psVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The minor version under which the schema will be published. This parameter is recommended. Schemas have both a major and minor version associated with them.
--
-- /Note:/ Consider using 'minorVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psMinorVersion :: Lens.Lens' PublishSchema (Core.Maybe Types.MinorVersion)
psMinorVersion = Lens.field @"minorVersion"
{-# DEPRECATED psMinorVersion "Use generic-lens or generic-optics with 'minorVersion' instead." #-}

-- | The new name under which the schema will be published. If this is not provided, the development schema is considered.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psName :: Lens.Lens' PublishSchema (Core.Maybe Types.SchemaName)
psName = Lens.field @"name"
{-# DEPRECATED psName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON PublishSchema where
  toJSON PublishSchema {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Version" Core..= version),
            ("MinorVersion" Core..=) Core.<$> minorVersion,
            ("Name" Core..=) Core.<$> name
          ]
      )

instance Core.AWSRequest PublishSchema where
  type Rs PublishSchema = PublishSchemaResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/schema/publish",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-data-partition" developmentSchemaArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PublishSchemaResponse'
            Core.<$> (x Core..:? "PublishedSchemaArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPublishSchemaResponse' smart constructor.
data PublishSchemaResponse = PublishSchemaResponse'
  { -- | The ARN that is associated with the published schema. For more information, see 'arns' .
    publishedSchemaArn :: Core.Maybe Types.Arn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PublishSchemaResponse' value with any optional fields omitted.
mkPublishSchemaResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PublishSchemaResponse
mkPublishSchemaResponse responseStatus =
  PublishSchemaResponse'
    { publishedSchemaArn = Core.Nothing,
      responseStatus
    }

-- | The ARN that is associated with the published schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'publishedSchemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrrsPublishedSchemaArn :: Lens.Lens' PublishSchemaResponse (Core.Maybe Types.Arn)
psrrsPublishedSchemaArn = Lens.field @"publishedSchemaArn"
{-# DEPRECATED psrrsPublishedSchemaArn "Use generic-lens or generic-optics with 'publishedSchemaArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrrsResponseStatus :: Lens.Lens' PublishSchemaResponse Core.Int
psrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED psrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
