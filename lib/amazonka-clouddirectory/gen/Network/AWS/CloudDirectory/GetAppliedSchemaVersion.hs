{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.GetAppliedSchemaVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns current applied schema version ARN, including the minor version in use.
module Network.AWS.CloudDirectory.GetAppliedSchemaVersion
  ( -- * Creating a request
    GetAppliedSchemaVersion (..),
    mkGetAppliedSchemaVersion,

    -- ** Request lenses
    gasvSchemaArn,

    -- * Destructuring the response
    GetAppliedSchemaVersionResponse (..),
    mkGetAppliedSchemaVersionResponse,

    -- ** Response lenses
    gasvrrsAppliedSchemaArn,
    gasvrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAppliedSchemaVersion' smart constructor.
newtype GetAppliedSchemaVersion = GetAppliedSchemaVersion'
  { -- | The ARN of the applied schema.
    schemaArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetAppliedSchemaVersion' value with any optional fields omitted.
mkGetAppliedSchemaVersion ::
  -- | 'schemaArn'
  Types.Arn ->
  GetAppliedSchemaVersion
mkGetAppliedSchemaVersion schemaArn =
  GetAppliedSchemaVersion' {schemaArn}

-- | The ARN of the applied schema.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasvSchemaArn :: Lens.Lens' GetAppliedSchemaVersion Types.Arn
gasvSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED gasvSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

instance Core.FromJSON GetAppliedSchemaVersion where
  toJSON GetAppliedSchemaVersion {..} =
    Core.object
      (Core.catMaybes [Core.Just ("SchemaArn" Core..= schemaArn)])

instance Core.AWSRequest GetAppliedSchemaVersion where
  type Rs GetAppliedSchemaVersion = GetAppliedSchemaVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            "/amazonclouddirectory/2017-01-11/schema/getappliedschema",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppliedSchemaVersionResponse'
            Core.<$> (x Core..:? "AppliedSchemaArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetAppliedSchemaVersionResponse' smart constructor.
data GetAppliedSchemaVersionResponse = GetAppliedSchemaVersionResponse'
  { -- | Current applied schema ARN, including the minor version in use if one was provided.
    appliedSchemaArn :: Core.Maybe Types.Arn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAppliedSchemaVersionResponse' value with any optional fields omitted.
mkGetAppliedSchemaVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetAppliedSchemaVersionResponse
mkGetAppliedSchemaVersionResponse responseStatus =
  GetAppliedSchemaVersionResponse'
    { appliedSchemaArn = Core.Nothing,
      responseStatus
    }

-- | Current applied schema ARN, including the minor version in use if one was provided.
--
-- /Note:/ Consider using 'appliedSchemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasvrrsAppliedSchemaArn :: Lens.Lens' GetAppliedSchemaVersionResponse (Core.Maybe Types.Arn)
gasvrrsAppliedSchemaArn = Lens.field @"appliedSchemaArn"
{-# DEPRECATED gasvrrsAppliedSchemaArn "Use generic-lens or generic-optics with 'appliedSchemaArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasvrrsResponseStatus :: Lens.Lens' GetAppliedSchemaVersionResponse Core.Int
gasvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gasvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
