{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.GetSchemaCreationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current status of a schema creation operation.
module Network.AWS.AppSync.GetSchemaCreationStatus
  ( -- * Creating a request
    GetSchemaCreationStatus (..),
    mkGetSchemaCreationStatus,

    -- ** Request lenses
    gscsApiId,

    -- * Destructuring the response
    GetSchemaCreationStatusResponse (..),
    mkGetSchemaCreationStatusResponse,

    -- ** Response lenses
    gscsrrsDetails,
    gscsrrsStatus,
    gscsrrsResponseStatus,
  )
where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSchemaCreationStatus' smart constructor.
newtype GetSchemaCreationStatus = GetSchemaCreationStatus'
  { -- | The API ID.
    apiId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSchemaCreationStatus' value with any optional fields omitted.
mkGetSchemaCreationStatus ::
  -- | 'apiId'
  Types.String ->
  GetSchemaCreationStatus
mkGetSchemaCreationStatus apiId = GetSchemaCreationStatus' {apiId}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscsApiId :: Lens.Lens' GetSchemaCreationStatus Types.String
gscsApiId = Lens.field @"apiId"
{-# DEPRECATED gscsApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

instance Core.AWSRequest GetSchemaCreationStatus where
  type Rs GetSchemaCreationStatus = GetSchemaCreationStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apis/" Core.<> (Core.toText apiId)
                Core.<> ("/schemacreation")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSchemaCreationStatusResponse'
            Core.<$> (x Core..:? "details")
            Core.<*> (x Core..:? "status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetSchemaCreationStatusResponse' smart constructor.
data GetSchemaCreationStatusResponse = GetSchemaCreationStatusResponse'
  { -- | Detailed information about the status of the schema creation operation.
    details :: Core.Maybe Types.String,
    -- | The current state of the schema (PROCESSING, FAILED, SUCCESS, or NOT_APPLICABLE). When the schema is in the ACTIVE state, you can add data.
    status :: Core.Maybe Types.SchemaStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSchemaCreationStatusResponse' value with any optional fields omitted.
mkGetSchemaCreationStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSchemaCreationStatusResponse
mkGetSchemaCreationStatusResponse responseStatus =
  GetSchemaCreationStatusResponse'
    { details = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | Detailed information about the status of the schema creation operation.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscsrrsDetails :: Lens.Lens' GetSchemaCreationStatusResponse (Core.Maybe Types.String)
gscsrrsDetails = Lens.field @"details"
{-# DEPRECATED gscsrrsDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The current state of the schema (PROCESSING, FAILED, SUCCESS, or NOT_APPLICABLE). When the schema is in the ACTIVE state, you can add data.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscsrrsStatus :: Lens.Lens' GetSchemaCreationStatusResponse (Core.Maybe Types.SchemaStatus)
gscsrrsStatus = Lens.field @"status"
{-# DEPRECATED gscsrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscsrrsResponseStatus :: Lens.Lens' GetSchemaCreationStatusResponse Core.Int
gscsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gscsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
