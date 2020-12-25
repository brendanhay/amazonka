{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.CreateImportJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an import job for an application.
module Network.AWS.Pinpoint.CreateImportJob
  ( -- * Creating a request
    CreateImportJob (..),
    mkCreateImportJob,

    -- ** Request lenses
    cijApplicationId,
    cijImportJobRequest,

    -- * Destructuring the response
    CreateImportJobResponse (..),
    mkCreateImportJobResponse,

    -- ** Response lenses
    cijrrsImportJobResponse,
    cijrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateImportJob' smart constructor.
data CreateImportJob = CreateImportJob'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    importJobRequest :: Types.ImportJobRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateImportJob' value with any optional fields omitted.
mkCreateImportJob ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'importJobRequest'
  Types.ImportJobRequest ->
  CreateImportJob
mkCreateImportJob applicationId importJobRequest =
  CreateImportJob' {applicationId, importJobRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cijApplicationId :: Lens.Lens' CreateImportJob Core.Text
cijApplicationId = Lens.field @"applicationId"
{-# DEPRECATED cijApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'importJobRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cijImportJobRequest :: Lens.Lens' CreateImportJob Types.ImportJobRequest
cijImportJobRequest = Lens.field @"importJobRequest"
{-# DEPRECATED cijImportJobRequest "Use generic-lens or generic-optics with 'importJobRequest' instead." #-}

instance Core.FromJSON CreateImportJob where
  toJSON CreateImportJob {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ImportJobRequest" Core..= importJobRequest)]
      )

instance Core.AWSRequest CreateImportJob where
  type Rs CreateImportJob = CreateImportJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/jobs/import")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateImportJobResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateImportJobResponse' smart constructor.
data CreateImportJobResponse = CreateImportJobResponse'
  { importJobResponse :: Types.ImportJobResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateImportJobResponse' value with any optional fields omitted.
mkCreateImportJobResponse ::
  -- | 'importJobResponse'
  Types.ImportJobResponse ->
  -- | 'responseStatus'
  Core.Int ->
  CreateImportJobResponse
mkCreateImportJobResponse importJobResponse responseStatus =
  CreateImportJobResponse' {importJobResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'importJobResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cijrrsImportJobResponse :: Lens.Lens' CreateImportJobResponse Types.ImportJobResponse
cijrrsImportJobResponse = Lens.field @"importJobResponse"
{-# DEPRECATED cijrrsImportJobResponse "Use generic-lens or generic-optics with 'importJobResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cijrrsResponseStatus :: Lens.Lens' CreateImportJobResponse Core.Int
cijrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cijrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
