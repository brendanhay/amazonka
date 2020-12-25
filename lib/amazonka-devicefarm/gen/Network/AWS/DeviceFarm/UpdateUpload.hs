{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.UpdateUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an uploaded test spec.
module Network.AWS.DeviceFarm.UpdateUpload
  ( -- * Creating a request
    UpdateUpload (..),
    mkUpdateUpload,

    -- ** Request lenses
    uuArn,
    uuContentType,
    uuEditContent,
    uuName,

    -- * Destructuring the response
    UpdateUploadResponse (..),
    mkUpdateUploadResponse,

    -- ** Response lenses
    uurrsUpload,
    uurrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateUpload' smart constructor.
data UpdateUpload = UpdateUpload'
  { -- | The Amazon Resource Name (ARN) of the uploaded test spec.
    arn :: Types.AmazonResourceName,
    -- | The upload's content type (for example, @application/x-yaml@ ).
    contentType :: Core.Maybe Types.ContentType,
    -- | Set to true if the YAML file has changed and must be updated. Otherwise, set to false.
    editContent :: Core.Maybe Core.Bool,
    -- | The upload's test spec file name. The name must not contain any forward slashes (/). The test spec file name must end with the @.yaml@ or @.yml@ file extension.
    name :: Core.Maybe Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUpload' value with any optional fields omitted.
mkUpdateUpload ::
  -- | 'arn'
  Types.AmazonResourceName ->
  UpdateUpload
mkUpdateUpload arn =
  UpdateUpload'
    { arn,
      contentType = Core.Nothing,
      editContent = Core.Nothing,
      name = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the uploaded test spec.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuArn :: Lens.Lens' UpdateUpload Types.AmazonResourceName
uuArn = Lens.field @"arn"
{-# DEPRECATED uuArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The upload's content type (for example, @application/x-yaml@ ).
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuContentType :: Lens.Lens' UpdateUpload (Core.Maybe Types.ContentType)
uuContentType = Lens.field @"contentType"
{-# DEPRECATED uuContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | Set to true if the YAML file has changed and must be updated. Otherwise, set to false.
--
-- /Note:/ Consider using 'editContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuEditContent :: Lens.Lens' UpdateUpload (Core.Maybe Core.Bool)
uuEditContent = Lens.field @"editContent"
{-# DEPRECATED uuEditContent "Use generic-lens or generic-optics with 'editContent' instead." #-}

-- | The upload's test spec file name. The name must not contain any forward slashes (/). The test spec file name must end with the @.yaml@ or @.yml@ file extension.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuName :: Lens.Lens' UpdateUpload (Core.Maybe Types.Name)
uuName = Lens.field @"name"
{-# DEPRECATED uuName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON UpdateUpload where
  toJSON UpdateUpload {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("arn" Core..= arn),
            ("contentType" Core..=) Core.<$> contentType,
            ("editContent" Core..=) Core.<$> editContent,
            ("name" Core..=) Core.<$> name
          ]
      )

instance Core.AWSRequest UpdateUpload where
  type Rs UpdateUpload = UpdateUploadResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DeviceFarm_20150623.UpdateUpload")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateUploadResponse'
            Core.<$> (x Core..:? "upload") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateUploadResponse' smart constructor.
data UpdateUploadResponse = UpdateUploadResponse'
  { -- | A test spec uploaded to Device Farm.
    upload :: Core.Maybe Types.Upload,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateUploadResponse' value with any optional fields omitted.
mkUpdateUploadResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateUploadResponse
mkUpdateUploadResponse responseStatus =
  UpdateUploadResponse' {upload = Core.Nothing, responseStatus}

-- | A test spec uploaded to Device Farm.
--
-- /Note:/ Consider using 'upload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uurrsUpload :: Lens.Lens' UpdateUploadResponse (Core.Maybe Types.Upload)
uurrsUpload = Lens.field @"upload"
{-# DEPRECATED uurrsUpload "Use generic-lens or generic-optics with 'upload' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uurrsResponseStatus :: Lens.Lens' UpdateUploadResponse Core.Int
uurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
