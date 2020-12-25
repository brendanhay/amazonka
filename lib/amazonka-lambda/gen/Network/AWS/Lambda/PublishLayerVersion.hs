{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.PublishLayerVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> from a ZIP archive. Each time you call @PublishLayerVersion@ with the same layer name, a new version is created.
--
-- Add layers to your function with 'CreateFunction' or 'UpdateFunctionConfiguration' .
module Network.AWS.Lambda.PublishLayerVersion
  ( -- * Creating a request
    PublishLayerVersion (..),
    mkPublishLayerVersion,

    -- ** Request lenses
    plvLayerName,
    plvContent,
    plvCompatibleRuntimes,
    plvDescription,
    plvLicenseInfo,

    -- * Destructuring the response
    PublishLayerVersionResponse (..),
    mkPublishLayerVersionResponse,

    -- ** Response lenses
    plvrrsCompatibleRuntimes,
    plvrrsContent,
    plvrrsCreatedDate,
    plvrrsDescription,
    plvrrsLayerArn,
    plvrrsLayerVersionArn,
    plvrrsLicenseInfo,
    plvrrsVersion,
    plvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPublishLayerVersion' smart constructor.
data PublishLayerVersion = PublishLayerVersion'
  { -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Types.LayerName,
    -- | The function layer archive.
    content :: Types.LayerVersionContentInput,
    -- | A list of compatible <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html function runtimes> . Used for filtering with 'ListLayers' and 'ListLayerVersions' .
    compatibleRuntimes :: Core.Maybe [Types.Runtime],
    -- | The description of the version.
    description :: Core.Maybe Types.Description,
    -- | The layer's software license. It can be any of the following:
    --
    --
    --     * An <https://spdx.org/licenses/ SPDX license identifier> . For example, @MIT@ .
    --
    --
    --     * The URL of a license hosted on the internet. For example, @https://opensource.org/licenses/MIT@ .
    --
    --
    --     * The full text of the license.
    licenseInfo :: Core.Maybe Types.LicenseInfo
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PublishLayerVersion' value with any optional fields omitted.
mkPublishLayerVersion ::
  -- | 'layerName'
  Types.LayerName ->
  -- | 'content'
  Types.LayerVersionContentInput ->
  PublishLayerVersion
mkPublishLayerVersion layerName content =
  PublishLayerVersion'
    { layerName,
      content,
      compatibleRuntimes = Core.Nothing,
      description = Core.Nothing,
      licenseInfo = Core.Nothing
    }

-- | The name or Amazon Resource Name (ARN) of the layer.
--
-- /Note:/ Consider using 'layerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvLayerName :: Lens.Lens' PublishLayerVersion Types.LayerName
plvLayerName = Lens.field @"layerName"
{-# DEPRECATED plvLayerName "Use generic-lens or generic-optics with 'layerName' instead." #-}

-- | The function layer archive.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvContent :: Lens.Lens' PublishLayerVersion Types.LayerVersionContentInput
plvContent = Lens.field @"content"
{-# DEPRECATED plvContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | A list of compatible <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html function runtimes> . Used for filtering with 'ListLayers' and 'ListLayerVersions' .
--
-- /Note:/ Consider using 'compatibleRuntimes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvCompatibleRuntimes :: Lens.Lens' PublishLayerVersion (Core.Maybe [Types.Runtime])
plvCompatibleRuntimes = Lens.field @"compatibleRuntimes"
{-# DEPRECATED plvCompatibleRuntimes "Use generic-lens or generic-optics with 'compatibleRuntimes' instead." #-}

-- | The description of the version.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvDescription :: Lens.Lens' PublishLayerVersion (Core.Maybe Types.Description)
plvDescription = Lens.field @"description"
{-# DEPRECATED plvDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The layer's software license. It can be any of the following:
--
--
--     * An <https://spdx.org/licenses/ SPDX license identifier> . For example, @MIT@ .
--
--
--     * The URL of a license hosted on the internet. For example, @https://opensource.org/licenses/MIT@ .
--
--
--     * The full text of the license.
--
--
--
-- /Note:/ Consider using 'licenseInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvLicenseInfo :: Lens.Lens' PublishLayerVersion (Core.Maybe Types.LicenseInfo)
plvLicenseInfo = Lens.field @"licenseInfo"
{-# DEPRECATED plvLicenseInfo "Use generic-lens or generic-optics with 'licenseInfo' instead." #-}

instance Core.FromJSON PublishLayerVersion where
  toJSON PublishLayerVersion {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Content" Core..= content),
            ("CompatibleRuntimes" Core..=) Core.<$> compatibleRuntimes,
            ("Description" Core..=) Core.<$> description,
            ("LicenseInfo" Core..=) Core.<$> licenseInfo
          ]
      )

instance Core.AWSRequest PublishLayerVersion where
  type Rs PublishLayerVersion = PublishLayerVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/2018-10-31/layers/" Core.<> (Core.toText layerName)
                Core.<> ("/versions")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PublishLayerVersionResponse'
            Core.<$> (x Core..:? "CompatibleRuntimes")
            Core.<*> (x Core..:? "Content")
            Core.<*> (x Core..:? "CreatedDate")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "LayerArn")
            Core.<*> (x Core..:? "LayerVersionArn")
            Core.<*> (x Core..:? "LicenseInfo")
            Core.<*> (x Core..:? "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPublishLayerVersionResponse' smart constructor.
data PublishLayerVersionResponse = PublishLayerVersionResponse'
  { -- | The layer's compatible runtimes.
    compatibleRuntimes :: Core.Maybe [Types.Runtime],
    -- | Details about the layer version.
    content :: Core.Maybe Types.LayerVersionContentOutput,
    -- | The date that the layer version was created, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
    createdDate :: Core.Maybe Types.CreatedDate,
    -- | The description of the version.
    description :: Core.Maybe Types.Description,
    -- | The ARN of the layer.
    layerArn :: Core.Maybe Types.LayerArn,
    -- | The ARN of the layer version.
    layerVersionArn :: Core.Maybe Types.LayerVersionArn,
    -- | The layer's software license.
    licenseInfo :: Core.Maybe Types.LicenseInfo,
    -- | The version number.
    version :: Core.Maybe Core.Integer,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PublishLayerVersionResponse' value with any optional fields omitted.
mkPublishLayerVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PublishLayerVersionResponse
mkPublishLayerVersionResponse responseStatus =
  PublishLayerVersionResponse'
    { compatibleRuntimes = Core.Nothing,
      content = Core.Nothing,
      createdDate = Core.Nothing,
      description = Core.Nothing,
      layerArn = Core.Nothing,
      layerVersionArn = Core.Nothing,
      licenseInfo = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | The layer's compatible runtimes.
--
-- /Note:/ Consider using 'compatibleRuntimes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvrrsCompatibleRuntimes :: Lens.Lens' PublishLayerVersionResponse (Core.Maybe [Types.Runtime])
plvrrsCompatibleRuntimes = Lens.field @"compatibleRuntimes"
{-# DEPRECATED plvrrsCompatibleRuntimes "Use generic-lens or generic-optics with 'compatibleRuntimes' instead." #-}

-- | Details about the layer version.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvrrsContent :: Lens.Lens' PublishLayerVersionResponse (Core.Maybe Types.LayerVersionContentOutput)
plvrrsContent = Lens.field @"content"
{-# DEPRECATED plvrrsContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The date that the layer version was created, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvrrsCreatedDate :: Lens.Lens' PublishLayerVersionResponse (Core.Maybe Types.CreatedDate)
plvrrsCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED plvrrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The description of the version.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvrrsDescription :: Lens.Lens' PublishLayerVersionResponse (Core.Maybe Types.Description)
plvrrsDescription = Lens.field @"description"
{-# DEPRECATED plvrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ARN of the layer.
--
-- /Note:/ Consider using 'layerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvrrsLayerArn :: Lens.Lens' PublishLayerVersionResponse (Core.Maybe Types.LayerArn)
plvrrsLayerArn = Lens.field @"layerArn"
{-# DEPRECATED plvrrsLayerArn "Use generic-lens or generic-optics with 'layerArn' instead." #-}

-- | The ARN of the layer version.
--
-- /Note:/ Consider using 'layerVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvrrsLayerVersionArn :: Lens.Lens' PublishLayerVersionResponse (Core.Maybe Types.LayerVersionArn)
plvrrsLayerVersionArn = Lens.field @"layerVersionArn"
{-# DEPRECATED plvrrsLayerVersionArn "Use generic-lens or generic-optics with 'layerVersionArn' instead." #-}

-- | The layer's software license.
--
-- /Note:/ Consider using 'licenseInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvrrsLicenseInfo :: Lens.Lens' PublishLayerVersionResponse (Core.Maybe Types.LicenseInfo)
plvrrsLicenseInfo = Lens.field @"licenseInfo"
{-# DEPRECATED plvrrsLicenseInfo "Use generic-lens or generic-optics with 'licenseInfo' instead." #-}

-- | The version number.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvrrsVersion :: Lens.Lens' PublishLayerVersionResponse (Core.Maybe Core.Integer)
plvrrsVersion = Lens.field @"version"
{-# DEPRECATED plvrrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plvrrsResponseStatus :: Lens.Lens' PublishLayerVersionResponse Core.Int
plvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED plvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
