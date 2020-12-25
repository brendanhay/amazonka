{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DetectCustomLabels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects custom labels in a supplied image by using an Amazon Rekognition Custom Labels model.
--
-- You specify which version of a model version to use by using the @ProjectVersionArn@ input parameter.
-- You pass the input image as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file.
-- For each object that the model version detects on an image, the API returns a (@CustomLabel@ ) object in an array (@CustomLabels@ ). Each @CustomLabel@ object provides the label name (@Name@ ), the level of confidence that the image contains the object (@Confidence@ ), and object location information, if it exists, for the label on the image (@Geometry@ ).
-- During training model calculates a threshold value that determines if a prediction for a label is true. By default, @DetectCustomLabels@ doesn't return labels whose confidence value is below the model's calculated threshold value. To filter labels that are returned, specify a value for @MinConfidence@ that is higher than the model's calculated threshold. You can get the model's calculated threshold from the model's training results shown in the Amazon Rekognition Custom Labels console. To get all labels, regardless of confidence, specify a @MinConfidence@ value of 0.
-- You can also add the @MaxResults@ parameter to limit the number of labels returned.
-- This is a stateless API operation. That is, the operation does not persist any data.
-- This operation requires permissions to perform the @rekognition:DetectCustomLabels@ action.
module Network.AWS.Rekognition.DetectCustomLabels
  ( -- * Creating a request
    DetectCustomLabels (..),
    mkDetectCustomLabels,

    -- ** Request lenses
    dclProjectVersionArn,
    dclImage,
    dclMaxResults,
    dclMinConfidence,

    -- * Destructuring the response
    DetectCustomLabelsResponse (..),
    mkDetectCustomLabelsResponse,

    -- ** Response lenses
    dclrrsCustomLabels,
    dclrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetectCustomLabels' smart constructor.
data DetectCustomLabels = DetectCustomLabels'
  { -- | The ARN of the model version that you want to use.
    projectVersionArn :: Types.ProjectVersionArn,
    image :: Types.Image,
    -- | Maximum number of results you want the service to return in the response. The service returns the specified number of highest confidence labels ranked from highest confidence to lowest.
    maxResults :: Core.Maybe Core.Natural,
    -- | Specifies the minimum confidence level for the labels to return. Amazon Rekognition doesn't return any labels with a confidence lower than this specified value. If you specify a value of 0, all labels are return, regardless of the default thresholds that the model version applies.
    minConfidence :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectCustomLabels' value with any optional fields omitted.
mkDetectCustomLabels ::
  -- | 'projectVersionArn'
  Types.ProjectVersionArn ->
  -- | 'image'
  Types.Image ->
  DetectCustomLabels
mkDetectCustomLabels projectVersionArn image =
  DetectCustomLabels'
    { projectVersionArn,
      image,
      maxResults = Core.Nothing,
      minConfidence = Core.Nothing
    }

-- | The ARN of the model version that you want to use.
--
-- /Note:/ Consider using 'projectVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclProjectVersionArn :: Lens.Lens' DetectCustomLabels Types.ProjectVersionArn
dclProjectVersionArn = Lens.field @"projectVersionArn"
{-# DEPRECATED dclProjectVersionArn "Use generic-lens or generic-optics with 'projectVersionArn' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclImage :: Lens.Lens' DetectCustomLabels Types.Image
dclImage = Lens.field @"image"
{-# DEPRECATED dclImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | Maximum number of results you want the service to return in the response. The service returns the specified number of highest confidence labels ranked from highest confidence to lowest.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclMaxResults :: Lens.Lens' DetectCustomLabels (Core.Maybe Core.Natural)
dclMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dclMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Specifies the minimum confidence level for the labels to return. Amazon Rekognition doesn't return any labels with a confidence lower than this specified value. If you specify a value of 0, all labels are return, regardless of the default thresholds that the model version applies.
--
-- /Note:/ Consider using 'minConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclMinConfidence :: Lens.Lens' DetectCustomLabels (Core.Maybe Core.Double)
dclMinConfidence = Lens.field @"minConfidence"
{-# DEPRECATED dclMinConfidence "Use generic-lens or generic-optics with 'minConfidence' instead." #-}

instance Core.FromJSON DetectCustomLabels where
  toJSON DetectCustomLabels {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProjectVersionArn" Core..= projectVersionArn),
            Core.Just ("Image" Core..= image),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("MinConfidence" Core..=) Core.<$> minConfidence
          ]
      )

instance Core.AWSRequest DetectCustomLabels where
  type Rs DetectCustomLabels = DetectCustomLabelsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "RekognitionService.DetectCustomLabels")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectCustomLabelsResponse'
            Core.<$> (x Core..:? "CustomLabels") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDetectCustomLabelsResponse' smart constructor.
data DetectCustomLabelsResponse = DetectCustomLabelsResponse'
  { -- | An array of custom labels detected in the input image.
    customLabels :: Core.Maybe [Types.CustomLabel],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectCustomLabelsResponse' value with any optional fields omitted.
mkDetectCustomLabelsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DetectCustomLabelsResponse
mkDetectCustomLabelsResponse responseStatus =
  DetectCustomLabelsResponse'
    { customLabels = Core.Nothing,
      responseStatus
    }

-- | An array of custom labels detected in the input image.
--
-- /Note:/ Consider using 'customLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclrrsCustomLabels :: Lens.Lens' DetectCustomLabelsResponse (Core.Maybe [Types.CustomLabel])
dclrrsCustomLabels = Lens.field @"customLabels"
{-# DEPRECATED dclrrsCustomLabels "Use generic-lens or generic-optics with 'customLabels' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclrrsResponseStatus :: Lens.Lens' DetectCustomLabelsResponse Core.Int
dclrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dclrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
