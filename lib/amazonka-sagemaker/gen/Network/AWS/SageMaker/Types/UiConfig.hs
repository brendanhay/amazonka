{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UiConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UiConfig
  ( UiConfig (..),

    -- * Smart constructor
    mkUiConfig,

    -- * Lenses
    ucHumanTaskUiArn,
    ucUiTemplateS3Uri,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.HumanTaskUiArn as Types
import qualified Network.AWS.SageMaker.Types.UiTemplateS3Uri as Types

-- | Provided configuration information for the worker UI for a labeling job.
--
-- /See:/ 'mkUiConfig' smart constructor.
data UiConfig = UiConfig'
  { -- | The ARN of the worker task template used to render the worker UI and tools for labeling job tasks.
    --
    -- Use this parameter when you are creating a labeling job for 3D point cloud and video fram labeling jobs. Use your labeling job task type to select one of the following ARN's and use it with this parameter when you create a labeling job. Replace @aws-region@ with the AWS region you are creating your labeling job in.
    -- __3D Point Cloud HumanTaskUiArns__
    -- Use this @HumanTaskUiArn@ for 3D point cloud object detection and 3D point cloud object detection adjustment labeling jobs.
    --
    --     * @arn:aws:sagemaker:aws-region:394669845002:human-task-ui/PointCloudObjectDetection@
    --
    --
    -- Use this @HumanTaskUiArn@ for 3D point cloud object tracking and 3D point cloud object tracking adjustment labeling jobs.
    --
    --     * @arn:aws:sagemaker:aws-region:394669845002:human-task-ui/PointCloudObjectTracking@
    --
    --
    -- Use this @HumanTaskUiArn@ for 3D point cloud semantic segmentation and 3D point cloud semantic segmentation adjustment labeling jobs.
    --
    --     * @arn:aws:sagemaker:aws-region:394669845002:human-task-ui/PointCloudSemanticSegmentation@
    --
    --
    -- __Video Frame HumanTaskUiArns__
    -- Use this @HumanTaskUiArn@ for video frame object detection and video frame object detection adjustment labeling jobs.
    --
    --     * @arn:aws:sagemaker:region:394669845002:human-task-ui/VideoObjectDetection@
    --
    --
    -- Use this @HumanTaskUiArn@ for video frame object tracking and video frame object tracking adjustment labeling jobs.
    --
    --     * @arn:aws:sagemaker:aws-region:394669845002:human-task-ui/VideoObjectTracking@
    humanTaskUiArn :: Core.Maybe Types.HumanTaskUiArn,
    -- | The Amazon S3 bucket location of the UI template, or worker task template. This is the template used to render the worker UI and tools for labeling job tasks. For more information about the contents of a UI template, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-custom-templates-step2.html Creating Your Custom Labeling Task Template> .
    uiTemplateS3Uri :: Core.Maybe Types.UiTemplateS3Uri
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UiConfig' value with any optional fields omitted.
mkUiConfig ::
  UiConfig
mkUiConfig =
  UiConfig'
    { humanTaskUiArn = Core.Nothing,
      uiTemplateS3Uri = Core.Nothing
    }

-- | The ARN of the worker task template used to render the worker UI and tools for labeling job tasks.
--
-- Use this parameter when you are creating a labeling job for 3D point cloud and video fram labeling jobs. Use your labeling job task type to select one of the following ARN's and use it with this parameter when you create a labeling job. Replace @aws-region@ with the AWS region you are creating your labeling job in.
-- __3D Point Cloud HumanTaskUiArns__
-- Use this @HumanTaskUiArn@ for 3D point cloud object detection and 3D point cloud object detection adjustment labeling jobs.
--
--     * @arn:aws:sagemaker:aws-region:394669845002:human-task-ui/PointCloudObjectDetection@
--
--
-- Use this @HumanTaskUiArn@ for 3D point cloud object tracking and 3D point cloud object tracking adjustment labeling jobs.
--
--     * @arn:aws:sagemaker:aws-region:394669845002:human-task-ui/PointCloudObjectTracking@
--
--
-- Use this @HumanTaskUiArn@ for 3D point cloud semantic segmentation and 3D point cloud semantic segmentation adjustment labeling jobs.
--
--     * @arn:aws:sagemaker:aws-region:394669845002:human-task-ui/PointCloudSemanticSegmentation@
--
--
-- __Video Frame HumanTaskUiArns__
-- Use this @HumanTaskUiArn@ for video frame object detection and video frame object detection adjustment labeling jobs.
--
--     * @arn:aws:sagemaker:region:394669845002:human-task-ui/VideoObjectDetection@
--
--
-- Use this @HumanTaskUiArn@ for video frame object tracking and video frame object tracking adjustment labeling jobs.
--
--     * @arn:aws:sagemaker:aws-region:394669845002:human-task-ui/VideoObjectTracking@
--
--
--
-- /Note:/ Consider using 'humanTaskUiArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucHumanTaskUiArn :: Lens.Lens' UiConfig (Core.Maybe Types.HumanTaskUiArn)
ucHumanTaskUiArn = Lens.field @"humanTaskUiArn"
{-# DEPRECATED ucHumanTaskUiArn "Use generic-lens or generic-optics with 'humanTaskUiArn' instead." #-}

-- | The Amazon S3 bucket location of the UI template, or worker task template. This is the template used to render the worker UI and tools for labeling job tasks. For more information about the contents of a UI template, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-custom-templates-step2.html Creating Your Custom Labeling Task Template> .
--
-- /Note:/ Consider using 'uiTemplateS3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucUiTemplateS3Uri :: Lens.Lens' UiConfig (Core.Maybe Types.UiTemplateS3Uri)
ucUiTemplateS3Uri = Lens.field @"uiTemplateS3Uri"
{-# DEPRECATED ucUiTemplateS3Uri "Use generic-lens or generic-optics with 'uiTemplateS3Uri' instead." #-}

instance Core.FromJSON UiConfig where
  toJSON UiConfig {..} =
    Core.object
      ( Core.catMaybes
          [ ("HumanTaskUiArn" Core..=) Core.<$> humanTaskUiArn,
            ("UiTemplateS3Uri" Core..=) Core.<$> uiTemplateS3Uri
          ]
      )

instance Core.FromJSON UiConfig where
  parseJSON =
    Core.withObject "UiConfig" Core.$
      \x ->
        UiConfig'
          Core.<$> (x Core..:? "HumanTaskUiArn")
          Core.<*> (x Core..:? "UiTemplateS3Uri")
