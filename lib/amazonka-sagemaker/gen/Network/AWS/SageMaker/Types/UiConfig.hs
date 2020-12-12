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
    ucUiTemplateS3URI,
    ucHumanTaskUiARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provided configuration information for the worker UI for a labeling job.
--
-- /See:/ 'mkUiConfig' smart constructor.
data UiConfig = UiConfig'
  { uiTemplateS3URI :: Lude.Maybe Lude.Text,
    humanTaskUiARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UiConfig' with the minimum fields required to make a request.
--
-- * 'humanTaskUiARN' - The ARN of the worker task template used to render the worker UI and tools for labeling job tasks.
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
-- * 'uiTemplateS3URI' - The Amazon S3 bucket location of the UI template, or worker task template. This is the template used to render the worker UI and tools for labeling job tasks. For more information about the contents of a UI template, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-custom-templates-step2.html Creating Your Custom Labeling Task Template> .
mkUiConfig ::
  UiConfig
mkUiConfig =
  UiConfig'
    { uiTemplateS3URI = Lude.Nothing,
      humanTaskUiARN = Lude.Nothing
    }

-- | The Amazon S3 bucket location of the UI template, or worker task template. This is the template used to render the worker UI and tools for labeling job tasks. For more information about the contents of a UI template, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-custom-templates-step2.html Creating Your Custom Labeling Task Template> .
--
-- /Note:/ Consider using 'uiTemplateS3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucUiTemplateS3URI :: Lens.Lens' UiConfig (Lude.Maybe Lude.Text)
ucUiTemplateS3URI = Lens.lens (uiTemplateS3URI :: UiConfig -> Lude.Maybe Lude.Text) (\s a -> s {uiTemplateS3URI = a} :: UiConfig)
{-# DEPRECATED ucUiTemplateS3URI "Use generic-lens or generic-optics with 'uiTemplateS3URI' instead." #-}

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
-- /Note:/ Consider using 'humanTaskUiARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucHumanTaskUiARN :: Lens.Lens' UiConfig (Lude.Maybe Lude.Text)
ucHumanTaskUiARN = Lens.lens (humanTaskUiARN :: UiConfig -> Lude.Maybe Lude.Text) (\s a -> s {humanTaskUiARN = a} :: UiConfig)
{-# DEPRECATED ucHumanTaskUiARN "Use generic-lens or generic-optics with 'humanTaskUiARN' instead." #-}

instance Lude.FromJSON UiConfig where
  parseJSON =
    Lude.withObject
      "UiConfig"
      ( \x ->
          UiConfig'
            Lude.<$> (x Lude..:? "UiTemplateS3Uri")
            Lude.<*> (x Lude..:? "HumanTaskUiArn")
      )

instance Lude.ToJSON UiConfig where
  toJSON UiConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("UiTemplateS3Uri" Lude..=) Lude.<$> uiTemplateS3URI,
            ("HumanTaskUiArn" Lude..=) Lude.<$> humanTaskUiARN
          ]
      )
