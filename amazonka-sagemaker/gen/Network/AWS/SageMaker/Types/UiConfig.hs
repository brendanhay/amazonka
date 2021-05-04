{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UiConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UiConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provided configuration information for the worker UI for a labeling job.
--
-- /See:/ 'newUiConfig' smart constructor.
data UiConfig = UiConfig'
  { -- | The ARN of the worker task template used to render the worker UI and
    -- tools for labeling job tasks.
    --
    -- Use this parameter when you are creating a labeling job for 3D point
    -- cloud and video fram labeling jobs. Use your labeling job task type to
    -- select one of the following ARNs and use it with this parameter when you
    -- create a labeling job. Replace @aws-region@ with the AWS region you are
    -- creating your labeling job in.
    --
    -- __3D Point Cloud HumanTaskUiArns__
    --
    -- Use this @HumanTaskUiArn@ for 3D point cloud object detection and 3D
    -- point cloud object detection adjustment labeling jobs.
    --
    -- -   @arn:aws:sagemaker:aws-region:394669845002:human-task-ui\/PointCloudObjectDetection@
    --
    -- Use this @HumanTaskUiArn@ for 3D point cloud object tracking and 3D
    -- point cloud object tracking adjustment labeling jobs.
    --
    -- -   @arn:aws:sagemaker:aws-region:394669845002:human-task-ui\/PointCloudObjectTracking@
    --
    -- Use this @HumanTaskUiArn@ for 3D point cloud semantic segmentation and
    -- 3D point cloud semantic segmentation adjustment labeling jobs.
    --
    -- -   @arn:aws:sagemaker:aws-region:394669845002:human-task-ui\/PointCloudSemanticSegmentation@
    --
    -- __Video Frame HumanTaskUiArns__
    --
    -- Use this @HumanTaskUiArn@ for video frame object detection and video
    -- frame object detection adjustment labeling jobs.
    --
    -- -   @arn:aws:sagemaker:region:394669845002:human-task-ui\/VideoObjectDetection@
    --
    -- Use this @HumanTaskUiArn@ for video frame object tracking and video
    -- frame object tracking adjustment labeling jobs.
    --
    -- -   @arn:aws:sagemaker:aws-region:394669845002:human-task-ui\/VideoObjectTracking@
    humanTaskUiArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket location of the UI template, or worker task
    -- template. This is the template used to render the worker UI and tools
    -- for labeling job tasks. For more information about the contents of a UI
    -- template, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-custom-templates-step2.html Creating Your Custom Labeling Task Template>.
    uiTemplateS3Uri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UiConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'humanTaskUiArn', 'uiConfig_humanTaskUiArn' - The ARN of the worker task template used to render the worker UI and
-- tools for labeling job tasks.
--
-- Use this parameter when you are creating a labeling job for 3D point
-- cloud and video fram labeling jobs. Use your labeling job task type to
-- select one of the following ARNs and use it with this parameter when you
-- create a labeling job. Replace @aws-region@ with the AWS region you are
-- creating your labeling job in.
--
-- __3D Point Cloud HumanTaskUiArns__
--
-- Use this @HumanTaskUiArn@ for 3D point cloud object detection and 3D
-- point cloud object detection adjustment labeling jobs.
--
-- -   @arn:aws:sagemaker:aws-region:394669845002:human-task-ui\/PointCloudObjectDetection@
--
-- Use this @HumanTaskUiArn@ for 3D point cloud object tracking and 3D
-- point cloud object tracking adjustment labeling jobs.
--
-- -   @arn:aws:sagemaker:aws-region:394669845002:human-task-ui\/PointCloudObjectTracking@
--
-- Use this @HumanTaskUiArn@ for 3D point cloud semantic segmentation and
-- 3D point cloud semantic segmentation adjustment labeling jobs.
--
-- -   @arn:aws:sagemaker:aws-region:394669845002:human-task-ui\/PointCloudSemanticSegmentation@
--
-- __Video Frame HumanTaskUiArns__
--
-- Use this @HumanTaskUiArn@ for video frame object detection and video
-- frame object detection adjustment labeling jobs.
--
-- -   @arn:aws:sagemaker:region:394669845002:human-task-ui\/VideoObjectDetection@
--
-- Use this @HumanTaskUiArn@ for video frame object tracking and video
-- frame object tracking adjustment labeling jobs.
--
-- -   @arn:aws:sagemaker:aws-region:394669845002:human-task-ui\/VideoObjectTracking@
--
-- 'uiTemplateS3Uri', 'uiConfig_uiTemplateS3Uri' - The Amazon S3 bucket location of the UI template, or worker task
-- template. This is the template used to render the worker UI and tools
-- for labeling job tasks. For more information about the contents of a UI
-- template, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-custom-templates-step2.html Creating Your Custom Labeling Task Template>.
newUiConfig ::
  UiConfig
newUiConfig =
  UiConfig'
    { humanTaskUiArn = Prelude.Nothing,
      uiTemplateS3Uri = Prelude.Nothing
    }

-- | The ARN of the worker task template used to render the worker UI and
-- tools for labeling job tasks.
--
-- Use this parameter when you are creating a labeling job for 3D point
-- cloud and video fram labeling jobs. Use your labeling job task type to
-- select one of the following ARNs and use it with this parameter when you
-- create a labeling job. Replace @aws-region@ with the AWS region you are
-- creating your labeling job in.
--
-- __3D Point Cloud HumanTaskUiArns__
--
-- Use this @HumanTaskUiArn@ for 3D point cloud object detection and 3D
-- point cloud object detection adjustment labeling jobs.
--
-- -   @arn:aws:sagemaker:aws-region:394669845002:human-task-ui\/PointCloudObjectDetection@
--
-- Use this @HumanTaskUiArn@ for 3D point cloud object tracking and 3D
-- point cloud object tracking adjustment labeling jobs.
--
-- -   @arn:aws:sagemaker:aws-region:394669845002:human-task-ui\/PointCloudObjectTracking@
--
-- Use this @HumanTaskUiArn@ for 3D point cloud semantic segmentation and
-- 3D point cloud semantic segmentation adjustment labeling jobs.
--
-- -   @arn:aws:sagemaker:aws-region:394669845002:human-task-ui\/PointCloudSemanticSegmentation@
--
-- __Video Frame HumanTaskUiArns__
--
-- Use this @HumanTaskUiArn@ for video frame object detection and video
-- frame object detection adjustment labeling jobs.
--
-- -   @arn:aws:sagemaker:region:394669845002:human-task-ui\/VideoObjectDetection@
--
-- Use this @HumanTaskUiArn@ for video frame object tracking and video
-- frame object tracking adjustment labeling jobs.
--
-- -   @arn:aws:sagemaker:aws-region:394669845002:human-task-ui\/VideoObjectTracking@
uiConfig_humanTaskUiArn :: Lens.Lens' UiConfig (Prelude.Maybe Prelude.Text)
uiConfig_humanTaskUiArn = Lens.lens (\UiConfig' {humanTaskUiArn} -> humanTaskUiArn) (\s@UiConfig' {} a -> s {humanTaskUiArn = a} :: UiConfig)

-- | The Amazon S3 bucket location of the UI template, or worker task
-- template. This is the template used to render the worker UI and tools
-- for labeling job tasks. For more information about the contents of a UI
-- template, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-custom-templates-step2.html Creating Your Custom Labeling Task Template>.
uiConfig_uiTemplateS3Uri :: Lens.Lens' UiConfig (Prelude.Maybe Prelude.Text)
uiConfig_uiTemplateS3Uri = Lens.lens (\UiConfig' {uiTemplateS3Uri} -> uiTemplateS3Uri) (\s@UiConfig' {} a -> s {uiTemplateS3Uri = a} :: UiConfig)

instance Prelude.FromJSON UiConfig where
  parseJSON =
    Prelude.withObject
      "UiConfig"
      ( \x ->
          UiConfig'
            Prelude.<$> (x Prelude..:? "HumanTaskUiArn")
            Prelude.<*> (x Prelude..:? "UiTemplateS3Uri")
      )

instance Prelude.Hashable UiConfig

instance Prelude.NFData UiConfig

instance Prelude.ToJSON UiConfig where
  toJSON UiConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("HumanTaskUiArn" Prelude..=)
              Prelude.<$> humanTaskUiArn,
            ("UiTemplateS3Uri" Prelude..=)
              Prelude.<$> uiTemplateS3Uri
          ]
      )
