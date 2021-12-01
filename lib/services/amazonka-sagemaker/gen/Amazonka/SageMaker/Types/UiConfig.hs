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
-- Module      : Amazonka.SageMaker.Types.UiConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.UiConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provided configuration information for the worker UI for a labeling job.
-- Provide either @HumanTaskUiArn@ or @UiTemplateS3Uri@.
--
-- For named entity recognition, 3D point cloud and video frame labeling
-- jobs, use @HumanTaskUiArn@.
--
-- For all other Ground Truth built-in task types and custom task types,
-- use @UiTemplateS3Uri@ to specify the location of a worker task template
-- in Amazon S3.
--
-- /See:/ 'newUiConfig' smart constructor.
data UiConfig = UiConfig'
  { -- | The Amazon S3 bucket location of the UI template, or worker task
    -- template. This is the template used to render the worker UI and tools
    -- for labeling job tasks. For more information about the contents of a UI
    -- template, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-custom-templates-step2.html Creating Your Custom Labeling Task Template>.
    uiTemplateS3Uri :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the worker task template used to render the worker UI and
    -- tools for labeling job tasks.
    --
    -- Use this parameter when you are creating a labeling job for named entity
    -- recognition, 3D point cloud and video frame labeling jobs. Use your
    -- labeling job task type to select one of the following ARNs and use it
    -- with this parameter when you create a labeling job. Replace @aws-region@
    -- with the Amazon Web Services Region you are creating your labeling job
    -- in. For example, replace @aws-region@ with @us-west-1@ if you create a
    -- labeling job in US West (N. California).
    --
    -- __Named Entity Recognition__
    --
    -- Use the following @HumanTaskUiArn@ for named entity recognition labeling
    -- jobs:
    --
    -- @arn:aws:sagemaker:aws-region:394669845002:human-task-ui\/NamedEntityRecognition@
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
    humanTaskUiArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UiConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uiTemplateS3Uri', 'uiConfig_uiTemplateS3Uri' - The Amazon S3 bucket location of the UI template, or worker task
-- template. This is the template used to render the worker UI and tools
-- for labeling job tasks. For more information about the contents of a UI
-- template, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-custom-templates-step2.html Creating Your Custom Labeling Task Template>.
--
-- 'humanTaskUiArn', 'uiConfig_humanTaskUiArn' - The ARN of the worker task template used to render the worker UI and
-- tools for labeling job tasks.
--
-- Use this parameter when you are creating a labeling job for named entity
-- recognition, 3D point cloud and video frame labeling jobs. Use your
-- labeling job task type to select one of the following ARNs and use it
-- with this parameter when you create a labeling job. Replace @aws-region@
-- with the Amazon Web Services Region you are creating your labeling job
-- in. For example, replace @aws-region@ with @us-west-1@ if you create a
-- labeling job in US West (N. California).
--
-- __Named Entity Recognition__
--
-- Use the following @HumanTaskUiArn@ for named entity recognition labeling
-- jobs:
--
-- @arn:aws:sagemaker:aws-region:394669845002:human-task-ui\/NamedEntityRecognition@
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
newUiConfig ::
  UiConfig
newUiConfig =
  UiConfig'
    { uiTemplateS3Uri = Prelude.Nothing,
      humanTaskUiArn = Prelude.Nothing
    }

-- | The Amazon S3 bucket location of the UI template, or worker task
-- template. This is the template used to render the worker UI and tools
-- for labeling job tasks. For more information about the contents of a UI
-- template, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-custom-templates-step2.html Creating Your Custom Labeling Task Template>.
uiConfig_uiTemplateS3Uri :: Lens.Lens' UiConfig (Prelude.Maybe Prelude.Text)
uiConfig_uiTemplateS3Uri = Lens.lens (\UiConfig' {uiTemplateS3Uri} -> uiTemplateS3Uri) (\s@UiConfig' {} a -> s {uiTemplateS3Uri = a} :: UiConfig)

-- | The ARN of the worker task template used to render the worker UI and
-- tools for labeling job tasks.
--
-- Use this parameter when you are creating a labeling job for named entity
-- recognition, 3D point cloud and video frame labeling jobs. Use your
-- labeling job task type to select one of the following ARNs and use it
-- with this parameter when you create a labeling job. Replace @aws-region@
-- with the Amazon Web Services Region you are creating your labeling job
-- in. For example, replace @aws-region@ with @us-west-1@ if you create a
-- labeling job in US West (N. California).
--
-- __Named Entity Recognition__
--
-- Use the following @HumanTaskUiArn@ for named entity recognition labeling
-- jobs:
--
-- @arn:aws:sagemaker:aws-region:394669845002:human-task-ui\/NamedEntityRecognition@
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

instance Core.FromJSON UiConfig where
  parseJSON =
    Core.withObject
      "UiConfig"
      ( \x ->
          UiConfig'
            Prelude.<$> (x Core..:? "UiTemplateS3Uri")
            Prelude.<*> (x Core..:? "HumanTaskUiArn")
      )

instance Prelude.Hashable UiConfig where
  hashWithSalt salt' UiConfig' {..} =
    salt' `Prelude.hashWithSalt` humanTaskUiArn
      `Prelude.hashWithSalt` uiTemplateS3Uri

instance Prelude.NFData UiConfig where
  rnf UiConfig' {..} =
    Prelude.rnf uiTemplateS3Uri
      `Prelude.seq` Prelude.rnf humanTaskUiArn

instance Core.ToJSON UiConfig where
  toJSON UiConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("UiTemplateS3Uri" Core..=)
              Prelude.<$> uiTemplateS3Uri,
            ("HumanTaskUiArn" Core..=)
              Prelude.<$> humanTaskUiArn
          ]
      )
