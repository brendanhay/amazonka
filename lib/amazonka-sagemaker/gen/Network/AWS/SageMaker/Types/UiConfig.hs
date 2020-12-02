{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UiConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UiConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provided configuration information for the worker UI for a labeling job.
--
--
--
-- /See:/ 'uiConfig' smart constructor.
data UiConfig = UiConfig'
  { _ucUiTemplateS3URI :: !(Maybe Text),
    _ucHumanTaskUiARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UiConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucUiTemplateS3URI' - The Amazon S3 bucket location of the UI template, or worker task template. This is the template used to render the worker UI and tools for labeling job tasks. For more information about the contents of a UI template, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-custom-templates-step2.html Creating Your Custom Labeling Task Template> .
--
-- * 'ucHumanTaskUiARN' - The ARN of the worker task template used to render the worker UI and tools for labeling job tasks. Use this parameter when you are creating a labeling job for 3D point cloud and video fram labeling jobs. Use your labeling job task type to select one of the following ARN's and use it with this parameter when you create a labeling job. Replace @aws-region@ with the AWS region you are creating your labeling job in. __3D Point Cloud HumanTaskUiArns__  Use this @HumanTaskUiArn@ for 3D point cloud object detection and 3D point cloud object detection adjustment labeling jobs.      * @arn:aws:sagemaker:aws-region:394669845002:human-task-ui/PointCloudObjectDetection@  Use this @HumanTaskUiArn@ for 3D point cloud object tracking and 3D point cloud object tracking adjustment labeling jobs.      * @arn:aws:sagemaker:aws-region:394669845002:human-task-ui/PointCloudObjectTracking@  Use this @HumanTaskUiArn@ for 3D point cloud semantic segmentation and 3D point cloud semantic segmentation adjustment labeling jobs.     * @arn:aws:sagemaker:aws-region:394669845002:human-task-ui/PointCloudSemanticSegmentation@  __Video Frame HumanTaskUiArns__  Use this @HumanTaskUiArn@ for video frame object detection and video frame object detection adjustment labeling jobs.      * @arn:aws:sagemaker:region:394669845002:human-task-ui/VideoObjectDetection@  Use this @HumanTaskUiArn@ for video frame object tracking and video frame object tracking adjustment labeling jobs.      * @arn:aws:sagemaker:aws-region:394669845002:human-task-ui/VideoObjectTracking@
uiConfig ::
  UiConfig
uiConfig =
  UiConfig'
    { _ucUiTemplateS3URI = Nothing,
      _ucHumanTaskUiARN = Nothing
    }

-- | The Amazon S3 bucket location of the UI template, or worker task template. This is the template used to render the worker UI and tools for labeling job tasks. For more information about the contents of a UI template, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-custom-templates-step2.html Creating Your Custom Labeling Task Template> .
ucUiTemplateS3URI :: Lens' UiConfig (Maybe Text)
ucUiTemplateS3URI = lens _ucUiTemplateS3URI (\s a -> s {_ucUiTemplateS3URI = a})

-- | The ARN of the worker task template used to render the worker UI and tools for labeling job tasks. Use this parameter when you are creating a labeling job for 3D point cloud and video fram labeling jobs. Use your labeling job task type to select one of the following ARN's and use it with this parameter when you create a labeling job. Replace @aws-region@ with the AWS region you are creating your labeling job in. __3D Point Cloud HumanTaskUiArns__  Use this @HumanTaskUiArn@ for 3D point cloud object detection and 3D point cloud object detection adjustment labeling jobs.      * @arn:aws:sagemaker:aws-region:394669845002:human-task-ui/PointCloudObjectDetection@  Use this @HumanTaskUiArn@ for 3D point cloud object tracking and 3D point cloud object tracking adjustment labeling jobs.      * @arn:aws:sagemaker:aws-region:394669845002:human-task-ui/PointCloudObjectTracking@  Use this @HumanTaskUiArn@ for 3D point cloud semantic segmentation and 3D point cloud semantic segmentation adjustment labeling jobs.     * @arn:aws:sagemaker:aws-region:394669845002:human-task-ui/PointCloudSemanticSegmentation@  __Video Frame HumanTaskUiArns__  Use this @HumanTaskUiArn@ for video frame object detection and video frame object detection adjustment labeling jobs.      * @arn:aws:sagemaker:region:394669845002:human-task-ui/VideoObjectDetection@  Use this @HumanTaskUiArn@ for video frame object tracking and video frame object tracking adjustment labeling jobs.      * @arn:aws:sagemaker:aws-region:394669845002:human-task-ui/VideoObjectTracking@
ucHumanTaskUiARN :: Lens' UiConfig (Maybe Text)
ucHumanTaskUiARN = lens _ucHumanTaskUiARN (\s a -> s {_ucHumanTaskUiARN = a})

instance FromJSON UiConfig where
  parseJSON =
    withObject
      "UiConfig"
      ( \x ->
          UiConfig'
            <$> (x .:? "UiTemplateS3Uri") <*> (x .:? "HumanTaskUiArn")
      )

instance Hashable UiConfig

instance NFData UiConfig

instance ToJSON UiConfig where
  toJSON UiConfig' {..} =
    object
      ( catMaybes
          [ ("UiTemplateS3Uri" .=) <$> _ucUiTemplateS3URI,
            ("HumanTaskUiArn" .=) <$> _ucHumanTaskUiARN
          ]
      )
