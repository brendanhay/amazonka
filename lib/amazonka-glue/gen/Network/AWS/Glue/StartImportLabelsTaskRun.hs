{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StartImportLabelsTaskRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables you to provide additional labels (examples of truth) to be used to teach the machine learning transform and improve its quality. This API operation is generally used as part of the active learning workflow that starts with the @StartMLLabelingSetGenerationTaskRun@ call and that ultimately results in improving the quality of your machine learning transform.
--
--
-- After the @StartMLLabelingSetGenerationTaskRun@ finishes, AWS Glue machine learning will have generated a series of questions for humans to answer. (Answering these questions is often called 'labeling' in the machine learning workflows). In the case of the @FindMatches@ transform, these questions are of the form, “What is the correct way to group these rows together into groups composed entirely of matching records?” After the labeling process is finished, users upload their answers/labels with a call to @StartImportLabelsTaskRun@ . After @StartImportLabelsTaskRun@ finishes, all future runs of the machine learning transform use the new and improved labels and perform a higher-quality transformation.
--
-- By default, @StartMLLabelingSetGenerationTaskRun@ continually learns from and combines all labels that you upload unless you set @Replace@ to true. If you set @Replace@ to true, @StartImportLabelsTaskRun@ deletes and forgets all previously uploaded labels and learns only from the exact set that you upload. Replacing labels can be helpful if you realize that you previously uploaded incorrect labels, and you believe that they are having a negative effect on your transform quality.
--
-- You can check on the status of your task run by calling the @GetMLTaskRun@ operation.
module Network.AWS.Glue.StartImportLabelsTaskRun
  ( -- * Creating a Request
    startImportLabelsTaskRun,
    StartImportLabelsTaskRun,

    -- * Request Lenses
    siltrReplaceAllLabels,
    siltrTransformId,
    siltrInputS3Path,

    -- * Destructuring the Response
    startImportLabelsTaskRunResponse,
    StartImportLabelsTaskRunResponse,

    -- * Response Lenses
    siltrrsTaskRunId,
    siltrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startImportLabelsTaskRun' smart constructor.
data StartImportLabelsTaskRun = StartImportLabelsTaskRun'
  { _siltrReplaceAllLabels ::
      !(Maybe Bool),
    _siltrTransformId :: !Text,
    _siltrInputS3Path :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartImportLabelsTaskRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siltrReplaceAllLabels' - Indicates whether to overwrite your existing labels.
--
-- * 'siltrTransformId' - The unique identifier of the machine learning transform.
--
-- * 'siltrInputS3Path' - The Amazon Simple Storage Service (Amazon S3) path from where you import the labels.
startImportLabelsTaskRun ::
  -- | 'siltrTransformId'
  Text ->
  -- | 'siltrInputS3Path'
  Text ->
  StartImportLabelsTaskRun
startImportLabelsTaskRun pTransformId_ pInputS3Path_ =
  StartImportLabelsTaskRun'
    { _siltrReplaceAllLabels = Nothing,
      _siltrTransformId = pTransformId_,
      _siltrInputS3Path = pInputS3Path_
    }

-- | Indicates whether to overwrite your existing labels.
siltrReplaceAllLabels :: Lens' StartImportLabelsTaskRun (Maybe Bool)
siltrReplaceAllLabels = lens _siltrReplaceAllLabels (\s a -> s {_siltrReplaceAllLabels = a})

-- | The unique identifier of the machine learning transform.
siltrTransformId :: Lens' StartImportLabelsTaskRun Text
siltrTransformId = lens _siltrTransformId (\s a -> s {_siltrTransformId = a})

-- | The Amazon Simple Storage Service (Amazon S3) path from where you import the labels.
siltrInputS3Path :: Lens' StartImportLabelsTaskRun Text
siltrInputS3Path = lens _siltrInputS3Path (\s a -> s {_siltrInputS3Path = a})

instance AWSRequest StartImportLabelsTaskRun where
  type Rs StartImportLabelsTaskRun = StartImportLabelsTaskRunResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          StartImportLabelsTaskRunResponse'
            <$> (x .?> "TaskRunId") <*> (pure (fromEnum s))
      )

instance Hashable StartImportLabelsTaskRun

instance NFData StartImportLabelsTaskRun

instance ToHeaders StartImportLabelsTaskRun where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSGlue.StartImportLabelsTaskRun" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartImportLabelsTaskRun where
  toJSON StartImportLabelsTaskRun' {..} =
    object
      ( catMaybes
          [ ("ReplaceAllLabels" .=) <$> _siltrReplaceAllLabels,
            Just ("TransformId" .= _siltrTransformId),
            Just ("InputS3Path" .= _siltrInputS3Path)
          ]
      )

instance ToPath StartImportLabelsTaskRun where
  toPath = const "/"

instance ToQuery StartImportLabelsTaskRun where
  toQuery = const mempty

-- | /See:/ 'startImportLabelsTaskRunResponse' smart constructor.
data StartImportLabelsTaskRunResponse = StartImportLabelsTaskRunResponse'
  { _siltrrsTaskRunId ::
      !(Maybe Text),
    _siltrrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartImportLabelsTaskRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siltrrsTaskRunId' - The unique identifier for the task run.
--
-- * 'siltrrsResponseStatus' - -- | The response status code.
startImportLabelsTaskRunResponse ::
  -- | 'siltrrsResponseStatus'
  Int ->
  StartImportLabelsTaskRunResponse
startImportLabelsTaskRunResponse pResponseStatus_ =
  StartImportLabelsTaskRunResponse'
    { _siltrrsTaskRunId = Nothing,
      _siltrrsResponseStatus = pResponseStatus_
    }

-- | The unique identifier for the task run.
siltrrsTaskRunId :: Lens' StartImportLabelsTaskRunResponse (Maybe Text)
siltrrsTaskRunId = lens _siltrrsTaskRunId (\s a -> s {_siltrrsTaskRunId = a})

-- | -- | The response status code.
siltrrsResponseStatus :: Lens' StartImportLabelsTaskRunResponse Int
siltrrsResponseStatus = lens _siltrrsResponseStatus (\s a -> s {_siltrrsResponseStatus = a})

instance NFData StartImportLabelsTaskRunResponse
