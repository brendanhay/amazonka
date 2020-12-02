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
-- Module      : Network.AWS.Glue.StartMLLabelingSetGenerationTaskRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the active learning workflow for your machine learning transform to improve the transform's quality by generating label sets and adding labels.
--
--
-- When the @StartMLLabelingSetGenerationTaskRun@ finishes, AWS Glue will have generated a "labeling set" or a set of questions for humans to answer.
--
-- In the case of the @FindMatches@ transform, these questions are of the form, “What is the correct way to group these rows together into groups composed entirely of matching records?”
--
-- After the labeling process is finished, you can upload your labels with a call to @StartImportLabelsTaskRun@ . After @StartImportLabelsTaskRun@ finishes, all future runs of the machine learning transform will use the new and improved labels and perform a higher-quality transformation.
module Network.AWS.Glue.StartMLLabelingSetGenerationTaskRun
  ( -- * Creating a Request
    startMLLabelingSetGenerationTaskRun,
    StartMLLabelingSetGenerationTaskRun,

    -- * Request Lenses
    smllsgtrTransformId,
    smllsgtrOutputS3Path,

    -- * Destructuring the Response
    startMLLabelingSetGenerationTaskRunResponse,
    StartMLLabelingSetGenerationTaskRunResponse,

    -- * Response Lenses
    smllsgtrrsTaskRunId,
    smllsgtrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startMLLabelingSetGenerationTaskRun' smart constructor.
data StartMLLabelingSetGenerationTaskRun = StartMLLabelingSetGenerationTaskRun'
  { _smllsgtrTransformId ::
      !Text,
    _smllsgtrOutputS3Path ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartMLLabelingSetGenerationTaskRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smllsgtrTransformId' - The unique identifier of the machine learning transform.
--
-- * 'smllsgtrOutputS3Path' - The Amazon Simple Storage Service (Amazon S3) path where you generate the labeling set.
startMLLabelingSetGenerationTaskRun ::
  -- | 'smllsgtrTransformId'
  Text ->
  -- | 'smllsgtrOutputS3Path'
  Text ->
  StartMLLabelingSetGenerationTaskRun
startMLLabelingSetGenerationTaskRun pTransformId_ pOutputS3Path_ =
  StartMLLabelingSetGenerationTaskRun'
    { _smllsgtrTransformId =
        pTransformId_,
      _smllsgtrOutputS3Path = pOutputS3Path_
    }

-- | The unique identifier of the machine learning transform.
smllsgtrTransformId :: Lens' StartMLLabelingSetGenerationTaskRun Text
smllsgtrTransformId = lens _smllsgtrTransformId (\s a -> s {_smllsgtrTransformId = a})

-- | The Amazon Simple Storage Service (Amazon S3) path where you generate the labeling set.
smllsgtrOutputS3Path :: Lens' StartMLLabelingSetGenerationTaskRun Text
smllsgtrOutputS3Path = lens _smllsgtrOutputS3Path (\s a -> s {_smllsgtrOutputS3Path = a})

instance AWSRequest StartMLLabelingSetGenerationTaskRun where
  type
    Rs StartMLLabelingSetGenerationTaskRun =
      StartMLLabelingSetGenerationTaskRunResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          StartMLLabelingSetGenerationTaskRunResponse'
            <$> (x .?> "TaskRunId") <*> (pure (fromEnum s))
      )

instance Hashable StartMLLabelingSetGenerationTaskRun

instance NFData StartMLLabelingSetGenerationTaskRun

instance ToHeaders StartMLLabelingSetGenerationTaskRun where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSGlue.StartMLLabelingSetGenerationTaskRun" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartMLLabelingSetGenerationTaskRun where
  toJSON StartMLLabelingSetGenerationTaskRun' {..} =
    object
      ( catMaybes
          [ Just ("TransformId" .= _smllsgtrTransformId),
            Just ("OutputS3Path" .= _smllsgtrOutputS3Path)
          ]
      )

instance ToPath StartMLLabelingSetGenerationTaskRun where
  toPath = const "/"

instance ToQuery StartMLLabelingSetGenerationTaskRun where
  toQuery = const mempty

-- | /See:/ 'startMLLabelingSetGenerationTaskRunResponse' smart constructor.
data StartMLLabelingSetGenerationTaskRunResponse = StartMLLabelingSetGenerationTaskRunResponse'
  { _smllsgtrrsTaskRunId ::
      !( Maybe
           Text
       ),
    _smllsgtrrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'StartMLLabelingSetGenerationTaskRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smllsgtrrsTaskRunId' - The unique run identifier that is associated with this task run.
--
-- * 'smllsgtrrsResponseStatus' - -- | The response status code.
startMLLabelingSetGenerationTaskRunResponse ::
  -- | 'smllsgtrrsResponseStatus'
  Int ->
  StartMLLabelingSetGenerationTaskRunResponse
startMLLabelingSetGenerationTaskRunResponse pResponseStatus_ =
  StartMLLabelingSetGenerationTaskRunResponse'
    { _smllsgtrrsTaskRunId =
        Nothing,
      _smllsgtrrsResponseStatus = pResponseStatus_
    }

-- | The unique run identifier that is associated with this task run.
smllsgtrrsTaskRunId :: Lens' StartMLLabelingSetGenerationTaskRunResponse (Maybe Text)
smllsgtrrsTaskRunId = lens _smllsgtrrsTaskRunId (\s a -> s {_smllsgtrrsTaskRunId = a})

-- | -- | The response status code.
smllsgtrrsResponseStatus :: Lens' StartMLLabelingSetGenerationTaskRunResponse Int
smllsgtrrsResponseStatus = lens _smllsgtrrsResponseStatus (\s a -> s {_smllsgtrrsResponseStatus = a})

instance NFData StartMLLabelingSetGenerationTaskRunResponse
