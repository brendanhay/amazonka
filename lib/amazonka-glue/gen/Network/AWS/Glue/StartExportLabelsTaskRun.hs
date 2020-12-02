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
-- Module      : Network.AWS.Glue.StartExportLabelsTaskRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Begins an asynchronous task to export all labeled data for a particular transform. This task is the only label-related API call that is not part of the typical active learning workflow. You typically use @StartExportLabelsTaskRun@ when you want to work with all of your existing labels at the same time, such as when you want to remove or change labels that were previously submitted as truth. This API operation accepts the @TransformId@ whose labels you want to export and an Amazon Simple Storage Service (Amazon S3) path to export the labels to. The operation returns a @TaskRunId@ . You can check on the status of your task run by calling the @GetMLTaskRun@ API.
module Network.AWS.Glue.StartExportLabelsTaskRun
  ( -- * Creating a Request
    startExportLabelsTaskRun,
    StartExportLabelsTaskRun,

    -- * Request Lenses
    seltrTransformId,
    seltrOutputS3Path,

    -- * Destructuring the Response
    startExportLabelsTaskRunResponse,
    StartExportLabelsTaskRunResponse,

    -- * Response Lenses
    seltrrsTaskRunId,
    seltrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startExportLabelsTaskRun' smart constructor.
data StartExportLabelsTaskRun = StartExportLabelsTaskRun'
  { _seltrTransformId ::
      !Text,
    _seltrOutputS3Path :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartExportLabelsTaskRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seltrTransformId' - The unique identifier of the machine learning transform.
--
-- * 'seltrOutputS3Path' - The Amazon S3 path where you export the labels.
startExportLabelsTaskRun ::
  -- | 'seltrTransformId'
  Text ->
  -- | 'seltrOutputS3Path'
  Text ->
  StartExportLabelsTaskRun
startExportLabelsTaskRun pTransformId_ pOutputS3Path_ =
  StartExportLabelsTaskRun'
    { _seltrTransformId = pTransformId_,
      _seltrOutputS3Path = pOutputS3Path_
    }

-- | The unique identifier of the machine learning transform.
seltrTransformId :: Lens' StartExportLabelsTaskRun Text
seltrTransformId = lens _seltrTransformId (\s a -> s {_seltrTransformId = a})

-- | The Amazon S3 path where you export the labels.
seltrOutputS3Path :: Lens' StartExportLabelsTaskRun Text
seltrOutputS3Path = lens _seltrOutputS3Path (\s a -> s {_seltrOutputS3Path = a})

instance AWSRequest StartExportLabelsTaskRun where
  type Rs StartExportLabelsTaskRun = StartExportLabelsTaskRunResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          StartExportLabelsTaskRunResponse'
            <$> (x .?> "TaskRunId") <*> (pure (fromEnum s))
      )

instance Hashable StartExportLabelsTaskRun

instance NFData StartExportLabelsTaskRun

instance ToHeaders StartExportLabelsTaskRun where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSGlue.StartExportLabelsTaskRun" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartExportLabelsTaskRun where
  toJSON StartExportLabelsTaskRun' {..} =
    object
      ( catMaybes
          [ Just ("TransformId" .= _seltrTransformId),
            Just ("OutputS3Path" .= _seltrOutputS3Path)
          ]
      )

instance ToPath StartExportLabelsTaskRun where
  toPath = const "/"

instance ToQuery StartExportLabelsTaskRun where
  toQuery = const mempty

-- | /See:/ 'startExportLabelsTaskRunResponse' smart constructor.
data StartExportLabelsTaskRunResponse = StartExportLabelsTaskRunResponse'
  { _seltrrsTaskRunId ::
      !(Maybe Text),
    _seltrrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartExportLabelsTaskRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seltrrsTaskRunId' - The unique identifier for the task run.
--
-- * 'seltrrsResponseStatus' - -- | The response status code.
startExportLabelsTaskRunResponse ::
  -- | 'seltrrsResponseStatus'
  Int ->
  StartExportLabelsTaskRunResponse
startExportLabelsTaskRunResponse pResponseStatus_ =
  StartExportLabelsTaskRunResponse'
    { _seltrrsTaskRunId = Nothing,
      _seltrrsResponseStatus = pResponseStatus_
    }

-- | The unique identifier for the task run.
seltrrsTaskRunId :: Lens' StartExportLabelsTaskRunResponse (Maybe Text)
seltrrsTaskRunId = lens _seltrrsTaskRunId (\s a -> s {_seltrrsTaskRunId = a})

-- | -- | The response status code.
seltrrsResponseStatus :: Lens' StartExportLabelsTaskRunResponse Int
seltrrsResponseStatus = lens _seltrrsResponseStatus (\s a -> s {_seltrrsResponseStatus = a})

instance NFData StartExportLabelsTaskRunResponse
