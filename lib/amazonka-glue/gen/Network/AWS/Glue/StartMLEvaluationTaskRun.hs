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
-- Module      : Network.AWS.Glue.StartMLEvaluationTaskRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a task to estimate the quality of the transform.
--
--
-- When you provide label sets as examples of truth, AWS Glue machine learning uses some of those examples to learn from them. The rest of the labels are used as a test to estimate quality.
--
-- Returns a unique identifier for the run. You can call @GetMLTaskRun@ to get more information about the stats of the @EvaluationTaskRun@ .
module Network.AWS.Glue.StartMLEvaluationTaskRun
  ( -- * Creating a Request
    startMLEvaluationTaskRun,
    StartMLEvaluationTaskRun,

    -- * Request Lenses
    smletrTransformId,

    -- * Destructuring the Response
    startMLEvaluationTaskRunResponse,
    StartMLEvaluationTaskRunResponse,

    -- * Response Lenses
    smletrrsTaskRunId,
    smletrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startMLEvaluationTaskRun' smart constructor.
newtype StartMLEvaluationTaskRun = StartMLEvaluationTaskRun'
  { _smletrTransformId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartMLEvaluationTaskRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smletrTransformId' - The unique identifier of the machine learning transform.
startMLEvaluationTaskRun ::
  -- | 'smletrTransformId'
  Text ->
  StartMLEvaluationTaskRun
startMLEvaluationTaskRun pTransformId_ =
  StartMLEvaluationTaskRun' {_smletrTransformId = pTransformId_}

-- | The unique identifier of the machine learning transform.
smletrTransformId :: Lens' StartMLEvaluationTaskRun Text
smletrTransformId = lens _smletrTransformId (\s a -> s {_smletrTransformId = a})

instance AWSRequest StartMLEvaluationTaskRun where
  type Rs StartMLEvaluationTaskRun = StartMLEvaluationTaskRunResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          StartMLEvaluationTaskRunResponse'
            <$> (x .?> "TaskRunId") <*> (pure (fromEnum s))
      )

instance Hashable StartMLEvaluationTaskRun

instance NFData StartMLEvaluationTaskRun

instance ToHeaders StartMLEvaluationTaskRun where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSGlue.StartMLEvaluationTaskRun" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartMLEvaluationTaskRun where
  toJSON StartMLEvaluationTaskRun' {..} =
    object (catMaybes [Just ("TransformId" .= _smletrTransformId)])

instance ToPath StartMLEvaluationTaskRun where
  toPath = const "/"

instance ToQuery StartMLEvaluationTaskRun where
  toQuery = const mempty

-- | /See:/ 'startMLEvaluationTaskRunResponse' smart constructor.
data StartMLEvaluationTaskRunResponse = StartMLEvaluationTaskRunResponse'
  { _smletrrsTaskRunId ::
      !(Maybe Text),
    _smletrrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartMLEvaluationTaskRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smletrrsTaskRunId' - The unique identifier associated with this run.
--
-- * 'smletrrsResponseStatus' - -- | The response status code.
startMLEvaluationTaskRunResponse ::
  -- | 'smletrrsResponseStatus'
  Int ->
  StartMLEvaluationTaskRunResponse
startMLEvaluationTaskRunResponse pResponseStatus_ =
  StartMLEvaluationTaskRunResponse'
    { _smletrrsTaskRunId = Nothing,
      _smletrrsResponseStatus = pResponseStatus_
    }

-- | The unique identifier associated with this run.
smletrrsTaskRunId :: Lens' StartMLEvaluationTaskRunResponse (Maybe Text)
smletrrsTaskRunId = lens _smletrrsTaskRunId (\s a -> s {_smletrrsTaskRunId = a})

-- | -- | The response status code.
smletrrsResponseStatus :: Lens' StartMLEvaluationTaskRunResponse Int
smletrrsResponseStatus = lens _smletrrsResponseStatus (\s a -> s {_smletrrsResponseStatus = a})

instance NFData StartMLEvaluationTaskRunResponse
