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
-- Module      : Network.AWS.SageMaker.StopLabelingJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running labeling job. A job that is stopped cannot be restarted. Any results obtained before the job is stopped are placed in the Amazon S3 output bucket.
module Network.AWS.SageMaker.StopLabelingJob
  ( -- * Creating a Request
    stopLabelingJob,
    StopLabelingJob,

    -- * Request Lenses
    sljLabelingJobName,

    -- * Destructuring the Response
    stopLabelingJobResponse,
    StopLabelingJobResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'stopLabelingJob' smart constructor.
newtype StopLabelingJob = StopLabelingJob'
  { _sljLabelingJobName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopLabelingJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sljLabelingJobName' - The name of the labeling job to stop.
stopLabelingJob ::
  -- | 'sljLabelingJobName'
  Text ->
  StopLabelingJob
stopLabelingJob pLabelingJobName_ =
  StopLabelingJob' {_sljLabelingJobName = pLabelingJobName_}

-- | The name of the labeling job to stop.
sljLabelingJobName :: Lens' StopLabelingJob Text
sljLabelingJobName = lens _sljLabelingJobName (\s a -> s {_sljLabelingJobName = a})

instance AWSRequest StopLabelingJob where
  type Rs StopLabelingJob = StopLabelingJobResponse
  request = postJSON sageMaker
  response = receiveNull StopLabelingJobResponse'

instance Hashable StopLabelingJob

instance NFData StopLabelingJob

instance ToHeaders StopLabelingJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.StopLabelingJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StopLabelingJob where
  toJSON StopLabelingJob' {..} =
    object
      (catMaybes [Just ("LabelingJobName" .= _sljLabelingJobName)])

instance ToPath StopLabelingJob where
  toPath = const "/"

instance ToQuery StopLabelingJob where
  toQuery = const mempty

-- | /See:/ 'stopLabelingJobResponse' smart constructor.
data StopLabelingJobResponse = StopLabelingJobResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopLabelingJobResponse' with the minimum fields required to make a request.
stopLabelingJobResponse ::
  StopLabelingJobResponse
stopLabelingJobResponse = StopLabelingJobResponse'

instance NFData StopLabelingJobResponse
