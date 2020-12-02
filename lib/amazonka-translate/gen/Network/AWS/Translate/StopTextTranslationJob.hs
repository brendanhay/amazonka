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
-- Module      : Network.AWS.Translate.StopTextTranslationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an asynchronous batch translation job that is in progress.
--
--
-- If the job's state is @IN_PROGRESS@ , the job will be marked for termination and put into the @STOP_REQUESTED@ state. If the job completes before it can be stopped, it is put into the @COMPLETED@ state. Otherwise, the job is put into the @STOPPED@ state.
--
-- Asynchronous batch translation jobs are started with the 'StartTextTranslationJob' operation. You can use the 'DescribeTextTranslationJob' or 'ListTextTranslationJobs' operations to get a batch translation job's @JobId@ .
module Network.AWS.Translate.StopTextTranslationJob
  ( -- * Creating a Request
    stopTextTranslationJob,
    StopTextTranslationJob,

    -- * Request Lenses
    sttjJobId,

    -- * Destructuring the Response
    stopTextTranslationJobResponse,
    StopTextTranslationJobResponse,

    -- * Response Lenses
    sttjrsJobId,
    sttjrsJobStatus,
    sttjrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Translate.Types

-- | /See:/ 'stopTextTranslationJob' smart constructor.
newtype StopTextTranslationJob = StopTextTranslationJob'
  { _sttjJobId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopTextTranslationJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sttjJobId' - The job ID of the job to be stopped.
stopTextTranslationJob ::
  -- | 'sttjJobId'
  Text ->
  StopTextTranslationJob
stopTextTranslationJob pJobId_ =
  StopTextTranslationJob' {_sttjJobId = pJobId_}

-- | The job ID of the job to be stopped.
sttjJobId :: Lens' StopTextTranslationJob Text
sttjJobId = lens _sttjJobId (\s a -> s {_sttjJobId = a})

instance AWSRequest StopTextTranslationJob where
  type Rs StopTextTranslationJob = StopTextTranslationJobResponse
  request = postJSON translate
  response =
    receiveJSON
      ( \s h x ->
          StopTextTranslationJobResponse'
            <$> (x .?> "JobId") <*> (x .?> "JobStatus") <*> (pure (fromEnum s))
      )

instance Hashable StopTextTranslationJob

instance NFData StopTextTranslationJob

instance ToHeaders StopTextTranslationJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSShineFrontendService_20170701.StopTextTranslationJob" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StopTextTranslationJob where
  toJSON StopTextTranslationJob' {..} =
    object (catMaybes [Just ("JobId" .= _sttjJobId)])

instance ToPath StopTextTranslationJob where
  toPath = const "/"

instance ToQuery StopTextTranslationJob where
  toQuery = const mempty

-- | /See:/ 'stopTextTranslationJobResponse' smart constructor.
data StopTextTranslationJobResponse = StopTextTranslationJobResponse'
  { _sttjrsJobId ::
      !(Maybe Text),
    _sttjrsJobStatus ::
      !(Maybe JobStatus),
    _sttjrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopTextTranslationJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sttjrsJobId' - The job ID of the stopped batch translation job.
--
-- * 'sttjrsJobStatus' - The status of the designated job. Upon successful completion, the job's status will be @STOPPED@ .
--
-- * 'sttjrsResponseStatus' - -- | The response status code.
stopTextTranslationJobResponse ::
  -- | 'sttjrsResponseStatus'
  Int ->
  StopTextTranslationJobResponse
stopTextTranslationJobResponse pResponseStatus_ =
  StopTextTranslationJobResponse'
    { _sttjrsJobId = Nothing,
      _sttjrsJobStatus = Nothing,
      _sttjrsResponseStatus = pResponseStatus_
    }

-- | The job ID of the stopped batch translation job.
sttjrsJobId :: Lens' StopTextTranslationJobResponse (Maybe Text)
sttjrsJobId = lens _sttjrsJobId (\s a -> s {_sttjrsJobId = a})

-- | The status of the designated job. Upon successful completion, the job's status will be @STOPPED@ .
sttjrsJobStatus :: Lens' StopTextTranslationJobResponse (Maybe JobStatus)
sttjrsJobStatus = lens _sttjrsJobStatus (\s a -> s {_sttjrsJobStatus = a})

-- | -- | The response status code.
sttjrsResponseStatus :: Lens' StopTextTranslationJobResponse Int
sttjrsResponseStatus = lens _sttjrsResponseStatus (\s a -> s {_sttjrsResponseStatus = a})

instance NFData StopTextTranslationJobResponse
