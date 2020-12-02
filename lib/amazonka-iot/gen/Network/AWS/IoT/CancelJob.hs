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
-- Module      : Network.AWS.IoT.CancelJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a job.
module Network.AWS.IoT.CancelJob
  ( -- * Creating a Request
    cancelJob,
    CancelJob,

    -- * Request Lenses
    cForce,
    cReasonCode,
    cComment,
    cJobId,

    -- * Destructuring the Response
    cancelJobResponse,
    CancelJobResponse,

    -- * Response Lenses
    canrsJobId,
    canrsJobARN,
    canrsDescription,
    canrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'cancelJob' smart constructor.
data CancelJob = CancelJob'
  { _cForce :: !(Maybe Bool),
    _cReasonCode :: !(Maybe Text),
    _cComment :: !(Maybe Text),
    _cJobId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cForce' - (Optional) If @true@ job executions with status "IN_PROGRESS" and "QUEUED" are canceled, otherwise only job executions with status "QUEUED" are canceled. The default is @false@ . Canceling a job which is "IN_PROGRESS", will cause a device which is executing the job to be unable to update the job execution status. Use caution and ensure that each device executing a job which is canceled is able to recover to a valid state.
--
-- * 'cReasonCode' - (Optional)A reason code string that explains why the job was canceled.
--
-- * 'cComment' - An optional comment string describing why the job was canceled.
--
-- * 'cJobId' - The unique identifier you assigned to this job when it was created.
cancelJob ::
  -- | 'cJobId'
  Text ->
  CancelJob
cancelJob pJobId_ =
  CancelJob'
    { _cForce = Nothing,
      _cReasonCode = Nothing,
      _cComment = Nothing,
      _cJobId = pJobId_
    }

-- | (Optional) If @true@ job executions with status "IN_PROGRESS" and "QUEUED" are canceled, otherwise only job executions with status "QUEUED" are canceled. The default is @false@ . Canceling a job which is "IN_PROGRESS", will cause a device which is executing the job to be unable to update the job execution status. Use caution and ensure that each device executing a job which is canceled is able to recover to a valid state.
cForce :: Lens' CancelJob (Maybe Bool)
cForce = lens _cForce (\s a -> s {_cForce = a})

-- | (Optional)A reason code string that explains why the job was canceled.
cReasonCode :: Lens' CancelJob (Maybe Text)
cReasonCode = lens _cReasonCode (\s a -> s {_cReasonCode = a})

-- | An optional comment string describing why the job was canceled.
cComment :: Lens' CancelJob (Maybe Text)
cComment = lens _cComment (\s a -> s {_cComment = a})

-- | The unique identifier you assigned to this job when it was created.
cJobId :: Lens' CancelJob Text
cJobId = lens _cJobId (\s a -> s {_cJobId = a})

instance AWSRequest CancelJob where
  type Rs CancelJob = CancelJobResponse
  request = putJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          CancelJobResponse'
            <$> (x .?> "jobId")
            <*> (x .?> "jobArn")
            <*> (x .?> "description")
            <*> (pure (fromEnum s))
      )

instance Hashable CancelJob

instance NFData CancelJob

instance ToHeaders CancelJob where
  toHeaders = const mempty

instance ToJSON CancelJob where
  toJSON CancelJob' {..} =
    object
      ( catMaybes
          [("reasonCode" .=) <$> _cReasonCode, ("comment" .=) <$> _cComment]
      )

instance ToPath CancelJob where
  toPath CancelJob' {..} = mconcat ["/jobs/", toBS _cJobId, "/cancel"]

instance ToQuery CancelJob where
  toQuery CancelJob' {..} = mconcat ["force" =: _cForce]

-- | /See:/ 'cancelJobResponse' smart constructor.
data CancelJobResponse = CancelJobResponse'
  { _canrsJobId ::
      !(Maybe Text),
    _canrsJobARN :: !(Maybe Text),
    _canrsDescription :: !(Maybe Text),
    _canrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'canrsJobId' - The unique identifier you assigned to this job when it was created.
--
-- * 'canrsJobARN' - The job ARN.
--
-- * 'canrsDescription' - A short text description of the job.
--
-- * 'canrsResponseStatus' - -- | The response status code.
cancelJobResponse ::
  -- | 'canrsResponseStatus'
  Int ->
  CancelJobResponse
cancelJobResponse pResponseStatus_ =
  CancelJobResponse'
    { _canrsJobId = Nothing,
      _canrsJobARN = Nothing,
      _canrsDescription = Nothing,
      _canrsResponseStatus = pResponseStatus_
    }

-- | The unique identifier you assigned to this job when it was created.
canrsJobId :: Lens' CancelJobResponse (Maybe Text)
canrsJobId = lens _canrsJobId (\s a -> s {_canrsJobId = a})

-- | The job ARN.
canrsJobARN :: Lens' CancelJobResponse (Maybe Text)
canrsJobARN = lens _canrsJobARN (\s a -> s {_canrsJobARN = a})

-- | A short text description of the job.
canrsDescription :: Lens' CancelJobResponse (Maybe Text)
canrsDescription = lens _canrsDescription (\s a -> s {_canrsDescription = a})

-- | -- | The response status code.
canrsResponseStatus :: Lens' CancelJobResponse Int
canrsResponseStatus = lens _canrsResponseStatus (\s a -> s {_canrsResponseStatus = a})

instance NFData CancelJobResponse
