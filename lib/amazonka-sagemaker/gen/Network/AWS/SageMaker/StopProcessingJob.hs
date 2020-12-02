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
-- Module      : Network.AWS.SageMaker.StopProcessingJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a processing job.
module Network.AWS.SageMaker.StopProcessingJob
  ( -- * Creating a Request
    stopProcessingJob,
    StopProcessingJob,

    -- * Request Lenses
    spjProcessingJobName,

    -- * Destructuring the Response
    stopProcessingJobResponse,
    StopProcessingJobResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'stopProcessingJob' smart constructor.
newtype StopProcessingJob = StopProcessingJob'
  { _spjProcessingJobName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopProcessingJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spjProcessingJobName' - The name of the processing job to stop.
stopProcessingJob ::
  -- | 'spjProcessingJobName'
  Text ->
  StopProcessingJob
stopProcessingJob pProcessingJobName_ =
  StopProcessingJob' {_spjProcessingJobName = pProcessingJobName_}

-- | The name of the processing job to stop.
spjProcessingJobName :: Lens' StopProcessingJob Text
spjProcessingJobName = lens _spjProcessingJobName (\s a -> s {_spjProcessingJobName = a})

instance AWSRequest StopProcessingJob where
  type Rs StopProcessingJob = StopProcessingJobResponse
  request = postJSON sageMaker
  response = receiveNull StopProcessingJobResponse'

instance Hashable StopProcessingJob

instance NFData StopProcessingJob

instance ToHeaders StopProcessingJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.StopProcessingJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StopProcessingJob where
  toJSON StopProcessingJob' {..} =
    object
      (catMaybes [Just ("ProcessingJobName" .= _spjProcessingJobName)])

instance ToPath StopProcessingJob where
  toPath = const "/"

instance ToQuery StopProcessingJob where
  toQuery = const mempty

-- | /See:/ 'stopProcessingJobResponse' smart constructor.
data StopProcessingJobResponse = StopProcessingJobResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopProcessingJobResponse' with the minimum fields required to make a request.
stopProcessingJobResponse ::
  StopProcessingJobResponse
stopProcessingJobResponse = StopProcessingJobResponse'

instance NFData StopProcessingJobResponse
