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
-- Module      : Network.AWS.SageMaker.StopHyperParameterTuningJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running hyperparameter tuning job and all running training jobs that the tuning job launched.
--
--
-- All model artifacts output from the training jobs are stored in Amazon Simple Storage Service (Amazon S3). All data that the training jobs write to Amazon CloudWatch Logs are still available in CloudWatch. After the tuning job moves to the @Stopped@ state, it releases all reserved resources for the tuning job.
module Network.AWS.SageMaker.StopHyperParameterTuningJob
  ( -- * Creating a Request
    stopHyperParameterTuningJob,
    StopHyperParameterTuningJob,

    -- * Request Lenses
    shptjHyperParameterTuningJobName,

    -- * Destructuring the Response
    stopHyperParameterTuningJobResponse,
    StopHyperParameterTuningJobResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'stopHyperParameterTuningJob' smart constructor.
newtype StopHyperParameterTuningJob = StopHyperParameterTuningJob'
  { _shptjHyperParameterTuningJobName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopHyperParameterTuningJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'shptjHyperParameterTuningJobName' - The name of the tuning job to stop.
stopHyperParameterTuningJob ::
  -- | 'shptjHyperParameterTuningJobName'
  Text ->
  StopHyperParameterTuningJob
stopHyperParameterTuningJob pHyperParameterTuningJobName_ =
  StopHyperParameterTuningJob'
    { _shptjHyperParameterTuningJobName =
        pHyperParameterTuningJobName_
    }

-- | The name of the tuning job to stop.
shptjHyperParameterTuningJobName :: Lens' StopHyperParameterTuningJob Text
shptjHyperParameterTuningJobName = lens _shptjHyperParameterTuningJobName (\s a -> s {_shptjHyperParameterTuningJobName = a})

instance AWSRequest StopHyperParameterTuningJob where
  type
    Rs StopHyperParameterTuningJob =
      StopHyperParameterTuningJobResponse
  request = postJSON sageMaker
  response = receiveNull StopHyperParameterTuningJobResponse'

instance Hashable StopHyperParameterTuningJob

instance NFData StopHyperParameterTuningJob

instance ToHeaders StopHyperParameterTuningJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.StopHyperParameterTuningJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StopHyperParameterTuningJob where
  toJSON StopHyperParameterTuningJob' {..} =
    object
      ( catMaybes
          [ Just
              ( "HyperParameterTuningJobName"
                  .= _shptjHyperParameterTuningJobName
              )
          ]
      )

instance ToPath StopHyperParameterTuningJob where
  toPath = const "/"

instance ToQuery StopHyperParameterTuningJob where
  toQuery = const mempty

-- | /See:/ 'stopHyperParameterTuningJobResponse' smart constructor.
data StopHyperParameterTuningJobResponse = StopHyperParameterTuningJobResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopHyperParameterTuningJobResponse' with the minimum fields required to make a request.
stopHyperParameterTuningJobResponse ::
  StopHyperParameterTuningJobResponse
stopHyperParameterTuningJobResponse =
  StopHyperParameterTuningJobResponse'

instance NFData StopHyperParameterTuningJobResponse
