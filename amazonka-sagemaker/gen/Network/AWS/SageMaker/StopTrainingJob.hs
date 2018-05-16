{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.StopTrainingJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a training job. To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms might use this 120-second window to save the model artifacts, so the results of the training is not lost.
--
--
-- Training algorithms provided by Amazon SageMaker save the intermediate results of a model training job. This intermediate data is a valid model artifact. You can use the model artifacts that are saved when Amazon SageMaker stops a training job to create a model.
--
-- When it receives a @StopTrainingJob@ request, Amazon SageMaker changes the status of the job to @Stopping@ . After Amazon SageMaker stops the job, it sets the status to @Stopped@ .
--
module Network.AWS.SageMaker.StopTrainingJob
    (
    -- * Creating a Request
      stopTrainingJob
    , StopTrainingJob
    -- * Request Lenses
    , stjTrainingJobName

    -- * Destructuring the Response
    , stopTrainingJobResponse
    , StopTrainingJobResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'stopTrainingJob' smart constructor.
newtype StopTrainingJob = StopTrainingJob'
  { _stjTrainingJobName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopTrainingJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stjTrainingJobName' - The name of the training job to stop.
stopTrainingJob
    :: Text -- ^ 'stjTrainingJobName'
    -> StopTrainingJob
stopTrainingJob pTrainingJobName_ =
  StopTrainingJob' {_stjTrainingJobName = pTrainingJobName_}


-- | The name of the training job to stop.
stjTrainingJobName :: Lens' StopTrainingJob Text
stjTrainingJobName = lens _stjTrainingJobName (\ s a -> s{_stjTrainingJobName = a})

instance AWSRequest StopTrainingJob where
        type Rs StopTrainingJob = StopTrainingJobResponse
        request = postJSON sageMaker
        response = receiveNull StopTrainingJobResponse'

instance Hashable StopTrainingJob where

instance NFData StopTrainingJob where

instance ToHeaders StopTrainingJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.StopTrainingJob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopTrainingJob where
        toJSON StopTrainingJob'{..}
          = object
              (catMaybes
                 [Just ("TrainingJobName" .= _stjTrainingJobName)])

instance ToPath StopTrainingJob where
        toPath = const "/"

instance ToQuery StopTrainingJob where
        toQuery = const mempty

-- | /See:/ 'stopTrainingJobResponse' smart constructor.
data StopTrainingJobResponse =
  StopTrainingJobResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopTrainingJobResponse' with the minimum fields required to make a request.
--
stopTrainingJobResponse
    :: StopTrainingJobResponse
stopTrainingJobResponse = StopTrainingJobResponse'


instance NFData StopTrainingJobResponse where
