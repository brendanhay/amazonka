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
-- Module      : Network.AWS.SageMaker.StopCompilationJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a model compilation job.
--
--
-- To stop a job, Amazon SageMaker sends the algorithm the SIGTERM signal. This gracefully shuts the job down. If the job hasn't stopped, it sends the SIGKILL signal.
--
-- When it receives a @StopCompilationJob@ request, Amazon SageMaker changes the 'CompilationJobSummary$CompilationJobStatus' of the job to @Stopping@ . After Amazon SageMaker stops the job, it sets the 'CompilationJobSummary$CompilationJobStatus' to @Stopped@ .
--
module Network.AWS.SageMaker.StopCompilationJob
    (
    -- * Creating a Request
      stopCompilationJob
    , StopCompilationJob
    -- * Request Lenses
    , scjCompilationJobName

    -- * Destructuring the Response
    , stopCompilationJobResponse
    , StopCompilationJobResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'stopCompilationJob' smart constructor.
newtype StopCompilationJob = StopCompilationJob'
  { _scjCompilationJobName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopCompilationJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scjCompilationJobName' - The name of the model compilation job to stop.
stopCompilationJob
    :: Text -- ^ 'scjCompilationJobName'
    -> StopCompilationJob
stopCompilationJob pCompilationJobName_ =
  StopCompilationJob' {_scjCompilationJobName = pCompilationJobName_}


-- | The name of the model compilation job to stop.
scjCompilationJobName :: Lens' StopCompilationJob Text
scjCompilationJobName = lens _scjCompilationJobName (\ s a -> s{_scjCompilationJobName = a})

instance AWSRequest StopCompilationJob where
        type Rs StopCompilationJob =
             StopCompilationJobResponse
        request = postJSON sageMaker
        response = receiveNull StopCompilationJobResponse'

instance Hashable StopCompilationJob where

instance NFData StopCompilationJob where

instance ToHeaders StopCompilationJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.StopCompilationJob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopCompilationJob where
        toJSON StopCompilationJob'{..}
          = object
              (catMaybes
                 [Just
                    ("CompilationJobName" .= _scjCompilationJobName)])

instance ToPath StopCompilationJob where
        toPath = const "/"

instance ToQuery StopCompilationJob where
        toQuery = const mempty

-- | /See:/ 'stopCompilationJobResponse' smart constructor.
data StopCompilationJobResponse =
  StopCompilationJobResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopCompilationJobResponse' with the minimum fields required to make a request.
--
stopCompilationJobResponse
    :: StopCompilationJobResponse
stopCompilationJobResponse = StopCompilationJobResponse'


instance NFData StopCompilationJobResponse where
