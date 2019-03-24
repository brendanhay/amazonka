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
-- Module      : Network.AWS.SageMaker.StopTransformJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a transform job.
--
--
-- When Amazon SageMaker receives a @StopTransformJob@ request, the status of the job changes to @Stopping@ . After Amazon SageMaker stops the job, the status is set to @Stopped@ . When you stop a transform job before it is completed, Amazon SageMaker doesn't store the job's output in Amazon S3.
--
module Network.AWS.SageMaker.StopTransformJob
    (
    -- * Creating a Request
      stopTransformJob
    , StopTransformJob
    -- * Request Lenses
    , stjTransformJobName

    -- * Destructuring the Response
    , stopTransformJobResponse
    , StopTransformJobResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'stopTransformJob' smart constructor.
newtype StopTransformJob = StopTransformJob'
  { _stjTransformJobName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopTransformJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stjTransformJobName' - The name of the transform job to stop.
stopTransformJob
    :: Text -- ^ 'stjTransformJobName'
    -> StopTransformJob
stopTransformJob pTransformJobName_ =
  StopTransformJob' {_stjTransformJobName = pTransformJobName_}


-- | The name of the transform job to stop.
stjTransformJobName :: Lens' StopTransformJob Text
stjTransformJobName = lens _stjTransformJobName (\ s a -> s{_stjTransformJobName = a})

instance AWSRequest StopTransformJob where
        type Rs StopTransformJob = StopTransformJobResponse
        request = postJSON sageMaker
        response = receiveNull StopTransformJobResponse'

instance Hashable StopTransformJob where

instance NFData StopTransformJob where

instance ToHeaders StopTransformJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.StopTransformJob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopTransformJob where
        toJSON StopTransformJob'{..}
          = object
              (catMaybes
                 [Just ("TransformJobName" .= _stjTransformJobName)])

instance ToPath StopTransformJob where
        toPath = const "/"

instance ToQuery StopTransformJob where
        toQuery = const mempty

-- | /See:/ 'stopTransformJobResponse' smart constructor.
data StopTransformJobResponse =
  StopTransformJobResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopTransformJobResponse' with the minimum fields required to make a request.
--
stopTransformJobResponse
    :: StopTransformJobResponse
stopTransformJobResponse = StopTransformJobResponse'


instance NFData StopTransformJobResponse where
