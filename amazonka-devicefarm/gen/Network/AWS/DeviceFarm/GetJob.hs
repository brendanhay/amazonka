{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetJob
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a job.
--
-- <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_GetJob.html>
module Network.AWS.DeviceFarm.GetJob
    (
    -- * Request
      GetJob
    -- ** Request constructor
    , getJob
    -- ** Request lenses
    , gjArn

    -- * Response
    , GetJobResponse
    -- ** Response constructor
    , getJobResponse
    -- ** Response lenses
    , gjrJob
    , gjrStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the get job operation.
--
-- /See:/ 'getJob' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gjArn'
newtype GetJob = GetJob'
    { _gjArn :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetJob' smart constructor.
getJob :: Text -> GetJob
getJob pArn =
    GetJob'
    { _gjArn = pArn
    }

-- | The job\'s ARN.
gjArn :: Lens' GetJob Text
gjArn = lens _gjArn (\ s a -> s{_gjArn = a});

instance AWSRequest GetJob where
        type Sv GetJob = DeviceFarm
        type Rs GetJob = GetJobResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetJobResponse' <$>
                   (x .?> "job") <*> (pure (fromEnum s)))

instance ToHeaders GetJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.GetJob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetJob where
        toJSON GetJob'{..} = object ["arn" .= _gjArn]

instance ToPath GetJob where
        toPath = const "/"

instance ToQuery GetJob where
        toQuery = const mempty

-- | Represents the result of a get job request.
--
-- /See:/ 'getJobResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gjrJob'
--
-- * 'gjrStatus'
data GetJobResponse = GetJobResponse'
    { _gjrJob    :: !(Maybe Job)
    , _gjrStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetJobResponse' smart constructor.
getJobResponse :: Int -> GetJobResponse
getJobResponse pStatus =
    GetJobResponse'
    { _gjrJob = Nothing
    , _gjrStatus = pStatus
    }

-- | FIXME: Undocumented member.
gjrJob :: Lens' GetJobResponse (Maybe Job)
gjrJob = lens _gjrJob (\ s a -> s{_gjrJob = a});

-- | FIXME: Undocumented member.
gjrStatus :: Lens' GetJobResponse Int
gjrStatus = lens _gjrStatus (\ s a -> s{_gjrStatus = a});
