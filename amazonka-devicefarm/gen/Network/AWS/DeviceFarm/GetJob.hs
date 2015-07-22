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
    , gjrqArn

    -- * Response
    , GetJobResponse
    -- ** Response constructor
    , getJobResponse
    -- ** Response lenses
    , gjrsJob
    , gjrsStatus
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
-- * 'gjrqArn'
newtype GetJob = GetJob'
    { _gjrqArn :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetJob' smart constructor.
getJob :: Text -> GetJob
getJob pArn =
    GetJob'
    { _gjrqArn = pArn
    }

-- | The job\'s ARN.
gjrqArn :: Lens' GetJob Text
gjrqArn = lens _gjrqArn (\ s a -> s{_gjrqArn = a});

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
        toJSON GetJob'{..} = object ["arn" .= _gjrqArn]

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
-- * 'gjrsJob'
--
-- * 'gjrsStatus'
data GetJobResponse = GetJobResponse'
    { _gjrsJob    :: !(Maybe Job)
    , _gjrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetJobResponse' smart constructor.
getJobResponse :: Int -> GetJobResponse
getJobResponse pStatus =
    GetJobResponse'
    { _gjrsJob = Nothing
    , _gjrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
gjrsJob :: Lens' GetJobResponse (Maybe Job)
gjrsJob = lens _gjrsJob (\ s a -> s{_gjrsJob = a});

-- | FIXME: Undocumented member.
gjrsStatus :: Lens' GetJobResponse Int
gjrsStatus = lens _gjrsStatus (\ s a -> s{_gjrsStatus = a});
