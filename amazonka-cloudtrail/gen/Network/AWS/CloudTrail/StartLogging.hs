{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.StartLogging
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Starts the recording of AWS API calls and log file delivery for a trail.
--
-- <http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_StartLogging.html>
module Network.AWS.CloudTrail.StartLogging
    (
    -- * Request
      StartLogging
    -- ** Request constructor
    , startLogging
    -- ** Request lenses
    , srqName

    -- * Response
    , StartLoggingResponse
    -- ** Response constructor
    , startLoggingResponse
    -- ** Response lenses
    , srsStatus
    ) where

import           Network.AWS.CloudTrail.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to CloudTrail to start logging AWS API calls for an account.
--
-- /See:/ 'startLogging' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srqName'
newtype StartLogging = StartLogging'
    { _srqName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StartLogging' smart constructor.
startLogging :: Text -> StartLogging
startLogging pName =
    StartLogging'
    { _srqName = pName
    }

-- | The name of the trail for which CloudTrail logs AWS API calls.
srqName :: Lens' StartLogging Text
srqName = lens _srqName (\ s a -> s{_srqName = a});

instance AWSRequest StartLogging where
        type Sv StartLogging = CloudTrail
        type Rs StartLogging = StartLoggingResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 StartLoggingResponse' <$> (pure (fromEnum s)))

instance ToHeaders StartLogging where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.StartLogging"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartLogging where
        toJSON StartLogging'{..}
          = object ["Name" .= _srqName]

instance ToPath StartLogging where
        toPath = const "/"

instance ToQuery StartLogging where
        toQuery = const mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'startLoggingResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srsStatus'
newtype StartLoggingResponse = StartLoggingResponse'
    { _srsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StartLoggingResponse' smart constructor.
startLoggingResponse :: Int -> StartLoggingResponse
startLoggingResponse pStatus =
    StartLoggingResponse'
    { _srsStatus = pStatus
    }

-- | FIXME: Undocumented member.
srsStatus :: Lens' StartLoggingResponse Int
srsStatus = lens _srsStatus (\ s a -> s{_srsStatus = a});
