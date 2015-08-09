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
-- Module      : Network.AWS.CloudTrail.StartLogging
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the recording of AWS API calls and log file delivery for a trail.
--
-- /See:/ <http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_StartLogging.html AWS API Reference> for StartLogging.
module Network.AWS.CloudTrail.StartLogging
    (
    -- * Creating a Request
      StartLogging
    , startLogging
    -- * Request Lenses
    , sName

    -- * Destructuring the Response
    , StartLoggingResponse
    , startLoggingResponse
    -- * Response Lenses
    , srsStatus
    ) where

import Network.AWS.CloudTrail.Types
import Network.AWS.CloudTrail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to CloudTrail to start logging AWS API calls for an account.
--
-- /See:/ 'startLogging' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sName'
newtype StartLogging = StartLogging'
    { _sName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StartLogging' smart constructor.
startLogging :: Text -> StartLogging
startLogging pName_ = 
    StartLogging'
    { _sName = pName_
    }

-- | The name of the trail for which CloudTrail logs AWS API calls.
sName :: Lens' StartLogging Text
sName = lens _sName (\ s a -> s{_sName = a});

instance AWSRequest StartLogging where
        type Sv StartLogging = CloudTrail
        type Rs StartLogging = StartLoggingResponse
        request = postJSON
        response
          = receiveEmpty
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
        toJSON StartLogging'{..} = object ["Name" .= _sName]

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
startLoggingResponse pStatus_ = 
    StartLoggingResponse'
    { _srsStatus = pStatus_
    }

-- | Undocumented member.
srsStatus :: Lens' StartLoggingResponse Int
srsStatus = lens _srsStatus (\ s a -> s{_srsStatus = a});
