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
      startLogging
    , StartLogging
    -- * Request Lenses
    , sName

    -- * Destructuring the Response
    , startLoggingResponse
    , StartLoggingResponse
    -- * Response Lenses
    , srsStatus
    ) where

import           Network.AWS.CloudTrail.Types
import           Network.AWS.CloudTrail.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to CloudTrail to start logging AWS API calls for an account.
--
-- /See:/ 'startLogging' smart constructor.
newtype StartLogging = StartLogging'
    { _sName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StartLogging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sName'
startLogging
    :: Text -- ^ 'sName'
    -> StartLogging
startLogging pName_ =
    StartLogging'
    { _sName = pName_
    }

-- | The name of the trail for which CloudTrail logs AWS API calls.
sName :: Lens' StartLogging Text
sName = lens _sName (\ s a -> s{_sName = a});

instance AWSRequest StartLogging where
        type Rs StartLogging = StartLoggingResponse
        request = postJSON cloudTrail
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
        toJSON StartLogging'{..}
          = object (catMaybes [Just ("Name" .= _sName)])

instance ToPath StartLogging where
        toPath = const "/"

instance ToQuery StartLogging where
        toQuery = const mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'startLoggingResponse' smart constructor.
newtype StartLoggingResponse = StartLoggingResponse'
    { _srsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StartLoggingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsStatus'
startLoggingResponse
    :: Int -- ^ 'srsStatus'
    -> StartLoggingResponse
startLoggingResponse pStatus_ =
    StartLoggingResponse'
    { _srsStatus = pStatus_
    }

-- | The response status code.
srsStatus :: Lens' StartLoggingResponse Int
srsStatus = lens _srsStatus (\ s a -> s{_srsStatus = a});
