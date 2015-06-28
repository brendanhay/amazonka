{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudTrail.StartLogging
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Starts the recording of AWS API calls and log file delivery for a trail.
--
-- <http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_StartLogging.html>
module Network.AWS.CloudTrail.StartLogging
    (
    -- * Request
      StartLogging
    -- ** Request constructor
    , startLogging
    -- ** Request lenses
    , staName

    -- * Response
    , StartLoggingResponse
    -- ** Response constructor
    , startLoggingResponse
    -- ** Response lenses
    , staStatus
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
-- * 'staName'
newtype StartLogging = StartLogging'
    { _staName :: Text
    } deriving (Eq,Read,Show)

-- | 'StartLogging' smart constructor.
startLogging :: Text -> StartLogging
startLogging pName =
    StartLogging'
    { _staName = pName
    }

-- | The name of the trail for which CloudTrail logs AWS API calls.
staName :: Lens' StartLogging Text
staName = lens _staName (\ s a -> s{_staName = a});

instance AWSRequest StartLogging where
        type Sv StartLogging = CloudTrail
        type Rs StartLogging = StartLoggingResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x -> StartLoggingResponse' <$> (pure s))

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
          = object ["Name" .= _staName]

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
-- * 'staStatus'
newtype StartLoggingResponse = StartLoggingResponse'
    { _staStatus :: Status
    } deriving (Eq,Show)

-- | 'StartLoggingResponse' smart constructor.
startLoggingResponse :: Status -> StartLoggingResponse
startLoggingResponse pStatus =
    StartLoggingResponse'
    { _staStatus = pStatus
    }

-- | FIXME: Undocumented member.
staStatus :: Lens' StartLoggingResponse Status
staStatus = lens _staStatus (\ s a -> s{_staStatus = a});
