{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudTrail.StopLogging
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

-- | Suspends the recording of AWS API calls and log file delivery for the
-- specified trail. Under most circumstances, there is no need to use this
-- action. You can update a trail without stopping it first. This action is
-- the only way to stop recording.
--
-- <http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_StopLogging.html>
module Network.AWS.CloudTrail.StopLogging
    (
    -- * Request
      StopLogging
    -- ** Request constructor
    , stopLogging
    -- ** Request lenses
    , slName

    -- * Response
    , StopLoggingResponse
    -- ** Response constructor
    , stopLoggingResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudTrail.Types

-- | /See:/ 'stopLogging' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slName'
newtype StopLogging = StopLogging'{_slName :: Text} deriving (Eq, Read, Show)

-- | 'StopLogging' smart constructor.
stopLogging :: Text -> StopLogging
stopLogging pName = StopLogging'{_slName = pName};

-- | Communicates to CloudTrail the name of the trail for which to stop
-- logging AWS API calls.
slName :: Lens' StopLogging Text
slName = lens _slName (\ s a -> s{_slName = a});

instance AWSRequest StopLogging where
        type Sv StopLogging = CloudTrail
        type Rs StopLogging = StopLoggingResponse
        request = postJSON
        response = receiveNull StopLoggingResponse'

instance ToHeaders StopLogging where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.StopLogging"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopLogging where
        toJSON StopLogging'{..} = object ["Name" .= _slName]

instance ToPath StopLogging where
        toPath = const "/"

instance ToQuery StopLogging where
        toQuery = const mempty

-- | /See:/ 'stopLoggingResponse' smart constructor.
data StopLoggingResponse = StopLoggingResponse' deriving (Eq, Read, Show)

-- | 'StopLoggingResponse' smart constructor.
stopLoggingResponse :: StopLoggingResponse
stopLoggingResponse = StopLoggingResponse';
