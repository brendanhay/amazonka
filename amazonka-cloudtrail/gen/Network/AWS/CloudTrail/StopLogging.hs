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
    -- ** Response lenses
    , slrStatusCode
    ) where

import Network.AWS.CloudTrail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Passes the request to CloudTrail to stop logging AWS API calls for the
-- specified account.
--
-- /See:/ 'stopLogging' smart constructor.
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
        response
          = receiveJSON
              (\ s h x ->
                 StopLoggingResponse' <$> (pure (fromEnum s)))

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

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'stopLoggingResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slrStatusCode'
newtype StopLoggingResponse = StopLoggingResponse'{_slrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'StopLoggingResponse' smart constructor.
stopLoggingResponse :: Int -> StopLoggingResponse
stopLoggingResponse pStatusCode = StopLoggingResponse'{_slrStatusCode = pStatusCode};

-- | FIXME: Undocumented member.
slrStatusCode :: Lens' StopLoggingResponse Int
slrStatusCode = lens _slrStatusCode (\ s a -> s{_slrStatusCode = a});
