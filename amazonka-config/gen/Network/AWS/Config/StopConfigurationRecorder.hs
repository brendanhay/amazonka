{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Config.StopConfigurationRecorder
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

-- | Stops recording configurations of all the resources associated with the
-- account.
--
-- <http://docs.aws.amazon.com/config/latest/APIReference/API_StopConfigurationRecorder.html>
module Network.AWS.Config.StopConfigurationRecorder
    (
    -- * Request
      StopConfigurationRecorder
    -- ** Request constructor
    , stopConfigurationRecorder
    -- ** Request lenses
    , scrConfigurationRecorderName

    -- * Response
    , StopConfigurationRecorderResponse
    -- ** Response constructor
    , stopConfigurationRecorderResponse
    ) where

import Network.AWS.Config.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopConfigurationRecorder' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scrConfigurationRecorderName'
newtype StopConfigurationRecorder = StopConfigurationRecorder'{_scrConfigurationRecorderName :: Text} deriving (Eq, Read, Show)

-- | 'StopConfigurationRecorder' smart constructor.
stopConfigurationRecorder :: Text -> StopConfigurationRecorder
stopConfigurationRecorder pConfigurationRecorderName = StopConfigurationRecorder'{_scrConfigurationRecorderName = pConfigurationRecorderName};

-- | The name of the recorder object that records each configuration change
-- made to the resources.
scrConfigurationRecorderName :: Lens' StopConfigurationRecorder Text
scrConfigurationRecorderName = lens _scrConfigurationRecorderName (\ s a -> s{_scrConfigurationRecorderName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest StopConfigurationRecorder where
        type Sv StopConfigurationRecorder = Config
        type Rs StopConfigurationRecorder =
             StopConfigurationRecorderResponse
        request = postJSON
        response
          = receiveNull StopConfigurationRecorderResponse'

instance ToHeaders StopConfigurationRecorder where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.StopConfigurationRecorder" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopConfigurationRecorder where
        toJSON StopConfigurationRecorder'{..}
          = object
              ["ConfigurationRecorderName" .=
                 _scrConfigurationRecorderName]

instance ToPath StopConfigurationRecorder where
        toPath = const "/"

instance ToQuery StopConfigurationRecorder where
        toQuery = const mempty

-- | /See:/ 'stopConfigurationRecorderResponse' smart constructor.
data StopConfigurationRecorderResponse = StopConfigurationRecorderResponse' deriving (Eq, Read, Show)

-- | 'StopConfigurationRecorderResponse' smart constructor.
stopConfigurationRecorderResponse :: StopConfigurationRecorderResponse
stopConfigurationRecorderResponse = StopConfigurationRecorderResponse';
