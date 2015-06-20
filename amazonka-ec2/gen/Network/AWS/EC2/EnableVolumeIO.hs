{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.EnableVolumeIO
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

-- | Enables I\/O operations for a volume that had I\/O operations disabled
-- because the data on the volume was potentially inconsistent.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-EnableVolumeIO.html>
module Network.AWS.EC2.EnableVolumeIO
    (
    -- * Request
      EnableVolumeIO
    -- ** Request constructor
    , enableVolumeIO
    -- ** Request lenses
    , evioDryRun
    , evioVolumeId

    -- * Response
    , EnableVolumeIOResponse
    -- ** Response constructor
    , enableVolumeIOResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableVolumeIO' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'evioDryRun'
--
-- * 'evioVolumeId'
data EnableVolumeIO = EnableVolumeIO'{_evioDryRun :: Maybe Bool, _evioVolumeId :: Text} deriving (Eq, Read, Show)

-- | 'EnableVolumeIO' smart constructor.
enableVolumeIO :: Text -> EnableVolumeIO
enableVolumeIO pVolumeId = EnableVolumeIO'{_evioDryRun = Nothing, _evioVolumeId = pVolumeId};

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
evioDryRun :: Lens' EnableVolumeIO (Maybe Bool)
evioDryRun = lens _evioDryRun (\ s a -> s{_evioDryRun = a});

-- | The ID of the volume.
evioVolumeId :: Lens' EnableVolumeIO Text
evioVolumeId = lens _evioVolumeId (\ s a -> s{_evioVolumeId = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest EnableVolumeIO where
        type Sv EnableVolumeIO = EC2
        type Rs EnableVolumeIO = EnableVolumeIOResponse
        request = post
        response = receiveNull EnableVolumeIOResponse'

instance ToHeaders EnableVolumeIO where
        toHeaders = const mempty

instance ToPath EnableVolumeIO where
        toPath = const "/"

instance ToQuery EnableVolumeIO where
        toQuery EnableVolumeIO'{..}
          = mconcat
              ["Action" =: ("EnableVolumeIO" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _evioDryRun, "VolumeId" =: _evioVolumeId]

-- | /See:/ 'enableVolumeIOResponse' smart constructor.
data EnableVolumeIOResponse = EnableVolumeIOResponse' deriving (Eq, Read, Show)

-- | 'EnableVolumeIOResponse' smart constructor.
enableVolumeIOResponse :: EnableVolumeIOResponse
enableVolumeIOResponse = EnableVolumeIOResponse';
