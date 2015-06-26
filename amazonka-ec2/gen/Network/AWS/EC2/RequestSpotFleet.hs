{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.RequestSpotFleet
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

-- | Creates a Spot fleet request.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet.html Spot Fleets>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RequestSpotFleet.html>
module Network.AWS.EC2.RequestSpotFleet
    (
    -- * Request
      RequestSpotFleet
    -- ** Request constructor
    , requestSpotFleet
    -- ** Request lenses
    , rsfDryRun
    , rsfSpotFleetRequestConfig

    -- * Response
    , RequestSpotFleetResponse
    -- ** Response constructor
    , requestSpotFleetResponse
    -- ** Response lenses
    , rsfrSpotFleetRequestId
    , rsfrStatusCode
    ) where

import Network.AWS.EC2.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for RequestSpotFleet.
--
-- /See:/ 'requestSpotFleet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsfDryRun'
--
-- * 'rsfSpotFleetRequestConfig'
data RequestSpotFleet = RequestSpotFleet'{_rsfDryRun :: Maybe Bool, _rsfSpotFleetRequestConfig :: SpotFleetRequestConfigData} deriving (Eq, Read, Show)

-- | 'RequestSpotFleet' smart constructor.
requestSpotFleet :: SpotFleetRequestConfigData -> RequestSpotFleet
requestSpotFleet pSpotFleetRequestConfig = RequestSpotFleet'{_rsfDryRun = Nothing, _rsfSpotFleetRequestConfig = pSpotFleetRequestConfig};

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rsfDryRun :: Lens' RequestSpotFleet (Maybe Bool)
rsfDryRun = lens _rsfDryRun (\ s a -> s{_rsfDryRun = a});

-- | The configuration for the Spot fleet request.
rsfSpotFleetRequestConfig :: Lens' RequestSpotFleet SpotFleetRequestConfigData
rsfSpotFleetRequestConfig = lens _rsfSpotFleetRequestConfig (\ s a -> s{_rsfSpotFleetRequestConfig = a});

instance AWSRequest RequestSpotFleet where
        type Sv RequestSpotFleet = EC2
        type Rs RequestSpotFleet = RequestSpotFleetResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 RequestSpotFleetResponse' <$>
                   (x .@ "spotFleetRequestId") <*> (pure (fromEnum s)))

instance ToHeaders RequestSpotFleet where
        toHeaders = const mempty

instance ToPath RequestSpotFleet where
        toPath = const "/"

instance ToQuery RequestSpotFleet where
        toQuery RequestSpotFleet'{..}
          = mconcat
              ["Action" =: ("RequestSpotFleet" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _rsfDryRun,
               "SpotFleetRequestConfig" =:
                 _rsfSpotFleetRequestConfig]

-- | Contains the output of RequestSpotFleet.
--
-- /See:/ 'requestSpotFleetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsfrSpotFleetRequestId'
--
-- * 'rsfrStatusCode'
data RequestSpotFleetResponse = RequestSpotFleetResponse'{_rsfrSpotFleetRequestId :: Text, _rsfrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'RequestSpotFleetResponse' smart constructor.
requestSpotFleetResponse :: Text -> Int -> RequestSpotFleetResponse
requestSpotFleetResponse pSpotFleetRequestId pStatusCode = RequestSpotFleetResponse'{_rsfrSpotFleetRequestId = pSpotFleetRequestId, _rsfrStatusCode = pStatusCode};

-- | The ID of the Spot fleet request.
rsfrSpotFleetRequestId :: Lens' RequestSpotFleetResponse Text
rsfrSpotFleetRequestId = lens _rsfrSpotFleetRequestId (\ s a -> s{_rsfrSpotFleetRequestId = a});

-- | FIXME: Undocumented member.
rsfrStatusCode :: Lens' RequestSpotFleetResponse Int
rsfrStatusCode = lens _rsfrStatusCode (\ s a -> s{_rsfrStatusCode = a});
