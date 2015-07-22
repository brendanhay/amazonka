{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RequestSpotFleet
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a Spot fleet request.
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
    , rsfrqDryRun
    , rsfrqSpotFleetRequestConfig

    -- * Response
    , RequestSpotFleetResponse
    -- ** Response constructor
    , requestSpotFleetResponse
    -- ** Response lenses
    , rsfrsStatus
    , rsfrsSpotFleetRequestId
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for RequestSpotFleet.
--
-- /See:/ 'requestSpotFleet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsfrqDryRun'
--
-- * 'rsfrqSpotFleetRequestConfig'
data RequestSpotFleet = RequestSpotFleet'
    { _rsfrqDryRun                 :: !(Maybe Bool)
    , _rsfrqSpotFleetRequestConfig :: !SpotFleetRequestConfigData
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RequestSpotFleet' smart constructor.
requestSpotFleet :: SpotFleetRequestConfigData -> RequestSpotFleet
requestSpotFleet pSpotFleetRequestConfig_ =
    RequestSpotFleet'
    { _rsfrqDryRun = Nothing
    , _rsfrqSpotFleetRequestConfig = pSpotFleetRequestConfig_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rsfrqDryRun :: Lens' RequestSpotFleet (Maybe Bool)
rsfrqDryRun = lens _rsfrqDryRun (\ s a -> s{_rsfrqDryRun = a});

-- | The configuration for the Spot fleet request.
rsfrqSpotFleetRequestConfig :: Lens' RequestSpotFleet SpotFleetRequestConfigData
rsfrqSpotFleetRequestConfig = lens _rsfrqSpotFleetRequestConfig (\ s a -> s{_rsfrqSpotFleetRequestConfig = a});

instance AWSRequest RequestSpotFleet where
        type Sv RequestSpotFleet = EC2
        type Rs RequestSpotFleet = RequestSpotFleetResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 RequestSpotFleetResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "spotFleetRequestId"))

instance ToHeaders RequestSpotFleet where
        toHeaders = const mempty

instance ToPath RequestSpotFleet where
        toPath = const "/"

instance ToQuery RequestSpotFleet where
        toQuery RequestSpotFleet'{..}
          = mconcat
              ["Action" =: ("RequestSpotFleet" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _rsfrqDryRun,
               "SpotFleetRequestConfig" =:
                 _rsfrqSpotFleetRequestConfig]

-- | Contains the output of RequestSpotFleet.
--
-- /See:/ 'requestSpotFleetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsfrsStatus'
--
-- * 'rsfrsSpotFleetRequestId'
data RequestSpotFleetResponse = RequestSpotFleetResponse'
    { _rsfrsStatus             :: !Int
    , _rsfrsSpotFleetRequestId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RequestSpotFleetResponse' smart constructor.
requestSpotFleetResponse :: Int -> Text -> RequestSpotFleetResponse
requestSpotFleetResponse pStatus_ pSpotFleetRequestId_ =
    RequestSpotFleetResponse'
    { _rsfrsStatus = pStatus_
    , _rsfrsSpotFleetRequestId = pSpotFleetRequestId_
    }

-- | FIXME: Undocumented member.
rsfrsStatus :: Lens' RequestSpotFleetResponse Int
rsfrsStatus = lens _rsfrsStatus (\ s a -> s{_rsfrsStatus = a});

-- | The ID of the Spot fleet request.
rsfrsSpotFleetRequestId :: Lens' RequestSpotFleetResponse Text
rsfrsSpotFleetRequestId = lens _rsfrsSpotFleetRequestId (\ s a -> s{_rsfrsSpotFleetRequestId = a});
