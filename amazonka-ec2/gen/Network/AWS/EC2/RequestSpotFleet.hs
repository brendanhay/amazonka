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
-- Module      : Network.AWS.EC2.RequestSpotFleet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Spot Fleet request.
--
--
-- The Spot Fleet request specifies the total target capacity and the On-Demand target capacity. Amazon EC2 calculates the difference between the total capacity and On-Demand capacity, and launches the difference as Spot capacity.
--
-- You can submit a single request that includes multiple launch specifications that vary by instance type, AMI, Availability Zone, or subnet.
--
-- By default, the Spot Fleet requests Spot Instances in the Spot pool where the price per unit is the lowest. Each launch specification can include its own instance weighting that reflects the value of the instance type to your application workload.
--
-- Alternatively, you can specify that the Spot Fleet distribute the target capacity across the Spot pools included in its launch specifications. By ensuring that the Spot Instances in your Spot Fleet are in different Spot pools, you can improve the availability of your fleet.
--
-- You can specify tags for the Spot Instances. You cannot tag other resource types in a Spot Fleet request because only the @instance@ resource type is supported.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-requests.html Spot Fleet Requests> in the /Amazon EC2 User Guide for Linux Instances/ .
--
module Network.AWS.EC2.RequestSpotFleet
    (
    -- * Creating a Request
      requestSpotFleet
    , RequestSpotFleet
    -- * Request Lenses
    , rsfDryRun
    , rsfSpotFleetRequestConfig

    -- * Destructuring the Response
    , requestSpotFleetResponse
    , RequestSpotFleetResponse
    -- * Response Lenses
    , rsfrsResponseStatus
    , rsfrsSpotFleetRequestId
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for RequestSpotFleet.
--
--
--
-- /See:/ 'requestSpotFleet' smart constructor.
data RequestSpotFleet = RequestSpotFleet'
  { _rsfDryRun                 :: !(Maybe Bool)
  , _rsfSpotFleetRequestConfig :: !SpotFleetRequestConfigData
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RequestSpotFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsfDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'rsfSpotFleetRequestConfig' - The configuration for the Spot Fleet request.
requestSpotFleet
    :: SpotFleetRequestConfigData -- ^ 'rsfSpotFleetRequestConfig'
    -> RequestSpotFleet
requestSpotFleet pSpotFleetRequestConfig_ =
  RequestSpotFleet'
    { _rsfDryRun = Nothing
    , _rsfSpotFleetRequestConfig = pSpotFleetRequestConfig_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rsfDryRun :: Lens' RequestSpotFleet (Maybe Bool)
rsfDryRun = lens _rsfDryRun (\ s a -> s{_rsfDryRun = a})

-- | The configuration for the Spot Fleet request.
rsfSpotFleetRequestConfig :: Lens' RequestSpotFleet SpotFleetRequestConfigData
rsfSpotFleetRequestConfig = lens _rsfSpotFleetRequestConfig (\ s a -> s{_rsfSpotFleetRequestConfig = a})

instance AWSRequest RequestSpotFleet where
        type Rs RequestSpotFleet = RequestSpotFleetResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 RequestSpotFleetResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "spotFleetRequestId"))

instance Hashable RequestSpotFleet where

instance NFData RequestSpotFleet where

instance ToHeaders RequestSpotFleet where
        toHeaders = const mempty

instance ToPath RequestSpotFleet where
        toPath = const "/"

instance ToQuery RequestSpotFleet where
        toQuery RequestSpotFleet'{..}
          = mconcat
              ["Action" =: ("RequestSpotFleet" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _rsfDryRun,
               "SpotFleetRequestConfig" =:
                 _rsfSpotFleetRequestConfig]

-- | Contains the output of RequestSpotFleet.
--
--
--
-- /See:/ 'requestSpotFleetResponse' smart constructor.
data RequestSpotFleetResponse = RequestSpotFleetResponse'
  { _rsfrsResponseStatus     :: !Int
  , _rsfrsSpotFleetRequestId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RequestSpotFleetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsfrsResponseStatus' - -- | The response status code.
--
-- * 'rsfrsSpotFleetRequestId' - The ID of the Spot Fleet request.
requestSpotFleetResponse
    :: Int -- ^ 'rsfrsResponseStatus'
    -> Text -- ^ 'rsfrsSpotFleetRequestId'
    -> RequestSpotFleetResponse
requestSpotFleetResponse pResponseStatus_ pSpotFleetRequestId_ =
  RequestSpotFleetResponse'
    { _rsfrsResponseStatus = pResponseStatus_
    , _rsfrsSpotFleetRequestId = pSpotFleetRequestId_
    }


-- | -- | The response status code.
rsfrsResponseStatus :: Lens' RequestSpotFleetResponse Int
rsfrsResponseStatus = lens _rsfrsResponseStatus (\ s a -> s{_rsfrsResponseStatus = a})

-- | The ID of the Spot Fleet request.
rsfrsSpotFleetRequestId :: Lens' RequestSpotFleetResponse Text
rsfrsSpotFleetRequestId = lens _rsfrsSpotFleetRequestId (\ s a -> s{_rsfrsSpotFleetRequestId = a})

instance NFData RequestSpotFleetResponse where
