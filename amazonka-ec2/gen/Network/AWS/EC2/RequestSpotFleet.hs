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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Spot fleet request.
--
-- You can submit a single request that specifies multiple instance types,
-- each with its own instance weighting that reflects its value to your
-- application workload. Amazon EC2 computes the bid price for each launch
-- specification and requests Spot Instances in the Spot pool where the
-- price per unit is the lowest. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet.html Spot Fleets>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RequestSpotFleet.html AWS API Reference> for RequestSpotFleet.
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

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for RequestSpotFleet.
--
-- /See:/ 'requestSpotFleet' smart constructor.
data RequestSpotFleet = RequestSpotFleet'
    { _rsfDryRun                 :: !(Maybe Bool)
    , _rsfSpotFleetRequestConfig :: !SpotFleetRequestConfigData
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RequestSpotFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsfDryRun'
--
-- * 'rsfSpotFleetRequestConfig'
requestSpotFleet
    :: SpotFleetRequestConfigData -- ^ 'rsfSpotFleetRequestConfig'
    -> RequestSpotFleet
requestSpotFleet pSpotFleetRequestConfig_ =
    RequestSpotFleet'
    { _rsfDryRun = Nothing
    , _rsfSpotFleetRequestConfig = pSpotFleetRequestConfig_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
rsfDryRun :: Lens' RequestSpotFleet (Maybe Bool)
rsfDryRun = lens _rsfDryRun (\ s a -> s{_rsfDryRun = a});

-- | The configuration for the Spot fleet request.
rsfSpotFleetRequestConfig :: Lens' RequestSpotFleet SpotFleetRequestConfigData
rsfSpotFleetRequestConfig = lens _rsfSpotFleetRequestConfig (\ s a -> s{_rsfSpotFleetRequestConfig = a});

instance AWSRequest RequestSpotFleet where
        type Rs RequestSpotFleet = RequestSpotFleetResponse
        request = postQuery eC2
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
               "DryRun" =: _rsfDryRun,
               "SpotFleetRequestConfig" =:
                 _rsfSpotFleetRequestConfig]

-- | Contains the output of RequestSpotFleet.
--
-- /See:/ 'requestSpotFleetResponse' smart constructor.
data RequestSpotFleetResponse = RequestSpotFleetResponse'
    { _rsfrsResponseStatus     :: !Int
    , _rsfrsSpotFleetRequestId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RequestSpotFleetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsfrsResponseStatus'
--
-- * 'rsfrsSpotFleetRequestId'
requestSpotFleetResponse
    :: Int -- ^ 'rsfrsResponseStatus'
    -> Text -- ^ 'rsfrsSpotFleetRequestId'
    -> RequestSpotFleetResponse
requestSpotFleetResponse pResponseStatus_ pSpotFleetRequestId_ =
    RequestSpotFleetResponse'
    { _rsfrsResponseStatus = pResponseStatus_
    , _rsfrsSpotFleetRequestId = pSpotFleetRequestId_
    }

-- | The response status code.
rsfrsResponseStatus :: Lens' RequestSpotFleetResponse Int
rsfrsResponseStatus = lens _rsfrsResponseStatus (\ s a -> s{_rsfrsResponseStatus = a});

-- | The ID of the Spot fleet request.
rsfrsSpotFleetRequestId :: Lens' RequestSpotFleetResponse Text
rsfrsSpotFleetRequestId = lens _rsfrsSpotFleetRequestId (\ s a -> s{_rsfrsSpotFleetRequestId = a});
