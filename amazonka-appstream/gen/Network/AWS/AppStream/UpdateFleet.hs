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
-- Module      : Network.AWS.AppStream.UpdateFleet
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing fleet. All the attributes except the fleet name can be updated in the __STOPPED__ state. Only __ComputeCapacity__ and __ImageName__ can be updated in any other state.
--
--
module Network.AWS.AppStream.UpdateFleet
    (
    -- * Creating a Request
      updateFleet
    , UpdateFleet
    -- * Request Lenses
    , ufDisconnectTimeoutInSeconds
    , ufMaxUserDurationInSeconds
    , ufDeleteVPCConfig
    , ufInstanceType
    , ufVPCConfig
    , ufDisplayName
    , ufImageName
    , ufDescription
    , ufComputeCapacity
    , ufName

    -- * Destructuring the Response
    , updateFleetResponse
    , UpdateFleetResponse
    -- * Response Lenses
    , ufrsFleet
    , ufrsResponseStatus
    ) where

import           Network.AWS.AppStream.Types
import           Network.AWS.AppStream.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateFleet' smart constructor.
data UpdateFleet = UpdateFleet'
    { _ufDisconnectTimeoutInSeconds :: !(Maybe Int)
    , _ufMaxUserDurationInSeconds   :: !(Maybe Int)
    , _ufDeleteVPCConfig            :: !(Maybe Bool)
    , _ufInstanceType               :: !(Maybe Text)
    , _ufVPCConfig                  :: !(Maybe VPCConfig)
    , _ufDisplayName                :: !(Maybe Text)
    , _ufImageName                  :: !(Maybe Text)
    , _ufDescription                :: !(Maybe Text)
    , _ufComputeCapacity            :: !(Maybe ComputeCapacity)
    , _ufName                       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufDisconnectTimeoutInSeconds' - The time after disconnection when a session is considered to have ended. When the user reconnects after a disconnection, the user is connected to the same instance within this time interval.
--
-- * 'ufMaxUserDurationInSeconds' - The maximum time during which a streaming session can run.
--
-- * 'ufDeleteVPCConfig' - Delete the VPC association for the specified fleet.
--
-- * 'ufInstanceType' - The instance type of compute resources for the fleet. Fleet instances are launched from this instance type.
--
-- * 'ufVPCConfig' - The VPC configuration for the fleet.
--
-- * 'ufDisplayName' - The name displayed to end users on the AppStream 2.0 portal.
--
-- * 'ufImageName' - The image name from which a fleet is created.
--
-- * 'ufDescription' - The description displayed to end users on the AppStream 2.0 portal.
--
-- * 'ufComputeCapacity' - The parameters for the capacity allocated to the fleet.
--
-- * 'ufName' - The name of the fleet.
updateFleet
    :: Text -- ^ 'ufName'
    -> UpdateFleet
updateFleet pName_ =
    UpdateFleet'
    { _ufDisconnectTimeoutInSeconds = Nothing
    , _ufMaxUserDurationInSeconds = Nothing
    , _ufDeleteVPCConfig = Nothing
    , _ufInstanceType = Nothing
    , _ufVPCConfig = Nothing
    , _ufDisplayName = Nothing
    , _ufImageName = Nothing
    , _ufDescription = Nothing
    , _ufComputeCapacity = Nothing
    , _ufName = pName_
    }

-- | The time after disconnection when a session is considered to have ended. When the user reconnects after a disconnection, the user is connected to the same instance within this time interval.
ufDisconnectTimeoutInSeconds :: Lens' UpdateFleet (Maybe Int)
ufDisconnectTimeoutInSeconds = lens _ufDisconnectTimeoutInSeconds (\ s a -> s{_ufDisconnectTimeoutInSeconds = a});

-- | The maximum time during which a streaming session can run.
ufMaxUserDurationInSeconds :: Lens' UpdateFleet (Maybe Int)
ufMaxUserDurationInSeconds = lens _ufMaxUserDurationInSeconds (\ s a -> s{_ufMaxUserDurationInSeconds = a});

-- | Delete the VPC association for the specified fleet.
ufDeleteVPCConfig :: Lens' UpdateFleet (Maybe Bool)
ufDeleteVPCConfig = lens _ufDeleteVPCConfig (\ s a -> s{_ufDeleteVPCConfig = a});

-- | The instance type of compute resources for the fleet. Fleet instances are launched from this instance type.
ufInstanceType :: Lens' UpdateFleet (Maybe Text)
ufInstanceType = lens _ufInstanceType (\ s a -> s{_ufInstanceType = a});

-- | The VPC configuration for the fleet.
ufVPCConfig :: Lens' UpdateFleet (Maybe VPCConfig)
ufVPCConfig = lens _ufVPCConfig (\ s a -> s{_ufVPCConfig = a});

-- | The name displayed to end users on the AppStream 2.0 portal.
ufDisplayName :: Lens' UpdateFleet (Maybe Text)
ufDisplayName = lens _ufDisplayName (\ s a -> s{_ufDisplayName = a});

-- | The image name from which a fleet is created.
ufImageName :: Lens' UpdateFleet (Maybe Text)
ufImageName = lens _ufImageName (\ s a -> s{_ufImageName = a});

-- | The description displayed to end users on the AppStream 2.0 portal.
ufDescription :: Lens' UpdateFleet (Maybe Text)
ufDescription = lens _ufDescription (\ s a -> s{_ufDescription = a});

-- | The parameters for the capacity allocated to the fleet.
ufComputeCapacity :: Lens' UpdateFleet (Maybe ComputeCapacity)
ufComputeCapacity = lens _ufComputeCapacity (\ s a -> s{_ufComputeCapacity = a});

-- | The name of the fleet.
ufName :: Lens' UpdateFleet Text
ufName = lens _ufName (\ s a -> s{_ufName = a});

instance AWSRequest UpdateFleet where
        type Rs UpdateFleet = UpdateFleetResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 UpdateFleetResponse' <$>
                   (x .?> "Fleet") <*> (pure (fromEnum s)))

instance Hashable UpdateFleet

instance NFData UpdateFleet

instance ToHeaders UpdateFleet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.UpdateFleet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateFleet where
        toJSON UpdateFleet'{..}
          = object
              (catMaybes
                 [("DisconnectTimeoutInSeconds" .=) <$>
                    _ufDisconnectTimeoutInSeconds,
                  ("MaxUserDurationInSeconds" .=) <$>
                    _ufMaxUserDurationInSeconds,
                  ("DeleteVpcConfig" .=) <$> _ufDeleteVPCConfig,
                  ("InstanceType" .=) <$> _ufInstanceType,
                  ("VpcConfig" .=) <$> _ufVPCConfig,
                  ("DisplayName" .=) <$> _ufDisplayName,
                  ("ImageName" .=) <$> _ufImageName,
                  ("Description" .=) <$> _ufDescription,
                  ("ComputeCapacity" .=) <$> _ufComputeCapacity,
                  Just ("Name" .= _ufName)])

instance ToPath UpdateFleet where
        toPath = const "/"

instance ToQuery UpdateFleet where
        toQuery = const mempty

-- | /See:/ 'updateFleetResponse' smart constructor.
data UpdateFleetResponse = UpdateFleetResponse'
    { _ufrsFleet          :: !(Maybe Fleet)
    , _ufrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateFleetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufrsFleet' - A list of fleet details.
--
-- * 'ufrsResponseStatus' - -- | The response status code.
updateFleetResponse
    :: Int -- ^ 'ufrsResponseStatus'
    -> UpdateFleetResponse
updateFleetResponse pResponseStatus_ =
    UpdateFleetResponse'
    { _ufrsFleet = Nothing
    , _ufrsResponseStatus = pResponseStatus_
    }

-- | A list of fleet details.
ufrsFleet :: Lens' UpdateFleetResponse (Maybe Fleet)
ufrsFleet = lens _ufrsFleet (\ s a -> s{_ufrsFleet = a});

-- | -- | The response status code.
ufrsResponseStatus :: Lens' UpdateFleetResponse Int
ufrsResponseStatus = lens _ufrsResponseStatus (\ s a -> s{_ufrsResponseStatus = a});

instance NFData UpdateFleetResponse
