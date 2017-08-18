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
-- Updates an existing fleet. All the attributes except the fleet name can be updated in the __STOPPED__ state. When a fleet is in the __RUNNING__ state, only @DisplayName@ and @ComputeCapacity@ can be updated. A fleet cannot be updated in a status of __STARTING__ or __STOPPING__ .
--
--
module Network.AWS.AppStream.UpdateFleet
    (
    -- * Creating a Request
      updateFleet
    , UpdateFleet
    -- * Request Lenses
    , ufDomainJoinInfo
    , ufDisconnectTimeoutInSeconds
    , ufMaxUserDurationInSeconds
    , ufAttributesToDelete
    , ufDeleteVPCConfig
    , ufInstanceType
    , ufVPCConfig
    , ufDisplayName
    , ufEnableDefaultInternetAccess
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
    { _ufDomainJoinInfo              :: !(Maybe DomainJoinInfo)
    , _ufDisconnectTimeoutInSeconds  :: !(Maybe Int)
    , _ufMaxUserDurationInSeconds    :: !(Maybe Int)
    , _ufAttributesToDelete          :: !(Maybe [FleetAttribute])
    , _ufDeleteVPCConfig             :: !(Maybe Bool)
    , _ufInstanceType                :: !(Maybe Text)
    , _ufVPCConfig                   :: !(Maybe VPCConfig)
    , _ufDisplayName                 :: !(Maybe Text)
    , _ufEnableDefaultInternetAccess :: !(Maybe Bool)
    , _ufImageName                   :: !(Maybe Text)
    , _ufDescription                 :: !(Maybe Text)
    , _ufComputeCapacity             :: !(Maybe ComputeCapacity)
    , _ufName                        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufDomainJoinInfo' - The /DirectoryName/ and /OrganizationalUnitDistinguishedName/ values, which are used to join domains for the AppStream 2.0 streaming instances.
--
-- * 'ufDisconnectTimeoutInSeconds' - The time after disconnection when a session is considered to have ended. If a user who got disconnected reconnects within this timeout interval, the user is connected back to their previous session. The input can be any numeric value in seconds between 60 and 57600.
--
-- * 'ufMaxUserDurationInSeconds' - The maximum time for which a streaming session can run. The input can be any numeric value in seconds between 600 and 57600.
--
-- * 'ufAttributesToDelete' - Fleet attributes to be deleted.
--
-- * 'ufDeleteVPCConfig' - Delete the VPC association for the specified fleet.
--
-- * 'ufInstanceType' - The instance type of compute resources for the fleet. Fleet instances are launched from this instance type. Available instance types are:     * stream.standard.medium     * stream.standard.large     * stream.compute.large     * stream.compute.xlarge     * stream.compute.2xlarge     * stream.compute.4xlarge     * stream.compute.8xlarge     * stream.memory.large     * stream.memory.xlarge     * stream.memory.2xlarge     * stream.memory.4xlarge     * stream.memory.8xlarge
--
-- * 'ufVPCConfig' - The VPC configuration for the fleet.
--
-- * 'ufDisplayName' - The name displayed to end users on the AppStream 2.0 portal.
--
-- * 'ufEnableDefaultInternetAccess' - Enables or disables default internet access for the fleet.
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
    { _ufDomainJoinInfo = Nothing
    , _ufDisconnectTimeoutInSeconds = Nothing
    , _ufMaxUserDurationInSeconds = Nothing
    , _ufAttributesToDelete = Nothing
    , _ufDeleteVPCConfig = Nothing
    , _ufInstanceType = Nothing
    , _ufVPCConfig = Nothing
    , _ufDisplayName = Nothing
    , _ufEnableDefaultInternetAccess = Nothing
    , _ufImageName = Nothing
    , _ufDescription = Nothing
    , _ufComputeCapacity = Nothing
    , _ufName = pName_
    }

-- | The /DirectoryName/ and /OrganizationalUnitDistinguishedName/ values, which are used to join domains for the AppStream 2.0 streaming instances.
ufDomainJoinInfo :: Lens' UpdateFleet (Maybe DomainJoinInfo)
ufDomainJoinInfo = lens _ufDomainJoinInfo (\ s a -> s{_ufDomainJoinInfo = a});

-- | The time after disconnection when a session is considered to have ended. If a user who got disconnected reconnects within this timeout interval, the user is connected back to their previous session. The input can be any numeric value in seconds between 60 and 57600.
ufDisconnectTimeoutInSeconds :: Lens' UpdateFleet (Maybe Int)
ufDisconnectTimeoutInSeconds = lens _ufDisconnectTimeoutInSeconds (\ s a -> s{_ufDisconnectTimeoutInSeconds = a});

-- | The maximum time for which a streaming session can run. The input can be any numeric value in seconds between 600 and 57600.
ufMaxUserDurationInSeconds :: Lens' UpdateFleet (Maybe Int)
ufMaxUserDurationInSeconds = lens _ufMaxUserDurationInSeconds (\ s a -> s{_ufMaxUserDurationInSeconds = a});

-- | Fleet attributes to be deleted.
ufAttributesToDelete :: Lens' UpdateFleet [FleetAttribute]
ufAttributesToDelete = lens _ufAttributesToDelete (\ s a -> s{_ufAttributesToDelete = a}) . _Default . _Coerce;

-- | Delete the VPC association for the specified fleet.
ufDeleteVPCConfig :: Lens' UpdateFleet (Maybe Bool)
ufDeleteVPCConfig = lens _ufDeleteVPCConfig (\ s a -> s{_ufDeleteVPCConfig = a});

-- | The instance type of compute resources for the fleet. Fleet instances are launched from this instance type. Available instance types are:     * stream.standard.medium     * stream.standard.large     * stream.compute.large     * stream.compute.xlarge     * stream.compute.2xlarge     * stream.compute.4xlarge     * stream.compute.8xlarge     * stream.memory.large     * stream.memory.xlarge     * stream.memory.2xlarge     * stream.memory.4xlarge     * stream.memory.8xlarge
ufInstanceType :: Lens' UpdateFleet (Maybe Text)
ufInstanceType = lens _ufInstanceType (\ s a -> s{_ufInstanceType = a});

-- | The VPC configuration for the fleet.
ufVPCConfig :: Lens' UpdateFleet (Maybe VPCConfig)
ufVPCConfig = lens _ufVPCConfig (\ s a -> s{_ufVPCConfig = a});

-- | The name displayed to end users on the AppStream 2.0 portal.
ufDisplayName :: Lens' UpdateFleet (Maybe Text)
ufDisplayName = lens _ufDisplayName (\ s a -> s{_ufDisplayName = a});

-- | Enables or disables default internet access for the fleet.
ufEnableDefaultInternetAccess :: Lens' UpdateFleet (Maybe Bool)
ufEnableDefaultInternetAccess = lens _ufEnableDefaultInternetAccess (\ s a -> s{_ufEnableDefaultInternetAccess = a});

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
                 [("DomainJoinInfo" .=) <$> _ufDomainJoinInfo,
                  ("DisconnectTimeoutInSeconds" .=) <$>
                    _ufDisconnectTimeoutInSeconds,
                  ("MaxUserDurationInSeconds" .=) <$>
                    _ufMaxUserDurationInSeconds,
                  ("AttributesToDelete" .=) <$> _ufAttributesToDelete,
                  ("DeleteVpcConfig" .=) <$> _ufDeleteVPCConfig,
                  ("InstanceType" .=) <$> _ufInstanceType,
                  ("VpcConfig" .=) <$> _ufVPCConfig,
                  ("DisplayName" .=) <$> _ufDisplayName,
                  ("EnableDefaultInternetAccess" .=) <$>
                    _ufEnableDefaultInternetAccess,
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
