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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified fleet.
--
--
-- If the fleet is in the @STOPPED@ state, you can update any attribute except the fleet name. If the fleet is in the @RUNNING@ state, you can update the @DisplayName@ and @ComputeCapacity@ attributes. If the fleet is in the @STARTING@ or @STOPPING@ state, you can't update it.
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

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufDomainJoinInfo' - The information needed to join a Microsoft Active Directory domain.
--
-- * 'ufDisconnectTimeoutInSeconds' - The time after disconnection when a session is considered to have ended, in seconds. If a user who was disconnected reconnects within this time interval, the user is connected to their previous session. Specify a value between 60 and 57600.
--
-- * 'ufMaxUserDurationInSeconds' - The maximum time that a streaming session can run, in seconds. Specify a value between 600 and 57600.
--
-- * 'ufAttributesToDelete' - The fleet attributes to delete.
--
-- * 'ufDeleteVPCConfig' - Deletes the VPC association for the specified fleet.
--
-- * 'ufInstanceType' - The instance type to use when launching fleet instances. The following instance types are available:     * stream.standard.medium     * stream.standard.large     * stream.compute.large     * stream.compute.xlarge     * stream.compute.2xlarge     * stream.compute.4xlarge     * stream.compute.8xlarge     * stream.memory.large     * stream.memory.xlarge     * stream.memory.2xlarge     * stream.memory.4xlarge     * stream.memory.8xlarge     * stream.graphics-design.large     * stream.graphics-design.xlarge     * stream.graphics-design.2xlarge     * stream.graphics-design.4xlarge     * stream.graphics-desktop.2xlarge     * stream.graphics-pro.4xlarge     * stream.graphics-pro.8xlarge     * stream.graphics-pro.16xlarge
--
-- * 'ufVPCConfig' - The VPC configuration for the fleet.
--
-- * 'ufDisplayName' - The fleet name for display.
--
-- * 'ufEnableDefaultInternetAccess' - Enables or disables default internet access for the fleet.
--
-- * 'ufImageName' - The name of the image used to create the fleet.
--
-- * 'ufDescription' - The description for display.
--
-- * 'ufComputeCapacity' - The desired capacity for the fleet.
--
-- * 'ufName' - A unique name for the fleet.
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


-- | The information needed to join a Microsoft Active Directory domain.
ufDomainJoinInfo :: Lens' UpdateFleet (Maybe DomainJoinInfo)
ufDomainJoinInfo = lens _ufDomainJoinInfo (\ s a -> s{_ufDomainJoinInfo = a})

-- | The time after disconnection when a session is considered to have ended, in seconds. If a user who was disconnected reconnects within this time interval, the user is connected to their previous session. Specify a value between 60 and 57600.
ufDisconnectTimeoutInSeconds :: Lens' UpdateFleet (Maybe Int)
ufDisconnectTimeoutInSeconds = lens _ufDisconnectTimeoutInSeconds (\ s a -> s{_ufDisconnectTimeoutInSeconds = a})

-- | The maximum time that a streaming session can run, in seconds. Specify a value between 600 and 57600.
ufMaxUserDurationInSeconds :: Lens' UpdateFleet (Maybe Int)
ufMaxUserDurationInSeconds = lens _ufMaxUserDurationInSeconds (\ s a -> s{_ufMaxUserDurationInSeconds = a})

-- | The fleet attributes to delete.
ufAttributesToDelete :: Lens' UpdateFleet [FleetAttribute]
ufAttributesToDelete = lens _ufAttributesToDelete (\ s a -> s{_ufAttributesToDelete = a}) . _Default . _Coerce

-- | Deletes the VPC association for the specified fleet.
ufDeleteVPCConfig :: Lens' UpdateFleet (Maybe Bool)
ufDeleteVPCConfig = lens _ufDeleteVPCConfig (\ s a -> s{_ufDeleteVPCConfig = a})

-- | The instance type to use when launching fleet instances. The following instance types are available:     * stream.standard.medium     * stream.standard.large     * stream.compute.large     * stream.compute.xlarge     * stream.compute.2xlarge     * stream.compute.4xlarge     * stream.compute.8xlarge     * stream.memory.large     * stream.memory.xlarge     * stream.memory.2xlarge     * stream.memory.4xlarge     * stream.memory.8xlarge     * stream.graphics-design.large     * stream.graphics-design.xlarge     * stream.graphics-design.2xlarge     * stream.graphics-design.4xlarge     * stream.graphics-desktop.2xlarge     * stream.graphics-pro.4xlarge     * stream.graphics-pro.8xlarge     * stream.graphics-pro.16xlarge
ufInstanceType :: Lens' UpdateFleet (Maybe Text)
ufInstanceType = lens _ufInstanceType (\ s a -> s{_ufInstanceType = a})

-- | The VPC configuration for the fleet.
ufVPCConfig :: Lens' UpdateFleet (Maybe VPCConfig)
ufVPCConfig = lens _ufVPCConfig (\ s a -> s{_ufVPCConfig = a})

-- | The fleet name for display.
ufDisplayName :: Lens' UpdateFleet (Maybe Text)
ufDisplayName = lens _ufDisplayName (\ s a -> s{_ufDisplayName = a})

-- | Enables or disables default internet access for the fleet.
ufEnableDefaultInternetAccess :: Lens' UpdateFleet (Maybe Bool)
ufEnableDefaultInternetAccess = lens _ufEnableDefaultInternetAccess (\ s a -> s{_ufEnableDefaultInternetAccess = a})

-- | The name of the image used to create the fleet.
ufImageName :: Lens' UpdateFleet (Maybe Text)
ufImageName = lens _ufImageName (\ s a -> s{_ufImageName = a})

-- | The description for display.
ufDescription :: Lens' UpdateFleet (Maybe Text)
ufDescription = lens _ufDescription (\ s a -> s{_ufDescription = a})

-- | The desired capacity for the fleet.
ufComputeCapacity :: Lens' UpdateFleet (Maybe ComputeCapacity)
ufComputeCapacity = lens _ufComputeCapacity (\ s a -> s{_ufComputeCapacity = a})

-- | A unique name for the fleet.
ufName :: Lens' UpdateFleet Text
ufName = lens _ufName (\ s a -> s{_ufName = a})

instance AWSRequest UpdateFleet where
        type Rs UpdateFleet = UpdateFleetResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 UpdateFleetResponse' <$>
                   (x .?> "Fleet") <*> (pure (fromEnum s)))

instance Hashable UpdateFleet where

instance NFData UpdateFleet where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateFleetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufrsFleet' - Information about the fleet.
--
-- * 'ufrsResponseStatus' - -- | The response status code.
updateFleetResponse
    :: Int -- ^ 'ufrsResponseStatus'
    -> UpdateFleetResponse
updateFleetResponse pResponseStatus_ =
  UpdateFleetResponse'
    {_ufrsFleet = Nothing, _ufrsResponseStatus = pResponseStatus_}


-- | Information about the fleet.
ufrsFleet :: Lens' UpdateFleetResponse (Maybe Fleet)
ufrsFleet = lens _ufrsFleet (\ s a -> s{_ufrsFleet = a})

-- | -- | The response status code.
ufrsResponseStatus :: Lens' UpdateFleetResponse Int
ufrsResponseStatus = lens _ufrsResponseStatus (\ s a -> s{_ufrsResponseStatus = a})

instance NFData UpdateFleetResponse where
