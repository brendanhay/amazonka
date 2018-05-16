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
-- Module      : Network.AWS.AppStream.CreateFleet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a fleet.
--
--
module Network.AWS.AppStream.CreateFleet
    (
    -- * Creating a Request
      createFleet
    , CreateFleet
    -- * Request Lenses
    , cfDomainJoinInfo
    , cfDisconnectTimeoutInSeconds
    , cfMaxUserDurationInSeconds
    , cfFleetType
    , cfVPCConfig
    , cfDisplayName
    , cfEnableDefaultInternetAccess
    , cfDescription
    , cfName
    , cfImageName
    , cfInstanceType
    , cfComputeCapacity

    -- * Destructuring the Response
    , createFleetResponse
    , CreateFleetResponse
    -- * Response Lenses
    , cfrsFleet
    , cfrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createFleet' smart constructor.
data CreateFleet = CreateFleet'
  { _cfDomainJoinInfo              :: !(Maybe DomainJoinInfo)
  , _cfDisconnectTimeoutInSeconds  :: !(Maybe Int)
  , _cfMaxUserDurationInSeconds    :: !(Maybe Int)
  , _cfFleetType                   :: !(Maybe FleetType)
  , _cfVPCConfig                   :: !(Maybe VPCConfig)
  , _cfDisplayName                 :: !(Maybe Text)
  , _cfEnableDefaultInternetAccess :: !(Maybe Bool)
  , _cfDescription                 :: !(Maybe Text)
  , _cfName                        :: !Text
  , _cfImageName                   :: !Text
  , _cfInstanceType                :: !Text
  , _cfComputeCapacity             :: !ComputeCapacity
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfDomainJoinInfo' - The information needed to join a Microsoft Active Directory domain.
--
-- * 'cfDisconnectTimeoutInSeconds' - The time after disconnection when a session is considered to have ended, in seconds. If a user who was disconnected reconnects within this time interval, the user is connected to their previous session. Specify a value between 60 and 57600.
--
-- * 'cfMaxUserDurationInSeconds' - The maximum time that a streaming session can run, in seconds. Specify a value between 600 and 57600.
--
-- * 'cfFleetType' - The fleet type.     * ALWAYS_ON    * Provides users with instant-on access to their apps. You are charged for all running instances in your fleet, even if no users are streaming apps.     * ON_DEMAND    * Provide users with access to applications after they connect, which takes one to two minutes. You are charged for instance streaming when users are connected and a small hourly fee for instances that are not streaming apps.
--
-- * 'cfVPCConfig' - The VPC configuration for the fleet.
--
-- * 'cfDisplayName' - The fleet name for display.
--
-- * 'cfEnableDefaultInternetAccess' - Enables or disables default internet access for the fleet.
--
-- * 'cfDescription' - The description for display.
--
-- * 'cfName' - A unique name for the fleet.
--
-- * 'cfImageName' - The name of the image used to create the fleet.
--
-- * 'cfInstanceType' - The instance type to use when launching fleet instances. The following instance types are available:     * stream.standard.medium     * stream.standard.large     * stream.compute.large     * stream.compute.xlarge     * stream.compute.2xlarge     * stream.compute.4xlarge     * stream.compute.8xlarge     * stream.memory.large     * stream.memory.xlarge     * stream.memory.2xlarge     * stream.memory.4xlarge     * stream.memory.8xlarge     * stream.graphics-design.large     * stream.graphics-design.xlarge     * stream.graphics-design.2xlarge     * stream.graphics-design.4xlarge     * stream.graphics-desktop.2xlarge     * stream.graphics-pro.4xlarge     * stream.graphics-pro.8xlarge     * stream.graphics-pro.16xlarge
--
-- * 'cfComputeCapacity' - The desired capacity for the fleet.
createFleet
    :: Text -- ^ 'cfName'
    -> Text -- ^ 'cfImageName'
    -> Text -- ^ 'cfInstanceType'
    -> ComputeCapacity -- ^ 'cfComputeCapacity'
    -> CreateFleet
createFleet pName_ pImageName_ pInstanceType_ pComputeCapacity_ =
  CreateFleet'
    { _cfDomainJoinInfo = Nothing
    , _cfDisconnectTimeoutInSeconds = Nothing
    , _cfMaxUserDurationInSeconds = Nothing
    , _cfFleetType = Nothing
    , _cfVPCConfig = Nothing
    , _cfDisplayName = Nothing
    , _cfEnableDefaultInternetAccess = Nothing
    , _cfDescription = Nothing
    , _cfName = pName_
    , _cfImageName = pImageName_
    , _cfInstanceType = pInstanceType_
    , _cfComputeCapacity = pComputeCapacity_
    }


-- | The information needed to join a Microsoft Active Directory domain.
cfDomainJoinInfo :: Lens' CreateFleet (Maybe DomainJoinInfo)
cfDomainJoinInfo = lens _cfDomainJoinInfo (\ s a -> s{_cfDomainJoinInfo = a})

-- | The time after disconnection when a session is considered to have ended, in seconds. If a user who was disconnected reconnects within this time interval, the user is connected to their previous session. Specify a value between 60 and 57600.
cfDisconnectTimeoutInSeconds :: Lens' CreateFleet (Maybe Int)
cfDisconnectTimeoutInSeconds = lens _cfDisconnectTimeoutInSeconds (\ s a -> s{_cfDisconnectTimeoutInSeconds = a})

-- | The maximum time that a streaming session can run, in seconds. Specify a value between 600 and 57600.
cfMaxUserDurationInSeconds :: Lens' CreateFleet (Maybe Int)
cfMaxUserDurationInSeconds = lens _cfMaxUserDurationInSeconds (\ s a -> s{_cfMaxUserDurationInSeconds = a})

-- | The fleet type.     * ALWAYS_ON    * Provides users with instant-on access to their apps. You are charged for all running instances in your fleet, even if no users are streaming apps.     * ON_DEMAND    * Provide users with access to applications after they connect, which takes one to two minutes. You are charged for instance streaming when users are connected and a small hourly fee for instances that are not streaming apps.
cfFleetType :: Lens' CreateFleet (Maybe FleetType)
cfFleetType = lens _cfFleetType (\ s a -> s{_cfFleetType = a})

-- | The VPC configuration for the fleet.
cfVPCConfig :: Lens' CreateFleet (Maybe VPCConfig)
cfVPCConfig = lens _cfVPCConfig (\ s a -> s{_cfVPCConfig = a})

-- | The fleet name for display.
cfDisplayName :: Lens' CreateFleet (Maybe Text)
cfDisplayName = lens _cfDisplayName (\ s a -> s{_cfDisplayName = a})

-- | Enables or disables default internet access for the fleet.
cfEnableDefaultInternetAccess :: Lens' CreateFleet (Maybe Bool)
cfEnableDefaultInternetAccess = lens _cfEnableDefaultInternetAccess (\ s a -> s{_cfEnableDefaultInternetAccess = a})

-- | The description for display.
cfDescription :: Lens' CreateFleet (Maybe Text)
cfDescription = lens _cfDescription (\ s a -> s{_cfDescription = a})

-- | A unique name for the fleet.
cfName :: Lens' CreateFleet Text
cfName = lens _cfName (\ s a -> s{_cfName = a})

-- | The name of the image used to create the fleet.
cfImageName :: Lens' CreateFleet Text
cfImageName = lens _cfImageName (\ s a -> s{_cfImageName = a})

-- | The instance type to use when launching fleet instances. The following instance types are available:     * stream.standard.medium     * stream.standard.large     * stream.compute.large     * stream.compute.xlarge     * stream.compute.2xlarge     * stream.compute.4xlarge     * stream.compute.8xlarge     * stream.memory.large     * stream.memory.xlarge     * stream.memory.2xlarge     * stream.memory.4xlarge     * stream.memory.8xlarge     * stream.graphics-design.large     * stream.graphics-design.xlarge     * stream.graphics-design.2xlarge     * stream.graphics-design.4xlarge     * stream.graphics-desktop.2xlarge     * stream.graphics-pro.4xlarge     * stream.graphics-pro.8xlarge     * stream.graphics-pro.16xlarge
cfInstanceType :: Lens' CreateFleet Text
cfInstanceType = lens _cfInstanceType (\ s a -> s{_cfInstanceType = a})

-- | The desired capacity for the fleet.
cfComputeCapacity :: Lens' CreateFleet ComputeCapacity
cfComputeCapacity = lens _cfComputeCapacity (\ s a -> s{_cfComputeCapacity = a})

instance AWSRequest CreateFleet where
        type Rs CreateFleet = CreateFleetResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 CreateFleetResponse' <$>
                   (x .?> "Fleet") <*> (pure (fromEnum s)))

instance Hashable CreateFleet where

instance NFData CreateFleet where

instance ToHeaders CreateFleet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.CreateFleet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateFleet where
        toJSON CreateFleet'{..}
          = object
              (catMaybes
                 [("DomainJoinInfo" .=) <$> _cfDomainJoinInfo,
                  ("DisconnectTimeoutInSeconds" .=) <$>
                    _cfDisconnectTimeoutInSeconds,
                  ("MaxUserDurationInSeconds" .=) <$>
                    _cfMaxUserDurationInSeconds,
                  ("FleetType" .=) <$> _cfFleetType,
                  ("VpcConfig" .=) <$> _cfVPCConfig,
                  ("DisplayName" .=) <$> _cfDisplayName,
                  ("EnableDefaultInternetAccess" .=) <$>
                    _cfEnableDefaultInternetAccess,
                  ("Description" .=) <$> _cfDescription,
                  Just ("Name" .= _cfName),
                  Just ("ImageName" .= _cfImageName),
                  Just ("InstanceType" .= _cfInstanceType),
                  Just ("ComputeCapacity" .= _cfComputeCapacity)])

instance ToPath CreateFleet where
        toPath = const "/"

instance ToQuery CreateFleet where
        toQuery = const mempty

-- | /See:/ 'createFleetResponse' smart constructor.
data CreateFleetResponse = CreateFleetResponse'
  { _cfrsFleet          :: !(Maybe Fleet)
  , _cfrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFleetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfrsFleet' - Information about the fleet.
--
-- * 'cfrsResponseStatus' - -- | The response status code.
createFleetResponse
    :: Int -- ^ 'cfrsResponseStatus'
    -> CreateFleetResponse
createFleetResponse pResponseStatus_ =
  CreateFleetResponse'
    {_cfrsFleet = Nothing, _cfrsResponseStatus = pResponseStatus_}


-- | Information about the fleet.
cfrsFleet :: Lens' CreateFleetResponse (Maybe Fleet)
cfrsFleet = lens _cfrsFleet (\ s a -> s{_cfrsFleet = a})

-- | -- | The response status code.
cfrsResponseStatus :: Lens' CreateFleetResponse Int
cfrsResponseStatus = lens _cfrsResponseStatus (\ s a -> s{_cfrsResponseStatus = a})

instance NFData CreateFleetResponse where
