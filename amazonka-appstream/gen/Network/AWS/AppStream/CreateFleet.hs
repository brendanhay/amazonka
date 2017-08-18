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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new fleet.
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

import           Network.AWS.AppStream.Types
import           Network.AWS.AppStream.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for the new fleet to create.
--
--
--
-- /See:/ 'createFleet' smart constructor.
data CreateFleet = CreateFleet'
    { _cfDomainJoinInfo              :: !(Maybe DomainJoinInfo)
    , _cfDisconnectTimeoutInSeconds  :: !(Maybe Int)
    , _cfMaxUserDurationInSeconds    :: !(Maybe Int)
    , _cfVPCConfig                   :: !(Maybe VPCConfig)
    , _cfDisplayName                 :: !(Maybe Text)
    , _cfEnableDefaultInternetAccess :: !(Maybe Bool)
    , _cfDescription                 :: !(Maybe Text)
    , _cfName                        :: !Text
    , _cfImageName                   :: !Text
    , _cfInstanceType                :: !Text
    , _cfComputeCapacity             :: !ComputeCapacity
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfDomainJoinInfo' - The /DirectoryName/ and /OrganizationalUnitDistinguishedName/ values, which are used to join domains for the AppStream 2.0 streaming instances.
--
-- * 'cfDisconnectTimeoutInSeconds' - The time after disconnection when a session is considered to have ended. If a user who got disconnected reconnects within this timeout interval, the user is connected back to their previous session. The input can be any numeric value in seconds between 60 and 57600.
--
-- * 'cfMaxUserDurationInSeconds' - The maximum time for which a streaming session can run. The input can be any numeric value in seconds between 600 and 57600.
--
-- * 'cfVPCConfig' - The VPC configuration for the fleet.
--
-- * 'cfDisplayName' - The display name of the fleet.
--
-- * 'cfEnableDefaultInternetAccess' - Enables or disables default internet access for the fleet.
--
-- * 'cfDescription' - The description of the fleet.
--
-- * 'cfName' - A unique identifier for the fleet.
--
-- * 'cfImageName' - Unique name of the image used by the fleet.
--
-- * 'cfInstanceType' - The instance type of compute resources for the fleet. Fleet instances are launched from this instance type. Available instance types are:     * stream.standard.medium     * stream.standard.large     * stream.compute.large     * stream.compute.xlarge     * stream.compute.2xlarge     * stream.compute.4xlarge     * stream.compute.8xlarge     * stream.memory.large     * stream.memory.xlarge     * stream.memory.2xlarge     * stream.memory.4xlarge     * stream.memory.8xlarge
--
-- * 'cfComputeCapacity' - The parameters for the capacity allocated to the fleet.
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
    , _cfVPCConfig = Nothing
    , _cfDisplayName = Nothing
    , _cfEnableDefaultInternetAccess = Nothing
    , _cfDescription = Nothing
    , _cfName = pName_
    , _cfImageName = pImageName_
    , _cfInstanceType = pInstanceType_
    , _cfComputeCapacity = pComputeCapacity_
    }

-- | The /DirectoryName/ and /OrganizationalUnitDistinguishedName/ values, which are used to join domains for the AppStream 2.0 streaming instances.
cfDomainJoinInfo :: Lens' CreateFleet (Maybe DomainJoinInfo)
cfDomainJoinInfo = lens _cfDomainJoinInfo (\ s a -> s{_cfDomainJoinInfo = a});

-- | The time after disconnection when a session is considered to have ended. If a user who got disconnected reconnects within this timeout interval, the user is connected back to their previous session. The input can be any numeric value in seconds between 60 and 57600.
cfDisconnectTimeoutInSeconds :: Lens' CreateFleet (Maybe Int)
cfDisconnectTimeoutInSeconds = lens _cfDisconnectTimeoutInSeconds (\ s a -> s{_cfDisconnectTimeoutInSeconds = a});

-- | The maximum time for which a streaming session can run. The input can be any numeric value in seconds between 600 and 57600.
cfMaxUserDurationInSeconds :: Lens' CreateFleet (Maybe Int)
cfMaxUserDurationInSeconds = lens _cfMaxUserDurationInSeconds (\ s a -> s{_cfMaxUserDurationInSeconds = a});

-- | The VPC configuration for the fleet.
cfVPCConfig :: Lens' CreateFleet (Maybe VPCConfig)
cfVPCConfig = lens _cfVPCConfig (\ s a -> s{_cfVPCConfig = a});

-- | The display name of the fleet.
cfDisplayName :: Lens' CreateFleet (Maybe Text)
cfDisplayName = lens _cfDisplayName (\ s a -> s{_cfDisplayName = a});

-- | Enables or disables default internet access for the fleet.
cfEnableDefaultInternetAccess :: Lens' CreateFleet (Maybe Bool)
cfEnableDefaultInternetAccess = lens _cfEnableDefaultInternetAccess (\ s a -> s{_cfEnableDefaultInternetAccess = a});

-- | The description of the fleet.
cfDescription :: Lens' CreateFleet (Maybe Text)
cfDescription = lens _cfDescription (\ s a -> s{_cfDescription = a});

-- | A unique identifier for the fleet.
cfName :: Lens' CreateFleet Text
cfName = lens _cfName (\ s a -> s{_cfName = a});

-- | Unique name of the image used by the fleet.
cfImageName :: Lens' CreateFleet Text
cfImageName = lens _cfImageName (\ s a -> s{_cfImageName = a});

-- | The instance type of compute resources for the fleet. Fleet instances are launched from this instance type. Available instance types are:     * stream.standard.medium     * stream.standard.large     * stream.compute.large     * stream.compute.xlarge     * stream.compute.2xlarge     * stream.compute.4xlarge     * stream.compute.8xlarge     * stream.memory.large     * stream.memory.xlarge     * stream.memory.2xlarge     * stream.memory.4xlarge     * stream.memory.8xlarge
cfInstanceType :: Lens' CreateFleet Text
cfInstanceType = lens _cfInstanceType (\ s a -> s{_cfInstanceType = a});

-- | The parameters for the capacity allocated to the fleet.
cfComputeCapacity :: Lens' CreateFleet ComputeCapacity
cfComputeCapacity = lens _cfComputeCapacity (\ s a -> s{_cfComputeCapacity = a});

instance AWSRequest CreateFleet where
        type Rs CreateFleet = CreateFleetResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 CreateFleetResponse' <$>
                   (x .?> "Fleet") <*> (pure (fromEnum s)))

instance Hashable CreateFleet

instance NFData CreateFleet

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateFleetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfrsFleet' - The details for the created fleet.
--
-- * 'cfrsResponseStatus' - -- | The response status code.
createFleetResponse
    :: Int -- ^ 'cfrsResponseStatus'
    -> CreateFleetResponse
createFleetResponse pResponseStatus_ =
    CreateFleetResponse'
    { _cfrsFleet = Nothing
    , _cfrsResponseStatus = pResponseStatus_
    }

-- | The details for the created fleet.
cfrsFleet :: Lens' CreateFleetResponse (Maybe Fleet)
cfrsFleet = lens _cfrsFleet (\ s a -> s{_cfrsFleet = a});

-- | -- | The response status code.
cfrsResponseStatus :: Lens' CreateFleetResponse Int
cfrsResponseStatus = lens _cfrsResponseStatus (\ s a -> s{_cfrsResponseStatus = a});

instance NFData CreateFleetResponse
