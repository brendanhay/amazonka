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
-- Module      : Network.AWS.DeviceFarm.CreateVPCEConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration record in Device Farm for your Amazon Virtual Private Cloud (VPC) endpoint.
--
--
module Network.AWS.DeviceFarm.CreateVPCEConfiguration
    (
    -- * Creating a Request
      createVPCEConfiguration
    , CreateVPCEConfiguration
    -- * Request Lenses
    , cvecVpceConfigurationDescription
    , cvecVpceConfigurationName
    , cvecVpceServiceName
    , cvecServiceDNSName

    -- * Destructuring the Response
    , createVPCEConfigurationResponse
    , CreateVPCEConfigurationResponse
    -- * Response Lenses
    , cvecrsVpceConfiguration
    , cvecrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createVPCEConfiguration' smart constructor.
data CreateVPCEConfiguration = CreateVPCEConfiguration'
  { _cvecVpceConfigurationDescription :: !(Maybe Text)
  , _cvecVpceConfigurationName        :: !Text
  , _cvecVpceServiceName              :: !Text
  , _cvecServiceDNSName               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVPCEConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvecVpceConfigurationDescription' - An optional description, providing more details about your VPC endpoint configuration.
--
-- * 'cvecVpceConfigurationName' - The friendly name you give to your VPC endpoint configuration, to manage your configurations more easily.
--
-- * 'cvecVpceServiceName' - The name of the VPC endpoint service running inside your AWS account that you want Device Farm to test.
--
-- * 'cvecServiceDNSName' - The DNS name of the service running in your VPC that you want Device Farm to test.
createVPCEConfiguration
    :: Text -- ^ 'cvecVpceConfigurationName'
    -> Text -- ^ 'cvecVpceServiceName'
    -> Text -- ^ 'cvecServiceDNSName'
    -> CreateVPCEConfiguration
createVPCEConfiguration pVpceConfigurationName_ pVpceServiceName_ pServiceDNSName_ =
  CreateVPCEConfiguration'
    { _cvecVpceConfigurationDescription = Nothing
    , _cvecVpceConfigurationName = pVpceConfigurationName_
    , _cvecVpceServiceName = pVpceServiceName_
    , _cvecServiceDNSName = pServiceDNSName_
    }


-- | An optional description, providing more details about your VPC endpoint configuration.
cvecVpceConfigurationDescription :: Lens' CreateVPCEConfiguration (Maybe Text)
cvecVpceConfigurationDescription = lens _cvecVpceConfigurationDescription (\ s a -> s{_cvecVpceConfigurationDescription = a})

-- | The friendly name you give to your VPC endpoint configuration, to manage your configurations more easily.
cvecVpceConfigurationName :: Lens' CreateVPCEConfiguration Text
cvecVpceConfigurationName = lens _cvecVpceConfigurationName (\ s a -> s{_cvecVpceConfigurationName = a})

-- | The name of the VPC endpoint service running inside your AWS account that you want Device Farm to test.
cvecVpceServiceName :: Lens' CreateVPCEConfiguration Text
cvecVpceServiceName = lens _cvecVpceServiceName (\ s a -> s{_cvecVpceServiceName = a})

-- | The DNS name of the service running in your VPC that you want Device Farm to test.
cvecServiceDNSName :: Lens' CreateVPCEConfiguration Text
cvecServiceDNSName = lens _cvecServiceDNSName (\ s a -> s{_cvecServiceDNSName = a})

instance AWSRequest CreateVPCEConfiguration where
        type Rs CreateVPCEConfiguration =
             CreateVPCEConfigurationResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 CreateVPCEConfigurationResponse' <$>
                   (x .?> "vpceConfiguration") <*> (pure (fromEnum s)))

instance Hashable CreateVPCEConfiguration where

instance NFData CreateVPCEConfiguration where

instance ToHeaders CreateVPCEConfiguration where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.CreateVPCEConfiguration" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateVPCEConfiguration where
        toJSON CreateVPCEConfiguration'{..}
          = object
              (catMaybes
                 [("vpceConfigurationDescription" .=) <$>
                    _cvecVpceConfigurationDescription,
                  Just
                    ("vpceConfigurationName" .=
                       _cvecVpceConfigurationName),
                  Just ("vpceServiceName" .= _cvecVpceServiceName),
                  Just ("serviceDnsName" .= _cvecServiceDNSName)])

instance ToPath CreateVPCEConfiguration where
        toPath = const "/"

instance ToQuery CreateVPCEConfiguration where
        toQuery = const mempty

-- | /See:/ 'createVPCEConfigurationResponse' smart constructor.
data CreateVPCEConfigurationResponse = CreateVPCEConfigurationResponse'
  { _cvecrsVpceConfiguration :: !(Maybe VPCEConfiguration)
  , _cvecrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVPCEConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvecrsVpceConfiguration' - An object containing information about your VPC endpoint configuration.
--
-- * 'cvecrsResponseStatus' - -- | The response status code.
createVPCEConfigurationResponse
    :: Int -- ^ 'cvecrsResponseStatus'
    -> CreateVPCEConfigurationResponse
createVPCEConfigurationResponse pResponseStatus_ =
  CreateVPCEConfigurationResponse'
    { _cvecrsVpceConfiguration = Nothing
    , _cvecrsResponseStatus = pResponseStatus_
    }


-- | An object containing information about your VPC endpoint configuration.
cvecrsVpceConfiguration :: Lens' CreateVPCEConfigurationResponse (Maybe VPCEConfiguration)
cvecrsVpceConfiguration = lens _cvecrsVpceConfiguration (\ s a -> s{_cvecrsVpceConfiguration = a})

-- | -- | The response status code.
cvecrsResponseStatus :: Lens' CreateVPCEConfigurationResponse Int
cvecrsResponseStatus = lens _cvecrsResponseStatus (\ s a -> s{_cvecrsResponseStatus = a})

instance NFData CreateVPCEConfigurationResponse where
