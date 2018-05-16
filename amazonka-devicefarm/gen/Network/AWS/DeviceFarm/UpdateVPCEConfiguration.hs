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
-- Module      : Network.AWS.DeviceFarm.UpdateVPCEConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about an existing Amazon Virtual Private Cloud (VPC) endpoint configuration.
--
--
module Network.AWS.DeviceFarm.UpdateVPCEConfiguration
    (
    -- * Creating a Request
      updateVPCEConfiguration
    , UpdateVPCEConfiguration
    -- * Request Lenses
    , uvecVpceServiceName
    , uvecVpceConfigurationName
    , uvecServiceDNSName
    , uvecVpceConfigurationDescription
    , uvecArn

    -- * Destructuring the Response
    , updateVPCEConfigurationResponse
    , UpdateVPCEConfigurationResponse
    -- * Response Lenses
    , uvecrsVpceConfiguration
    , uvecrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateVPCEConfiguration' smart constructor.
data UpdateVPCEConfiguration = UpdateVPCEConfiguration'
  { _uvecVpceServiceName              :: !(Maybe Text)
  , _uvecVpceConfigurationName        :: !(Maybe Text)
  , _uvecServiceDNSName               :: !(Maybe Text)
  , _uvecVpceConfigurationDescription :: !(Maybe Text)
  , _uvecArn                          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateVPCEConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uvecVpceServiceName' - The name of the VPC endpoint service running inside your AWS account that you want Device Farm to test.
--
-- * 'uvecVpceConfigurationName' - The friendly name you give to your VPC endpoint configuration, to manage your configurations more easily.
--
-- * 'uvecServiceDNSName' - The DNS (domain) name used to connect to your private service in your Amazon VPC. The DNS name must not already be in use on the Internet.
--
-- * 'uvecVpceConfigurationDescription' - An optional description, providing more details about your VPC endpoint configuration.
--
-- * 'uvecArn' - The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to update.
updateVPCEConfiguration
    :: Text -- ^ 'uvecArn'
    -> UpdateVPCEConfiguration
updateVPCEConfiguration pArn_ =
  UpdateVPCEConfiguration'
    { _uvecVpceServiceName = Nothing
    , _uvecVpceConfigurationName = Nothing
    , _uvecServiceDNSName = Nothing
    , _uvecVpceConfigurationDescription = Nothing
    , _uvecArn = pArn_
    }


-- | The name of the VPC endpoint service running inside your AWS account that you want Device Farm to test.
uvecVpceServiceName :: Lens' UpdateVPCEConfiguration (Maybe Text)
uvecVpceServiceName = lens _uvecVpceServiceName (\ s a -> s{_uvecVpceServiceName = a})

-- | The friendly name you give to your VPC endpoint configuration, to manage your configurations more easily.
uvecVpceConfigurationName :: Lens' UpdateVPCEConfiguration (Maybe Text)
uvecVpceConfigurationName = lens _uvecVpceConfigurationName (\ s a -> s{_uvecVpceConfigurationName = a})

-- | The DNS (domain) name used to connect to your private service in your Amazon VPC. The DNS name must not already be in use on the Internet.
uvecServiceDNSName :: Lens' UpdateVPCEConfiguration (Maybe Text)
uvecServiceDNSName = lens _uvecServiceDNSName (\ s a -> s{_uvecServiceDNSName = a})

-- | An optional description, providing more details about your VPC endpoint configuration.
uvecVpceConfigurationDescription :: Lens' UpdateVPCEConfiguration (Maybe Text)
uvecVpceConfigurationDescription = lens _uvecVpceConfigurationDescription (\ s a -> s{_uvecVpceConfigurationDescription = a})

-- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to update.
uvecArn :: Lens' UpdateVPCEConfiguration Text
uvecArn = lens _uvecArn (\ s a -> s{_uvecArn = a})

instance AWSRequest UpdateVPCEConfiguration where
        type Rs UpdateVPCEConfiguration =
             UpdateVPCEConfigurationResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 UpdateVPCEConfigurationResponse' <$>
                   (x .?> "vpceConfiguration") <*> (pure (fromEnum s)))

instance Hashable UpdateVPCEConfiguration where

instance NFData UpdateVPCEConfiguration where

instance ToHeaders UpdateVPCEConfiguration where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.UpdateVPCEConfiguration" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateVPCEConfiguration where
        toJSON UpdateVPCEConfiguration'{..}
          = object
              (catMaybes
                 [("vpceServiceName" .=) <$> _uvecVpceServiceName,
                  ("vpceConfigurationName" .=) <$>
                    _uvecVpceConfigurationName,
                  ("serviceDnsName" .=) <$> _uvecServiceDNSName,
                  ("vpceConfigurationDescription" .=) <$>
                    _uvecVpceConfigurationDescription,
                  Just ("arn" .= _uvecArn)])

instance ToPath UpdateVPCEConfiguration where
        toPath = const "/"

instance ToQuery UpdateVPCEConfiguration where
        toQuery = const mempty

-- | /See:/ 'updateVPCEConfigurationResponse' smart constructor.
data UpdateVPCEConfigurationResponse = UpdateVPCEConfigurationResponse'
  { _uvecrsVpceConfiguration :: !(Maybe VPCEConfiguration)
  , _uvecrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateVPCEConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uvecrsVpceConfiguration' - An object containing information about your VPC endpoint configuration.
--
-- * 'uvecrsResponseStatus' - -- | The response status code.
updateVPCEConfigurationResponse
    :: Int -- ^ 'uvecrsResponseStatus'
    -> UpdateVPCEConfigurationResponse
updateVPCEConfigurationResponse pResponseStatus_ =
  UpdateVPCEConfigurationResponse'
    { _uvecrsVpceConfiguration = Nothing
    , _uvecrsResponseStatus = pResponseStatus_
    }


-- | An object containing information about your VPC endpoint configuration.
uvecrsVpceConfiguration :: Lens' UpdateVPCEConfigurationResponse (Maybe VPCEConfiguration)
uvecrsVpceConfiguration = lens _uvecrsVpceConfiguration (\ s a -> s{_uvecrsVpceConfiguration = a})

-- | -- | The response status code.
uvecrsResponseStatus :: Lens' UpdateVPCEConfigurationResponse Int
uvecrsResponseStatus = lens _uvecrsResponseStatus (\ s a -> s{_uvecrsResponseStatus = a})

instance NFData UpdateVPCEConfigurationResponse where
