{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ExportClientVPNClientConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads the contents of the Client VPN endpoint configuration file for the specified Client VPN endpoint. The Client VPN endpoint configuration file includes the Client VPN endpoint and certificate information clients need to establish a connection with the Client VPN endpoint.
module Network.AWS.EC2.ExportClientVPNClientConfiguration
  ( -- * Creating a Request
    exportClientVPNClientConfiguration,
    ExportClientVPNClientConfiguration,

    -- * Request Lenses
    ecvccDryRun,
    ecvccClientVPNEndpointId,

    -- * Destructuring the Response
    exportClientVPNClientConfigurationResponse,
    ExportClientVPNClientConfigurationResponse,

    -- * Response Lenses
    ecvccrsClientConfiguration,
    ecvccrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'exportClientVPNClientConfiguration' smart constructor.
data ExportClientVPNClientConfiguration = ExportClientVPNClientConfiguration'
  { _ecvccDryRun ::
      !(Maybe Bool),
    _ecvccClientVPNEndpointId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportClientVPNClientConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecvccDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ecvccClientVPNEndpointId' - The ID of the Client VPN endpoint.
exportClientVPNClientConfiguration ::
  -- | 'ecvccClientVPNEndpointId'
  Text ->
  ExportClientVPNClientConfiguration
exportClientVPNClientConfiguration pClientVPNEndpointId_ =
  ExportClientVPNClientConfiguration'
    { _ecvccDryRun = Nothing,
      _ecvccClientVPNEndpointId = pClientVPNEndpointId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ecvccDryRun :: Lens' ExportClientVPNClientConfiguration (Maybe Bool)
ecvccDryRun = lens _ecvccDryRun (\s a -> s {_ecvccDryRun = a})

-- | The ID of the Client VPN endpoint.
ecvccClientVPNEndpointId :: Lens' ExportClientVPNClientConfiguration Text
ecvccClientVPNEndpointId = lens _ecvccClientVPNEndpointId (\s a -> s {_ecvccClientVPNEndpointId = a})

instance AWSRequest ExportClientVPNClientConfiguration where
  type
    Rs ExportClientVPNClientConfiguration =
      ExportClientVPNClientConfigurationResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          ExportClientVPNClientConfigurationResponse'
            <$> (x .@? "clientConfiguration") <*> (pure (fromEnum s))
      )

instance Hashable ExportClientVPNClientConfiguration

instance NFData ExportClientVPNClientConfiguration

instance ToHeaders ExportClientVPNClientConfiguration where
  toHeaders = const mempty

instance ToPath ExportClientVPNClientConfiguration where
  toPath = const "/"

instance ToQuery ExportClientVPNClientConfiguration where
  toQuery ExportClientVPNClientConfiguration' {..} =
    mconcat
      [ "Action" =: ("ExportClientVpnClientConfiguration" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _ecvccDryRun,
        "ClientVpnEndpointId" =: _ecvccClientVPNEndpointId
      ]

-- | /See:/ 'exportClientVPNClientConfigurationResponse' smart constructor.
data ExportClientVPNClientConfigurationResponse = ExportClientVPNClientConfigurationResponse'
  { _ecvccrsClientConfiguration ::
      !( Maybe
           Text
       ),
    _ecvccrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ExportClientVPNClientConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecvccrsClientConfiguration' - The contents of the Client VPN endpoint configuration file.
--
-- * 'ecvccrsResponseStatus' - -- | The response status code.
exportClientVPNClientConfigurationResponse ::
  -- | 'ecvccrsResponseStatus'
  Int ->
  ExportClientVPNClientConfigurationResponse
exportClientVPNClientConfigurationResponse pResponseStatus_ =
  ExportClientVPNClientConfigurationResponse'
    { _ecvccrsClientConfiguration =
        Nothing,
      _ecvccrsResponseStatus = pResponseStatus_
    }

-- | The contents of the Client VPN endpoint configuration file.
ecvccrsClientConfiguration :: Lens' ExportClientVPNClientConfigurationResponse (Maybe Text)
ecvccrsClientConfiguration = lens _ecvccrsClientConfiguration (\s a -> s {_ecvccrsClientConfiguration = a})

-- | -- | The response status code.
ecvccrsResponseStatus :: Lens' ExportClientVPNClientConfigurationResponse Int
ecvccrsResponseStatus = lens _ecvccrsResponseStatus (\s a -> s {_ecvccrsResponseStatus = a})

instance NFData ExportClientVPNClientConfigurationResponse
