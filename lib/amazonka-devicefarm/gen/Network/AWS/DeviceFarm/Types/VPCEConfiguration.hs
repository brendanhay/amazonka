{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.VPCEConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.VPCEConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an Amazon Virtual Private Cloud (VPC) endpoint configuration.
--
--
--
-- /See:/ 'vpcEConfiguration' smart constructor.
data VPCEConfiguration = VPCEConfiguration'
  { _vecVpceServiceName ::
      !(Maybe Text),
    _vecArn :: !(Maybe Text),
    _vecVpceConfigurationName :: !(Maybe Text),
    _vecServiceDNSName :: !(Maybe Text),
    _vecVpceConfigurationDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPCEConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vecVpceServiceName' - The name of the VPC endpoint service running in your AWS account that you want Device Farm to test.
--
-- * 'vecArn' - The Amazon Resource Name (ARN) of the VPC endpoint configuration.
--
-- * 'vecVpceConfigurationName' - The friendly name you give to your VPC endpoint configuration to manage your configurations more easily.
--
-- * 'vecServiceDNSName' - The DNS name that maps to the private IP address of the service you want to access.
--
-- * 'vecVpceConfigurationDescription' - An optional description that provides details about your VPC endpoint configuration.
vpcEConfiguration ::
  VPCEConfiguration
vpcEConfiguration =
  VPCEConfiguration'
    { _vecVpceServiceName = Nothing,
      _vecArn = Nothing,
      _vecVpceConfigurationName = Nothing,
      _vecServiceDNSName = Nothing,
      _vecVpceConfigurationDescription = Nothing
    }

-- | The name of the VPC endpoint service running in your AWS account that you want Device Farm to test.
vecVpceServiceName :: Lens' VPCEConfiguration (Maybe Text)
vecVpceServiceName = lens _vecVpceServiceName (\s a -> s {_vecVpceServiceName = a})

-- | The Amazon Resource Name (ARN) of the VPC endpoint configuration.
vecArn :: Lens' VPCEConfiguration (Maybe Text)
vecArn = lens _vecArn (\s a -> s {_vecArn = a})

-- | The friendly name you give to your VPC endpoint configuration to manage your configurations more easily.
vecVpceConfigurationName :: Lens' VPCEConfiguration (Maybe Text)
vecVpceConfigurationName = lens _vecVpceConfigurationName (\s a -> s {_vecVpceConfigurationName = a})

-- | The DNS name that maps to the private IP address of the service you want to access.
vecServiceDNSName :: Lens' VPCEConfiguration (Maybe Text)
vecServiceDNSName = lens _vecServiceDNSName (\s a -> s {_vecServiceDNSName = a})

-- | An optional description that provides details about your VPC endpoint configuration.
vecVpceConfigurationDescription :: Lens' VPCEConfiguration (Maybe Text)
vecVpceConfigurationDescription = lens _vecVpceConfigurationDescription (\s a -> s {_vecVpceConfigurationDescription = a})

instance FromJSON VPCEConfiguration where
  parseJSON =
    withObject
      "VPCEConfiguration"
      ( \x ->
          VPCEConfiguration'
            <$> (x .:? "vpceServiceName")
            <*> (x .:? "arn")
            <*> (x .:? "vpceConfigurationName")
            <*> (x .:? "serviceDnsName")
            <*> (x .:? "vpceConfigurationDescription")
      )

instance Hashable VPCEConfiguration

instance NFData VPCEConfiguration
