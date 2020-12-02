{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfacePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfacePermission where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InterfacePermissionType
import Network.AWS.EC2.Types.NetworkInterfacePermissionState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a permission for a network interface.
--
--
--
-- /See:/ 'networkInterfacePermission' smart constructor.
data NetworkInterfacePermission = NetworkInterfacePermission'
  { _nipPermissionState ::
      !( Maybe
           NetworkInterfacePermissionState
       ),
    _nipNetworkInterfacePermissionId ::
      !(Maybe Text),
    _nipNetworkInterfaceId ::
      !(Maybe Text),
    _nipAWSAccountId :: !(Maybe Text),
    _nipAWSService :: !(Maybe Text),
    _nipPermission ::
      !(Maybe InterfacePermissionType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkInterfacePermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nipPermissionState' - Information about the state of the permission.
--
-- * 'nipNetworkInterfacePermissionId' - The ID of the network interface permission.
--
-- * 'nipNetworkInterfaceId' - The ID of the network interface.
--
-- * 'nipAWSAccountId' - The AWS account ID.
--
-- * 'nipAWSService' - The AWS service.
--
-- * 'nipPermission' - The type of permission.
networkInterfacePermission ::
  NetworkInterfacePermission
networkInterfacePermission =
  NetworkInterfacePermission'
    { _nipPermissionState = Nothing,
      _nipNetworkInterfacePermissionId = Nothing,
      _nipNetworkInterfaceId = Nothing,
      _nipAWSAccountId = Nothing,
      _nipAWSService = Nothing,
      _nipPermission = Nothing
    }

-- | Information about the state of the permission.
nipPermissionState :: Lens' NetworkInterfacePermission (Maybe NetworkInterfacePermissionState)
nipPermissionState = lens _nipPermissionState (\s a -> s {_nipPermissionState = a})

-- | The ID of the network interface permission.
nipNetworkInterfacePermissionId :: Lens' NetworkInterfacePermission (Maybe Text)
nipNetworkInterfacePermissionId = lens _nipNetworkInterfacePermissionId (\s a -> s {_nipNetworkInterfacePermissionId = a})

-- | The ID of the network interface.
nipNetworkInterfaceId :: Lens' NetworkInterfacePermission (Maybe Text)
nipNetworkInterfaceId = lens _nipNetworkInterfaceId (\s a -> s {_nipNetworkInterfaceId = a})

-- | The AWS account ID.
nipAWSAccountId :: Lens' NetworkInterfacePermission (Maybe Text)
nipAWSAccountId = lens _nipAWSAccountId (\s a -> s {_nipAWSAccountId = a})

-- | The AWS service.
nipAWSService :: Lens' NetworkInterfacePermission (Maybe Text)
nipAWSService = lens _nipAWSService (\s a -> s {_nipAWSService = a})

-- | The type of permission.
nipPermission :: Lens' NetworkInterfacePermission (Maybe InterfacePermissionType)
nipPermission = lens _nipPermission (\s a -> s {_nipPermission = a})

instance FromXML NetworkInterfacePermission where
  parseXML x =
    NetworkInterfacePermission'
      <$> (x .@? "permissionState")
      <*> (x .@? "networkInterfacePermissionId")
      <*> (x .@? "networkInterfaceId")
      <*> (x .@? "awsAccountId")
      <*> (x .@? "awsService")
      <*> (x .@? "permission")

instance Hashable NetworkInterfacePermission

instance NFData NetworkInterfacePermission
