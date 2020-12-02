{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfacePermissionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfacePermissionState where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.NetworkInterfacePermissionStateCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the state of a network interface permission.
--
--
--
-- /See:/ 'networkInterfacePermissionState' smart constructor.
data NetworkInterfacePermissionState = NetworkInterfacePermissionState'
  { _nipsState ::
      !( Maybe
           NetworkInterfacePermissionStateCode
       ),
    _nipsStatusMessage ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkInterfacePermissionState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nipsState' - The state of the permission.
--
-- * 'nipsStatusMessage' - A status message, if applicable.
networkInterfacePermissionState ::
  NetworkInterfacePermissionState
networkInterfacePermissionState =
  NetworkInterfacePermissionState'
    { _nipsState = Nothing,
      _nipsStatusMessage = Nothing
    }

-- | The state of the permission.
nipsState :: Lens' NetworkInterfacePermissionState (Maybe NetworkInterfacePermissionStateCode)
nipsState = lens _nipsState (\s a -> s {_nipsState = a})

-- | A status message, if applicable.
nipsStatusMessage :: Lens' NetworkInterfacePermissionState (Maybe Text)
nipsStatusMessage = lens _nipsStatusMessage (\s a -> s {_nipsStatusMessage = a})

instance FromXML NetworkInterfacePermissionState where
  parseXML x =
    NetworkInterfacePermissionState'
      <$> (x .@? "state") <*> (x .@? "statusMessage")

instance Hashable NetworkInterfacePermissionState

instance NFData NetworkInterfacePermissionState
