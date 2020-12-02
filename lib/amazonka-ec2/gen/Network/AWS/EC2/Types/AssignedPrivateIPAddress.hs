{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AssignedPrivateIPAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AssignedPrivateIPAddress where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the private IP addresses assigned to a network interface.
--
--
--
-- /See:/ 'assignedPrivateIPAddress' smart constructor.
newtype AssignedPrivateIPAddress = AssignedPrivateIPAddress'
  { _apiaPrivateIPAddress ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssignedPrivateIPAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apiaPrivateIPAddress' - The private IP address assigned to the network interface.
assignedPrivateIPAddress ::
  AssignedPrivateIPAddress
assignedPrivateIPAddress =
  AssignedPrivateIPAddress' {_apiaPrivateIPAddress = Nothing}

-- | The private IP address assigned to the network interface.
apiaPrivateIPAddress :: Lens' AssignedPrivateIPAddress (Maybe Text)
apiaPrivateIPAddress = lens _apiaPrivateIPAddress (\s a -> s {_apiaPrivateIPAddress = a})

instance FromXML AssignedPrivateIPAddress where
  parseXML x =
    AssignedPrivateIPAddress' <$> (x .@? "privateIpAddress")

instance Hashable AssignedPrivateIPAddress

instance NFData AssignedPrivateIPAddress
