{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AssociatedTargetNetwork
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AssociatedTargetNetwork where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AssociatedNetworkType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a target network that is associated with a Client VPN endpoint. A target network is a subnet in a VPC.
--
--
--
-- /See:/ 'associatedTargetNetwork' smart constructor.
data AssociatedTargetNetwork = AssociatedTargetNetwork'
  { _atnNetworkId ::
      !(Maybe Text),
    _atnNetworkType ::
      !(Maybe AssociatedNetworkType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociatedTargetNetwork' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atnNetworkId' - The ID of the subnet.
--
-- * 'atnNetworkType' - The target network type.
associatedTargetNetwork ::
  AssociatedTargetNetwork
associatedTargetNetwork =
  AssociatedTargetNetwork'
    { _atnNetworkId = Nothing,
      _atnNetworkType = Nothing
    }

-- | The ID of the subnet.
atnNetworkId :: Lens' AssociatedTargetNetwork (Maybe Text)
atnNetworkId = lens _atnNetworkId (\s a -> s {_atnNetworkId = a})

-- | The target network type.
atnNetworkType :: Lens' AssociatedTargetNetwork (Maybe AssociatedNetworkType)
atnNetworkType = lens _atnNetworkType (\s a -> s {_atnNetworkType = a})

instance FromXML AssociatedTargetNetwork where
  parseXML x =
    AssociatedTargetNetwork'
      <$> (x .@? "networkId") <*> (x .@? "networkType")

instance Hashable AssociatedTargetNetwork

instance NFData AssociatedTargetNetwork
