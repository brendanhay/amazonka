{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SubnetAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SubnetAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TransitGatewayMulitcastDomainAssociationState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the subnet association with the transit gateway multicast domain.
--
--
--
-- /See:/ 'subnetAssociation' smart constructor.
data SubnetAssociation = SubnetAssociation'
  { _saState ::
      !(Maybe TransitGatewayMulitcastDomainAssociationState),
    _saSubnetId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SubnetAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saState' - The state of the subnet association.
--
-- * 'saSubnetId' - The ID of the subnet.
subnetAssociation ::
  SubnetAssociation
subnetAssociation =
  SubnetAssociation' {_saState = Nothing, _saSubnetId = Nothing}

-- | The state of the subnet association.
saState :: Lens' SubnetAssociation (Maybe TransitGatewayMulitcastDomainAssociationState)
saState = lens _saState (\s a -> s {_saState = a})

-- | The ID of the subnet.
saSubnetId :: Lens' SubnetAssociation (Maybe Text)
saSubnetId = lens _saSubnetId (\s a -> s {_saSubnetId = a})

instance FromXML SubnetAssociation where
  parseXML x =
    SubnetAssociation' <$> (x .@? "state") <*> (x .@? "subnetId")

instance Hashable SubnetAssociation

instance NFData SubnetAssociation
