{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkACLAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkACLAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an association between a network ACL and a subnet.
--
--
--
-- /See:/ 'networkACLAssociation' smart constructor.
data NetworkACLAssociation = NetworkACLAssociation'
  { _naaNetworkACLId ::
      !(Maybe Text),
    _naaSubnetId :: !(Maybe Text),
    _naaNetworkACLAssociationId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkACLAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'naaNetworkACLId' - The ID of the network ACL.
--
-- * 'naaSubnetId' - The ID of the subnet.
--
-- * 'naaNetworkACLAssociationId' - The ID of the association between a network ACL and a subnet.
networkACLAssociation ::
  NetworkACLAssociation
networkACLAssociation =
  NetworkACLAssociation'
    { _naaNetworkACLId = Nothing,
      _naaSubnetId = Nothing,
      _naaNetworkACLAssociationId = Nothing
    }

-- | The ID of the network ACL.
naaNetworkACLId :: Lens' NetworkACLAssociation (Maybe Text)
naaNetworkACLId = lens _naaNetworkACLId (\s a -> s {_naaNetworkACLId = a})

-- | The ID of the subnet.
naaSubnetId :: Lens' NetworkACLAssociation (Maybe Text)
naaSubnetId = lens _naaSubnetId (\s a -> s {_naaSubnetId = a})

-- | The ID of the association between a network ACL and a subnet.
naaNetworkACLAssociationId :: Lens' NetworkACLAssociation (Maybe Text)
naaNetworkACLAssociationId = lens _naaNetworkACLAssociationId (\s a -> s {_naaNetworkACLAssociationId = a})

instance FromXML NetworkACLAssociation where
  parseXML x =
    NetworkACLAssociation'
      <$> (x .@? "networkAclId")
      <*> (x .@? "subnetId")
      <*> (x .@? "networkAclAssociationId")

instance Hashable NetworkACLAssociation

instance NFData NetworkACLAssociation
