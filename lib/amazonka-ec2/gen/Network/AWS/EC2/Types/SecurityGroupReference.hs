{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SecurityGroupReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SecurityGroupReference where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a VPC with a security group that references your security group.
--
--
--
-- /See:/ 'securityGroupReference' smart constructor.
data SecurityGroupReference = SecurityGroupReference'
  { _sgrVPCPeeringConnectionId ::
      !(Maybe Text),
    _sgrReferencingVPCId :: !(Maybe Text),
    _sgrGroupId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SecurityGroupReference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgrVPCPeeringConnectionId' - The ID of the VPC peering connection.
--
-- * 'sgrReferencingVPCId' - The ID of the VPC with the referencing security group.
--
-- * 'sgrGroupId' - The ID of your security group.
securityGroupReference ::
  SecurityGroupReference
securityGroupReference =
  SecurityGroupReference'
    { _sgrVPCPeeringConnectionId = Nothing,
      _sgrReferencingVPCId = Nothing,
      _sgrGroupId = Nothing
    }

-- | The ID of the VPC peering connection.
sgrVPCPeeringConnectionId :: Lens' SecurityGroupReference (Maybe Text)
sgrVPCPeeringConnectionId = lens _sgrVPCPeeringConnectionId (\s a -> s {_sgrVPCPeeringConnectionId = a})

-- | The ID of the VPC with the referencing security group.
sgrReferencingVPCId :: Lens' SecurityGroupReference (Maybe Text)
sgrReferencingVPCId = lens _sgrReferencingVPCId (\s a -> s {_sgrReferencingVPCId = a})

-- | The ID of your security group.
sgrGroupId :: Lens' SecurityGroupReference (Maybe Text)
sgrGroupId = lens _sgrGroupId (\s a -> s {_sgrGroupId = a})

instance FromXML SecurityGroupReference where
  parseXML x =
    SecurityGroupReference'
      <$> (x .@? "vpcPeeringConnectionId")
      <*> (x .@? "referencingVpcId")
      <*> (x .@? "groupId")

instance Hashable SecurityGroupReference

instance NFData SecurityGroupReference
