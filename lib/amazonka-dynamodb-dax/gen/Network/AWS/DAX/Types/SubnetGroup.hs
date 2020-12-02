{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.SubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.SubnetGroup where

import Network.AWS.DAX.Types.Subnet
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of one of the following actions:
--
--
--     * /CreateSubnetGroup/
--
--     * /ModifySubnetGroup/
--
--
--
--
-- /See:/ 'subnetGroup' smart constructor.
data SubnetGroup = SubnetGroup'
  { _sgVPCId :: !(Maybe Text),
    _sgSubnets :: !(Maybe [Subnet]),
    _sgSubnetGroupName :: !(Maybe Text),
    _sgDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgVPCId' - The Amazon Virtual Private Cloud identifier (VPC ID) of the subnet group.
--
-- * 'sgSubnets' - A list of subnets associated with the subnet group.
--
-- * 'sgSubnetGroupName' - The name of the subnet group.
--
-- * 'sgDescription' - The description of the subnet group.
subnetGroup ::
  SubnetGroup
subnetGroup =
  SubnetGroup'
    { _sgVPCId = Nothing,
      _sgSubnets = Nothing,
      _sgSubnetGroupName = Nothing,
      _sgDescription = Nothing
    }

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the subnet group.
sgVPCId :: Lens' SubnetGroup (Maybe Text)
sgVPCId = lens _sgVPCId (\s a -> s {_sgVPCId = a})

-- | A list of subnets associated with the subnet group.
sgSubnets :: Lens' SubnetGroup [Subnet]
sgSubnets = lens _sgSubnets (\s a -> s {_sgSubnets = a}) . _Default . _Coerce

-- | The name of the subnet group.
sgSubnetGroupName :: Lens' SubnetGroup (Maybe Text)
sgSubnetGroupName = lens _sgSubnetGroupName (\s a -> s {_sgSubnetGroupName = a})

-- | The description of the subnet group.
sgDescription :: Lens' SubnetGroup (Maybe Text)
sgDescription = lens _sgDescription (\s a -> s {_sgDescription = a})

instance FromJSON SubnetGroup where
  parseJSON =
    withObject
      "SubnetGroup"
      ( \x ->
          SubnetGroup'
            <$> (x .:? "VpcId")
            <*> (x .:? "Subnets" .!= mempty)
            <*> (x .:? "SubnetGroupName")
            <*> (x .:? "Description")
      )

instance Hashable SubnetGroup

instance NFData SubnetGroup
