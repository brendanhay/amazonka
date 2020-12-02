{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBSubnetGroup where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.Subnet

-- | Contains the details of an Amazon RDS DB subnet group.
--
--
-- This data type is used as a response element in the @DescribeDBSubnetGroups@ action.
--
--
-- /See:/ 'dbSubnetGroup' smart constructor.
data DBSubnetGroup = DBSubnetGroup'
  { _dsgDBSubnetGroupName ::
      !(Maybe Text),
    _dsgVPCId :: !(Maybe Text),
    _dsgSubnets :: !(Maybe [Subnet]),
    _dsgDBSubnetGroupDescription :: !(Maybe Text),
    _dsgDBSubnetGroupARN :: !(Maybe Text),
    _dsgSubnetGroupStatus :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsgDBSubnetGroupName' - The name of the DB subnet group.
--
-- * 'dsgVPCId' - Provides the VpcId of the DB subnet group.
--
-- * 'dsgSubnets' - Contains a list of @Subnet@ elements.
--
-- * 'dsgDBSubnetGroupDescription' - Provides the description of the DB subnet group.
--
-- * 'dsgDBSubnetGroupARN' - The Amazon Resource Name (ARN) for the DB subnet group.
--
-- * 'dsgSubnetGroupStatus' - Provides the status of the DB subnet group.
dbSubnetGroup ::
  DBSubnetGroup
dbSubnetGroup =
  DBSubnetGroup'
    { _dsgDBSubnetGroupName = Nothing,
      _dsgVPCId = Nothing,
      _dsgSubnets = Nothing,
      _dsgDBSubnetGroupDescription = Nothing,
      _dsgDBSubnetGroupARN = Nothing,
      _dsgSubnetGroupStatus = Nothing
    }

-- | The name of the DB subnet group.
dsgDBSubnetGroupName :: Lens' DBSubnetGroup (Maybe Text)
dsgDBSubnetGroupName = lens _dsgDBSubnetGroupName (\s a -> s {_dsgDBSubnetGroupName = a})

-- | Provides the VpcId of the DB subnet group.
dsgVPCId :: Lens' DBSubnetGroup (Maybe Text)
dsgVPCId = lens _dsgVPCId (\s a -> s {_dsgVPCId = a})

-- | Contains a list of @Subnet@ elements.
dsgSubnets :: Lens' DBSubnetGroup [Subnet]
dsgSubnets = lens _dsgSubnets (\s a -> s {_dsgSubnets = a}) . _Default . _Coerce

-- | Provides the description of the DB subnet group.
dsgDBSubnetGroupDescription :: Lens' DBSubnetGroup (Maybe Text)
dsgDBSubnetGroupDescription = lens _dsgDBSubnetGroupDescription (\s a -> s {_dsgDBSubnetGroupDescription = a})

-- | The Amazon Resource Name (ARN) for the DB subnet group.
dsgDBSubnetGroupARN :: Lens' DBSubnetGroup (Maybe Text)
dsgDBSubnetGroupARN = lens _dsgDBSubnetGroupARN (\s a -> s {_dsgDBSubnetGroupARN = a})

-- | Provides the status of the DB subnet group.
dsgSubnetGroupStatus :: Lens' DBSubnetGroup (Maybe Text)
dsgSubnetGroupStatus = lens _dsgSubnetGroupStatus (\s a -> s {_dsgSubnetGroupStatus = a})

instance FromXML DBSubnetGroup where
  parseXML x =
    DBSubnetGroup'
      <$> (x .@? "DBSubnetGroupName")
      <*> (x .@? "VpcId")
      <*> (x .@? "Subnets" .!@ mempty >>= may (parseXMLList "Subnet"))
      <*> (x .@? "DBSubnetGroupDescription")
      <*> (x .@? "DBSubnetGroupArn")
      <*> (x .@? "SubnetGroupStatus")

instance Hashable DBSubnetGroup

instance NFData DBSubnetGroup
