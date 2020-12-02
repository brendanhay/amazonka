{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBSecurityGroup where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.EC2SecurityGroup
import Network.AWS.RDS.Types.IPRange

-- | Contains the details for an Amazon RDS DB security group.
--
--
-- This data type is used as a response element in the @DescribeDBSecurityGroups@ action.
--
--
-- /See:/ 'dbSecurityGroup' smart constructor.
data DBSecurityGroup = DBSecurityGroup'
  { _dbsgVPCId ::
      !(Maybe Text),
    _dbsgOwnerId :: !(Maybe Text),
    _dbsgDBSecurityGroupARN :: !(Maybe Text),
    _dbsgIPRanges :: !(Maybe [IPRange]),
    _dbsgDBSecurityGroupName :: !(Maybe Text),
    _dbsgEC2SecurityGroups :: !(Maybe [EC2SecurityGroup]),
    _dbsgDBSecurityGroupDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbsgVPCId' - Provides the VpcId of the DB security group.
--
-- * 'dbsgOwnerId' - Provides the AWS ID of the owner of a specific DB security group.
--
-- * 'dbsgDBSecurityGroupARN' - The Amazon Resource Name (ARN) for the DB security group.
--
-- * 'dbsgIPRanges' - Contains a list of @IPRange@ elements.
--
-- * 'dbsgDBSecurityGroupName' - Specifies the name of the DB security group.
--
-- * 'dbsgEC2SecurityGroups' - Contains a list of @EC2SecurityGroup@ elements.
--
-- * 'dbsgDBSecurityGroupDescription' - Provides the description of the DB security group.
dbSecurityGroup ::
  DBSecurityGroup
dbSecurityGroup =
  DBSecurityGroup'
    { _dbsgVPCId = Nothing,
      _dbsgOwnerId = Nothing,
      _dbsgDBSecurityGroupARN = Nothing,
      _dbsgIPRanges = Nothing,
      _dbsgDBSecurityGroupName = Nothing,
      _dbsgEC2SecurityGroups = Nothing,
      _dbsgDBSecurityGroupDescription = Nothing
    }

-- | Provides the VpcId of the DB security group.
dbsgVPCId :: Lens' DBSecurityGroup (Maybe Text)
dbsgVPCId = lens _dbsgVPCId (\s a -> s {_dbsgVPCId = a})

-- | Provides the AWS ID of the owner of a specific DB security group.
dbsgOwnerId :: Lens' DBSecurityGroup (Maybe Text)
dbsgOwnerId = lens _dbsgOwnerId (\s a -> s {_dbsgOwnerId = a})

-- | The Amazon Resource Name (ARN) for the DB security group.
dbsgDBSecurityGroupARN :: Lens' DBSecurityGroup (Maybe Text)
dbsgDBSecurityGroupARN = lens _dbsgDBSecurityGroupARN (\s a -> s {_dbsgDBSecurityGroupARN = a})

-- | Contains a list of @IPRange@ elements.
dbsgIPRanges :: Lens' DBSecurityGroup [IPRange]
dbsgIPRanges = lens _dbsgIPRanges (\s a -> s {_dbsgIPRanges = a}) . _Default . _Coerce

-- | Specifies the name of the DB security group.
dbsgDBSecurityGroupName :: Lens' DBSecurityGroup (Maybe Text)
dbsgDBSecurityGroupName = lens _dbsgDBSecurityGroupName (\s a -> s {_dbsgDBSecurityGroupName = a})

-- | Contains a list of @EC2SecurityGroup@ elements.
dbsgEC2SecurityGroups :: Lens' DBSecurityGroup [EC2SecurityGroup]
dbsgEC2SecurityGroups = lens _dbsgEC2SecurityGroups (\s a -> s {_dbsgEC2SecurityGroups = a}) . _Default . _Coerce

-- | Provides the description of the DB security group.
dbsgDBSecurityGroupDescription :: Lens' DBSecurityGroup (Maybe Text)
dbsgDBSecurityGroupDescription = lens _dbsgDBSecurityGroupDescription (\s a -> s {_dbsgDBSecurityGroupDescription = a})

instance FromXML DBSecurityGroup where
  parseXML x =
    DBSecurityGroup'
      <$> (x .@? "VpcId")
      <*> (x .@? "OwnerId")
      <*> (x .@? "DBSecurityGroupArn")
      <*> (x .@? "IPRanges" .!@ mempty >>= may (parseXMLList "IPRange"))
      <*> (x .@? "DBSecurityGroupName")
      <*> ( x .@? "EC2SecurityGroups" .!@ mempty
              >>= may (parseXMLList "EC2SecurityGroup")
          )
      <*> (x .@? "DBSecurityGroupDescription")

instance Hashable DBSecurityGroup

instance NFData DBSecurityGroup
