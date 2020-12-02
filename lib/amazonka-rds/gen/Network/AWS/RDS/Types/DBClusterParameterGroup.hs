{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterParameterGroup where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the details of an Amazon RDS DB cluster parameter group.
--
--
-- This data type is used as a response element in the @DescribeDBClusterParameterGroups@ action.
--
--
-- /See:/ 'dbClusterParameterGroup' smart constructor.
data DBClusterParameterGroup = DBClusterParameterGroup'
  { _dcpgDBClusterParameterGroupARN ::
      !(Maybe Text),
    _dcpgDBParameterGroupFamily ::
      !(Maybe Text),
    _dcpgDBClusterParameterGroupName ::
      !(Maybe Text),
    _dcpgDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBClusterParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpgDBClusterParameterGroupARN' - The Amazon Resource Name (ARN) for the DB cluster parameter group.
--
-- * 'dcpgDBParameterGroupFamily' - The name of the DB parameter group family that this DB cluster parameter group is compatible with.
--
-- * 'dcpgDBClusterParameterGroupName' - The name of the DB cluster parameter group.
--
-- * 'dcpgDescription' - Provides the customer-specified description for this DB cluster parameter group.
dbClusterParameterGroup ::
  DBClusterParameterGroup
dbClusterParameterGroup =
  DBClusterParameterGroup'
    { _dcpgDBClusterParameterGroupARN =
        Nothing,
      _dcpgDBParameterGroupFamily = Nothing,
      _dcpgDBClusterParameterGroupName = Nothing,
      _dcpgDescription = Nothing
    }

-- | The Amazon Resource Name (ARN) for the DB cluster parameter group.
dcpgDBClusterParameterGroupARN :: Lens' DBClusterParameterGroup (Maybe Text)
dcpgDBClusterParameterGroupARN = lens _dcpgDBClusterParameterGroupARN (\s a -> s {_dcpgDBClusterParameterGroupARN = a})

-- | The name of the DB parameter group family that this DB cluster parameter group is compatible with.
dcpgDBParameterGroupFamily :: Lens' DBClusterParameterGroup (Maybe Text)
dcpgDBParameterGroupFamily = lens _dcpgDBParameterGroupFamily (\s a -> s {_dcpgDBParameterGroupFamily = a})

-- | The name of the DB cluster parameter group.
dcpgDBClusterParameterGroupName :: Lens' DBClusterParameterGroup (Maybe Text)
dcpgDBClusterParameterGroupName = lens _dcpgDBClusterParameterGroupName (\s a -> s {_dcpgDBClusterParameterGroupName = a})

-- | Provides the customer-specified description for this DB cluster parameter group.
dcpgDescription :: Lens' DBClusterParameterGroup (Maybe Text)
dcpgDescription = lens _dcpgDescription (\s a -> s {_dcpgDescription = a})

instance FromXML DBClusterParameterGroup where
  parseXML x =
    DBClusterParameterGroup'
      <$> (x .@? "DBClusterParameterGroupArn")
      <*> (x .@? "DBParameterGroupFamily")
      <*> (x .@? "DBClusterParameterGroupName")
      <*> (x .@? "Description")

instance Hashable DBClusterParameterGroup

instance NFData DBClusterParameterGroup
