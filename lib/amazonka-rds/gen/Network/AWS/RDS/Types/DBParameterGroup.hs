{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBParameterGroup where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the details of an Amazon RDS DB parameter group.
--
--
-- This data type is used as a response element in the @DescribeDBParameterGroups@ action.
--
--
-- /See:/ 'dbParameterGroup' smart constructor.
data DBParameterGroup = DBParameterGroup'
  { _dpgDBParameterGroupARN ::
      !(Maybe Text),
    _dpgDBParameterGroupFamily :: !(Maybe Text),
    _dpgDBParameterGroupName :: !(Maybe Text),
    _dpgDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgDBParameterGroupARN' - The Amazon Resource Name (ARN) for the DB parameter group.
--
-- * 'dpgDBParameterGroupFamily' - The name of the DB parameter group family that this DB parameter group is compatible with.
--
-- * 'dpgDBParameterGroupName' - The name of the DB parameter group.
--
-- * 'dpgDescription' - Provides the customer-specified description for this DB parameter group.
dbParameterGroup ::
  DBParameterGroup
dbParameterGroup =
  DBParameterGroup'
    { _dpgDBParameterGroupARN = Nothing,
      _dpgDBParameterGroupFamily = Nothing,
      _dpgDBParameterGroupName = Nothing,
      _dpgDescription = Nothing
    }

-- | The Amazon Resource Name (ARN) for the DB parameter group.
dpgDBParameterGroupARN :: Lens' DBParameterGroup (Maybe Text)
dpgDBParameterGroupARN = lens _dpgDBParameterGroupARN (\s a -> s {_dpgDBParameterGroupARN = a})

-- | The name of the DB parameter group family that this DB parameter group is compatible with.
dpgDBParameterGroupFamily :: Lens' DBParameterGroup (Maybe Text)
dpgDBParameterGroupFamily = lens _dpgDBParameterGroupFamily (\s a -> s {_dpgDBParameterGroupFamily = a})

-- | The name of the DB parameter group.
dpgDBParameterGroupName :: Lens' DBParameterGroup (Maybe Text)
dpgDBParameterGroupName = lens _dpgDBParameterGroupName (\s a -> s {_dpgDBParameterGroupName = a})

-- | Provides the customer-specified description for this DB parameter group.
dpgDescription :: Lens' DBParameterGroup (Maybe Text)
dpgDescription = lens _dpgDescription (\s a -> s {_dpgDescription = a})

instance FromXML DBParameterGroup where
  parseXML x =
    DBParameterGroup'
      <$> (x .@? "DBParameterGroupArn")
      <*> (x .@? "DBParameterGroupFamily")
      <*> (x .@? "DBParameterGroupName")
      <*> (x .@? "Description")

instance Hashable DBParameterGroup

instance NFData DBParameterGroup
