{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterSnapshotAttributesResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterSnapshotAttributesResult where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.DBClusterSnapshotAttribute

-- | Contains the results of a successful call to the @DescribeDBClusterSnapshotAttributes@ API action.
--
--
-- Manual DB cluster snapshot attributes are used to authorize other AWS accounts to copy or restore a manual DB cluster snapshot. For more information, see the @ModifyDBClusterSnapshotAttribute@ API action.
--
--
-- /See:/ 'dbClusterSnapshotAttributesResult' smart constructor.
data DBClusterSnapshotAttributesResult = DBClusterSnapshotAttributesResult'
  { _dcsarDBClusterSnapshotIdentifier ::
      !(Maybe Text),
    _dcsarDBClusterSnapshotAttributes ::
      !( Maybe
           [DBClusterSnapshotAttribute]
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBClusterSnapshotAttributesResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsarDBClusterSnapshotIdentifier' - The identifier of the manual DB cluster snapshot that the attributes apply to.
--
-- * 'dcsarDBClusterSnapshotAttributes' - The list of attributes and values for the manual DB cluster snapshot.
dbClusterSnapshotAttributesResult ::
  DBClusterSnapshotAttributesResult
dbClusterSnapshotAttributesResult =
  DBClusterSnapshotAttributesResult'
    { _dcsarDBClusterSnapshotIdentifier =
        Nothing,
      _dcsarDBClusterSnapshotAttributes = Nothing
    }

-- | The identifier of the manual DB cluster snapshot that the attributes apply to.
dcsarDBClusterSnapshotIdentifier :: Lens' DBClusterSnapshotAttributesResult (Maybe Text)
dcsarDBClusterSnapshotIdentifier = lens _dcsarDBClusterSnapshotIdentifier (\s a -> s {_dcsarDBClusterSnapshotIdentifier = a})

-- | The list of attributes and values for the manual DB cluster snapshot.
dcsarDBClusterSnapshotAttributes :: Lens' DBClusterSnapshotAttributesResult [DBClusterSnapshotAttribute]
dcsarDBClusterSnapshotAttributes = lens _dcsarDBClusterSnapshotAttributes (\s a -> s {_dcsarDBClusterSnapshotAttributes = a}) . _Default . _Coerce

instance FromXML DBClusterSnapshotAttributesResult where
  parseXML x =
    DBClusterSnapshotAttributesResult'
      <$> (x .@? "DBClusterSnapshotIdentifier")
      <*> ( x .@? "DBClusterSnapshotAttributes" .!@ mempty
              >>= may (parseXMLList "DBClusterSnapshotAttribute")
          )

instance Hashable DBClusterSnapshotAttributesResult

instance NFData DBClusterSnapshotAttributesResult
