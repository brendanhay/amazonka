{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBSnapshotAttributesResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBSnapshotAttributesResult where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.DBSnapshotAttribute

-- | Contains the results of a successful call to the @DescribeDBSnapshotAttributes@ API action.
--
--
-- Manual DB snapshot attributes are used to authorize other AWS accounts to copy or restore a manual DB snapshot. For more information, see the @ModifyDBSnapshotAttribute@ API action.
--
--
-- /See:/ 'dbSnapshotAttributesResult' smart constructor.
data DBSnapshotAttributesResult = DBSnapshotAttributesResult'
  { _dsarDBSnapshotIdentifier ::
      !(Maybe Text),
    _dsarDBSnapshotAttributes ::
      !(Maybe [DBSnapshotAttribute])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBSnapshotAttributesResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsarDBSnapshotIdentifier' - The identifier of the manual DB snapshot that the attributes apply to.
--
-- * 'dsarDBSnapshotAttributes' - The list of attributes and values for the manual DB snapshot.
dbSnapshotAttributesResult ::
  DBSnapshotAttributesResult
dbSnapshotAttributesResult =
  DBSnapshotAttributesResult'
    { _dsarDBSnapshotIdentifier = Nothing,
      _dsarDBSnapshotAttributes = Nothing
    }

-- | The identifier of the manual DB snapshot that the attributes apply to.
dsarDBSnapshotIdentifier :: Lens' DBSnapshotAttributesResult (Maybe Text)
dsarDBSnapshotIdentifier = lens _dsarDBSnapshotIdentifier (\s a -> s {_dsarDBSnapshotIdentifier = a})

-- | The list of attributes and values for the manual DB snapshot.
dsarDBSnapshotAttributes :: Lens' DBSnapshotAttributesResult [DBSnapshotAttribute]
dsarDBSnapshotAttributes = lens _dsarDBSnapshotAttributes (\s a -> s {_dsarDBSnapshotAttributes = a}) . _Default . _Coerce

instance FromXML DBSnapshotAttributesResult where
  parseXML x =
    DBSnapshotAttributesResult'
      <$> (x .@? "DBSnapshotIdentifier")
      <*> ( x .@? "DBSnapshotAttributes" .!@ mempty
              >>= may (parseXMLList "DBSnapshotAttribute")
          )

instance Hashable DBSnapshotAttributesResult

instance NFData DBSnapshotAttributesResult
