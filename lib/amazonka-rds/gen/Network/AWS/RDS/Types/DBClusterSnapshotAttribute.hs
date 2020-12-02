{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterSnapshotAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterSnapshotAttribute where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the name and values of a manual DB cluster snapshot attribute.
--
--
-- Manual DB cluster snapshot attributes are used to authorize other AWS accounts to restore a manual DB cluster snapshot. For more information, see the @ModifyDBClusterSnapshotAttribute@ API action.
--
--
-- /See:/ 'dbClusterSnapshotAttribute' smart constructor.
data DBClusterSnapshotAttribute = DBClusterSnapshotAttribute'
  { _dcsaAttributeValues ::
      !(Maybe [Text]),
    _dcsaAttributeName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBClusterSnapshotAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsaAttributeValues' - The value(s) for the manual DB cluster snapshot attribute. If the @AttributeName@ field is set to @restore@ , then this element returns a list of IDs of the AWS accounts that are authorized to copy or restore the manual DB cluster snapshot. If a value of @all@ is in the list, then the manual DB cluster snapshot is public and available for any AWS account to copy or restore.
--
-- * 'dcsaAttributeName' - The name of the manual DB cluster snapshot attribute. The attribute named @restore@ refers to the list of AWS accounts that have permission to copy or restore the manual DB cluster snapshot. For more information, see the @ModifyDBClusterSnapshotAttribute@ API action.
dbClusterSnapshotAttribute ::
  DBClusterSnapshotAttribute
dbClusterSnapshotAttribute =
  DBClusterSnapshotAttribute'
    { _dcsaAttributeValues = Nothing,
      _dcsaAttributeName = Nothing
    }

-- | The value(s) for the manual DB cluster snapshot attribute. If the @AttributeName@ field is set to @restore@ , then this element returns a list of IDs of the AWS accounts that are authorized to copy or restore the manual DB cluster snapshot. If a value of @all@ is in the list, then the manual DB cluster snapshot is public and available for any AWS account to copy or restore.
dcsaAttributeValues :: Lens' DBClusterSnapshotAttribute [Text]
dcsaAttributeValues = lens _dcsaAttributeValues (\s a -> s {_dcsaAttributeValues = a}) . _Default . _Coerce

-- | The name of the manual DB cluster snapshot attribute. The attribute named @restore@ refers to the list of AWS accounts that have permission to copy or restore the manual DB cluster snapshot. For more information, see the @ModifyDBClusterSnapshotAttribute@ API action.
dcsaAttributeName :: Lens' DBClusterSnapshotAttribute (Maybe Text)
dcsaAttributeName = lens _dcsaAttributeName (\s a -> s {_dcsaAttributeName = a})

instance FromXML DBClusterSnapshotAttribute where
  parseXML x =
    DBClusterSnapshotAttribute'
      <$> ( x .@? "AttributeValues" .!@ mempty
              >>= may (parseXMLList "AttributeValue")
          )
      <*> (x .@? "AttributeName")

instance Hashable DBClusterSnapshotAttribute

instance NFData DBClusterSnapshotAttribute
