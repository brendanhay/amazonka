{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBSnapshotAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBSnapshotAttribute where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the name and values of a manual DB snapshot attribute
--
--
-- Manual DB snapshot attributes are used to authorize other AWS accounts to restore a manual DB snapshot. For more information, see the @ModifyDBSnapshotAttribute@ API.
--
--
-- /See:/ 'dbSnapshotAttribute' smart constructor.
data DBSnapshotAttribute = DBSnapshotAttribute'
  { _dsaAttributeValues ::
      !(Maybe [Text]),
    _dsaAttributeName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBSnapshotAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsaAttributeValues' - The value or values for the manual DB snapshot attribute. If the @AttributeName@ field is set to @restore@ , then this element returns a list of IDs of the AWS accounts that are authorized to copy or restore the manual DB snapshot. If a value of @all@ is in the list, then the manual DB snapshot is public and available for any AWS account to copy or restore.
--
-- * 'dsaAttributeName' - The name of the manual DB snapshot attribute. The attribute named @restore@ refers to the list of AWS accounts that have permission to copy or restore the manual DB cluster snapshot. For more information, see the @ModifyDBSnapshotAttribute@ API action.
dbSnapshotAttribute ::
  DBSnapshotAttribute
dbSnapshotAttribute =
  DBSnapshotAttribute'
    { _dsaAttributeValues = Nothing,
      _dsaAttributeName = Nothing
    }

-- | The value or values for the manual DB snapshot attribute. If the @AttributeName@ field is set to @restore@ , then this element returns a list of IDs of the AWS accounts that are authorized to copy or restore the manual DB snapshot. If a value of @all@ is in the list, then the manual DB snapshot is public and available for any AWS account to copy or restore.
dsaAttributeValues :: Lens' DBSnapshotAttribute [Text]
dsaAttributeValues = lens _dsaAttributeValues (\s a -> s {_dsaAttributeValues = a}) . _Default . _Coerce

-- | The name of the manual DB snapshot attribute. The attribute named @restore@ refers to the list of AWS accounts that have permission to copy or restore the manual DB cluster snapshot. For more information, see the @ModifyDBSnapshotAttribute@ API action.
dsaAttributeName :: Lens' DBSnapshotAttribute (Maybe Text)
dsaAttributeName = lens _dsaAttributeName (\s a -> s {_dsaAttributeName = a})

instance FromXML DBSnapshotAttribute where
  parseXML x =
    DBSnapshotAttribute'
      <$> ( x .@? "AttributeValues" .!@ mempty
              >>= may (parseXMLList "AttributeValue")
          )
      <*> (x .@? "AttributeName")

instance Hashable DBSnapshotAttribute

instance NFData DBSnapshotAttribute
