{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.Item
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.Item where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SDB.Types.Attribute

-- |
--
--
--
-- /See:/ 'item' smart constructor.
data Item = Item'
  { _iAlternateNameEncoding :: !(Maybe Text),
    _iName :: !Text,
    _iAttributes :: ![Attribute]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Item' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iAlternateNameEncoding' -
--
-- * 'iName' - The name of the item.
--
-- * 'iAttributes' - A list of attributes.
item ::
  -- | 'iName'
  Text ->
  Item
item pName_ =
  Item'
    { _iAlternateNameEncoding = Nothing,
      _iName = pName_,
      _iAttributes = mempty
    }

-- |
iAlternateNameEncoding :: Lens' Item (Maybe Text)
iAlternateNameEncoding = lens _iAlternateNameEncoding (\s a -> s {_iAlternateNameEncoding = a})

-- | The name of the item.
iName :: Lens' Item Text
iName = lens _iName (\s a -> s {_iName = a})

-- | A list of attributes.
iAttributes :: Lens' Item [Attribute]
iAttributes = lens _iAttributes (\s a -> s {_iAttributes = a}) . _Coerce

instance FromXML Item where
  parseXML x =
    Item'
      <$> (x .@? "AlternateNameEncoding")
      <*> (x .@ "Name")
      <*> (parseXMLList "Attribute" x)

instance Hashable Item

instance NFData Item
