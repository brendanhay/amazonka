{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KeyGroupList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KeyGroupList where

import Network.AWS.CloudFront.Types.KeyGroupSummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of key groups.
--
--
--
-- /See:/ 'keyGroupList' smart constructor.
data KeyGroupList = KeyGroupList'
  { _kglItems ::
      !(Maybe [KeyGroupSummary]),
    _kglNextMarker :: !(Maybe Text),
    _kglMaxItems :: !Int,
    _kglQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KeyGroupList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kglItems' - A list of key groups.
--
-- * 'kglNextMarker' - If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing key groups.
--
-- * 'kglMaxItems' - The maximum number of key groups requested.
--
-- * 'kglQuantity' - The number of key groups returned in the response.
keyGroupList ::
  -- | 'kglMaxItems'
  Int ->
  -- | 'kglQuantity'
  Int ->
  KeyGroupList
keyGroupList pMaxItems_ pQuantity_ =
  KeyGroupList'
    { _kglItems = Nothing,
      _kglNextMarker = Nothing,
      _kglMaxItems = pMaxItems_,
      _kglQuantity = pQuantity_
    }

-- | A list of key groups.
kglItems :: Lens' KeyGroupList [KeyGroupSummary]
kglItems = lens _kglItems (\s a -> s {_kglItems = a}) . _Default . _Coerce

-- | If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing key groups.
kglNextMarker :: Lens' KeyGroupList (Maybe Text)
kglNextMarker = lens _kglNextMarker (\s a -> s {_kglNextMarker = a})

-- | The maximum number of key groups requested.
kglMaxItems :: Lens' KeyGroupList Int
kglMaxItems = lens _kglMaxItems (\s a -> s {_kglMaxItems = a})

-- | The number of key groups returned in the response.
kglQuantity :: Lens' KeyGroupList Int
kglQuantity = lens _kglQuantity (\s a -> s {_kglQuantity = a})

instance FromXML KeyGroupList where
  parseXML x =
    KeyGroupList'
      <$> (x .@? "Items" .!@ mempty >>= may (parseXMLList "KeyGroupSummary"))
      <*> (x .@? "NextMarker")
      <*> (x .@ "MaxItems")
      <*> (x .@ "Quantity")

instance Hashable KeyGroupList

instance NFData KeyGroupList
