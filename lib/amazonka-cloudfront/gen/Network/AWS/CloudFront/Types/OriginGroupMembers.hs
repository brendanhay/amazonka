{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginGroupMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginGroupMembers where

import Network.AWS.CloudFront.Types.OriginGroupMember
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex data type for the origins included in an origin group.
--
--
--
-- /See:/ 'originGroupMembers' smart constructor.
data OriginGroupMembers = OriginGroupMembers'
  { _ogmQuantity :: !Int,
    _ogmItems :: !(List1 OriginGroupMember)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OriginGroupMembers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogmQuantity' - The number of origins in an origin group.
--
-- * 'ogmItems' - Items (origins) in an origin group.
originGroupMembers ::
  -- | 'ogmQuantity'
  Int ->
  -- | 'ogmItems'
  NonEmpty OriginGroupMember ->
  OriginGroupMembers
originGroupMembers pQuantity_ pItems_ =
  OriginGroupMembers'
    { _ogmQuantity = pQuantity_,
      _ogmItems = _List1 # pItems_
    }

-- | The number of origins in an origin group.
ogmQuantity :: Lens' OriginGroupMembers Int
ogmQuantity = lens _ogmQuantity (\s a -> s {_ogmQuantity = a})

-- | Items (origins) in an origin group.
ogmItems :: Lens' OriginGroupMembers (NonEmpty OriginGroupMember)
ogmItems = lens _ogmItems (\s a -> s {_ogmItems = a}) . _List1

instance FromXML OriginGroupMembers where
  parseXML x =
    OriginGroupMembers'
      <$> (x .@ "Quantity")
      <*> (x .@? "Items" .!@ mempty >>= parseXMLList1 "OriginGroupMember")

instance Hashable OriginGroupMembers

instance NFData OriginGroupMembers

instance ToXML OriginGroupMembers where
  toXML OriginGroupMembers' {..} =
    mconcat
      [ "Quantity" @= _ogmQuantity,
        "Items" @= toXMLList "OriginGroupMember" _ogmItems
      ]
