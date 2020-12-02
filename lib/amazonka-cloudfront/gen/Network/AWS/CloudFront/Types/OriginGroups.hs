{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginGroups where

import Network.AWS.CloudFront.Types.OriginGroup
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex data type for the origin groups specified for a distribution.
--
--
--
-- /See:/ 'originGroups' smart constructor.
data OriginGroups = OriginGroups'
  { _ogItems ::
      !(Maybe [OriginGroup]),
    _ogQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OriginGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogItems' - The items (origin groups) in a distribution.
--
-- * 'ogQuantity' - The number of origin groups.
originGroups ::
  -- | 'ogQuantity'
  Int ->
  OriginGroups
originGroups pQuantity_ =
  OriginGroups' {_ogItems = Nothing, _ogQuantity = pQuantity_}

-- | The items (origin groups) in a distribution.
ogItems :: Lens' OriginGroups [OriginGroup]
ogItems = lens _ogItems (\s a -> s {_ogItems = a}) . _Default . _Coerce

-- | The number of origin groups.
ogQuantity :: Lens' OriginGroups Int
ogQuantity = lens _ogQuantity (\s a -> s {_ogQuantity = a})

instance FromXML OriginGroups where
  parseXML x =
    OriginGroups'
      <$> (x .@? "Items" .!@ mempty >>= may (parseXMLList "OriginGroup"))
      <*> (x .@ "Quantity")

instance Hashable OriginGroups

instance NFData OriginGroups

instance ToXML OriginGroups where
  toXML OriginGroups' {..} =
    mconcat
      [ "Items" @= toXML (toXMLList "OriginGroup" <$> _ogItems),
        "Quantity" @= _ogQuantity
      ]
