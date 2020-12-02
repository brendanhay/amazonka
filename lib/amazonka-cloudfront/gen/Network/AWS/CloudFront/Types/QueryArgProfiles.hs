{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.QueryArgProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.QueryArgProfiles where

import Network.AWS.CloudFront.Types.QueryArgProfile
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Query argument-profile mapping for field-level encryption.
--
--
--
-- /See:/ 'queryArgProfiles' smart constructor.
data QueryArgProfiles = QueryArgProfiles'
  { _qapItems ::
      !(Maybe [QueryArgProfile]),
    _qapQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QueryArgProfiles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qapItems' - Number of items for query argument-profile mapping for field-level encryption.
--
-- * 'qapQuantity' - Number of profiles for query argument-profile mapping for field-level encryption.
queryArgProfiles ::
  -- | 'qapQuantity'
  Int ->
  QueryArgProfiles
queryArgProfiles pQuantity_ =
  QueryArgProfiles' {_qapItems = Nothing, _qapQuantity = pQuantity_}

-- | Number of items for query argument-profile mapping for field-level encryption.
qapItems :: Lens' QueryArgProfiles [QueryArgProfile]
qapItems = lens _qapItems (\s a -> s {_qapItems = a}) . _Default . _Coerce

-- | Number of profiles for query argument-profile mapping for field-level encryption.
qapQuantity :: Lens' QueryArgProfiles Int
qapQuantity = lens _qapQuantity (\s a -> s {_qapQuantity = a})

instance FromXML QueryArgProfiles where
  parseXML x =
    QueryArgProfiles'
      <$> (x .@? "Items" .!@ mempty >>= may (parseXMLList "QueryArgProfile"))
      <*> (x .@ "Quantity")

instance Hashable QueryArgProfiles

instance NFData QueryArgProfiles

instance ToXML QueryArgProfiles where
  toXML QueryArgProfiles' {..} =
    mconcat
      [ "Items" @= toXML (toXMLList "QueryArgProfile" <$> _qapItems),
        "Quantity" @= _qapQuantity
      ]
