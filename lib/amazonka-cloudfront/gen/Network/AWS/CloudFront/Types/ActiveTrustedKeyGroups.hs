{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ActiveTrustedKeyGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ActiveTrustedKeyGroups where

import Network.AWS.CloudFront.Types.KGKeyPairIds
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of key groups, and the public keys in each key group, that CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
--
--
-- /See:/ 'activeTrustedKeyGroups' smart constructor.
data ActiveTrustedKeyGroups = ActiveTrustedKeyGroups'
  { _atkgItems ::
      !(Maybe [KGKeyPairIds]),
    _atkgEnabled :: !Bool,
    _atkgQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActiveTrustedKeyGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atkgItems' - A list of key groups, including the identifiers of the public keys in each key group that CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
-- * 'atkgEnabled' - This field is @true@ if any of the key groups have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
--
-- * 'atkgQuantity' - The number of key groups in the list.
activeTrustedKeyGroups ::
  -- | 'atkgEnabled'
  Bool ->
  -- | 'atkgQuantity'
  Int ->
  ActiveTrustedKeyGroups
activeTrustedKeyGroups pEnabled_ pQuantity_ =
  ActiveTrustedKeyGroups'
    { _atkgItems = Nothing,
      _atkgEnabled = pEnabled_,
      _atkgQuantity = pQuantity_
    }

-- | A list of key groups, including the identifiers of the public keys in each key group that CloudFront can use to verify the signatures of signed URLs and signed cookies.
atkgItems :: Lens' ActiveTrustedKeyGroups [KGKeyPairIds]
atkgItems = lens _atkgItems (\s a -> s {_atkgItems = a}) . _Default . _Coerce

-- | This field is @true@ if any of the key groups have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
atkgEnabled :: Lens' ActiveTrustedKeyGroups Bool
atkgEnabled = lens _atkgEnabled (\s a -> s {_atkgEnabled = a})

-- | The number of key groups in the list.
atkgQuantity :: Lens' ActiveTrustedKeyGroups Int
atkgQuantity = lens _atkgQuantity (\s a -> s {_atkgQuantity = a})

instance FromXML ActiveTrustedKeyGroups where
  parseXML x =
    ActiveTrustedKeyGroups'
      <$> (x .@? "Items" .!@ mempty >>= may (parseXMLList "KeyGroup"))
      <*> (x .@ "Enabled")
      <*> (x .@ "Quantity")

instance Hashable ActiveTrustedKeyGroups

instance NFData ActiveTrustedKeyGroups
