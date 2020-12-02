{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.TrustedKeyGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.TrustedKeyGroups where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of key groups whose public keys CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
--
--
-- /See:/ 'trustedKeyGroups' smart constructor.
data TrustedKeyGroups = TrustedKeyGroups'
  { _tkgItems ::
      !(Maybe [Text]),
    _tkgEnabled :: !Bool,
    _tkgQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrustedKeyGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tkgItems' - A list of key groups identifiers.
--
-- * 'tkgEnabled' - This field is @true@ if any of the key groups in the list have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
--
-- * 'tkgQuantity' - The number of key groups in the list.
trustedKeyGroups ::
  -- | 'tkgEnabled'
  Bool ->
  -- | 'tkgQuantity'
  Int ->
  TrustedKeyGroups
trustedKeyGroups pEnabled_ pQuantity_ =
  TrustedKeyGroups'
    { _tkgItems = Nothing,
      _tkgEnabled = pEnabled_,
      _tkgQuantity = pQuantity_
    }

-- | A list of key groups identifiers.
tkgItems :: Lens' TrustedKeyGroups [Text]
tkgItems = lens _tkgItems (\s a -> s {_tkgItems = a}) . _Default . _Coerce

-- | This field is @true@ if any of the key groups in the list have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
tkgEnabled :: Lens' TrustedKeyGroups Bool
tkgEnabled = lens _tkgEnabled (\s a -> s {_tkgEnabled = a})

-- | The number of key groups in the list.
tkgQuantity :: Lens' TrustedKeyGroups Int
tkgQuantity = lens _tkgQuantity (\s a -> s {_tkgQuantity = a})

instance FromXML TrustedKeyGroups where
  parseXML x =
    TrustedKeyGroups'
      <$> (x .@? "Items" .!@ mempty >>= may (parseXMLList "KeyGroup"))
      <*> (x .@ "Enabled")
      <*> (x .@ "Quantity")

instance Hashable TrustedKeyGroups

instance NFData TrustedKeyGroups

instance ToXML TrustedKeyGroups where
  toXML TrustedKeyGroups' {..} =
    mconcat
      [ "Items" @= toXML (toXMLList "KeyGroup" <$> _tkgItems),
        "Enabled" @= _tkgEnabled,
        "Quantity" @= _tkgQuantity
      ]
