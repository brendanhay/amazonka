{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ActiveTrustedSigners
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ActiveTrustedSigners where

import Network.AWS.CloudFront.Types.Signer
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of AWS accounts and the active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
--
--
-- /See:/ 'activeTrustedSigners' smart constructor.
data ActiveTrustedSigners = ActiveTrustedSigners'
  { _atsItems ::
      !(Maybe [Signer]),
    _atsEnabled :: !Bool,
    _atsQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActiveTrustedSigners' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atsItems' - A list of AWS accounts and the identifiers of active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
-- * 'atsEnabled' - This field is @true@ if any of the AWS accounts in the list have active CloudFront key pairs that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
--
-- * 'atsQuantity' - The number of AWS accounts in the list.
activeTrustedSigners ::
  -- | 'atsEnabled'
  Bool ->
  -- | 'atsQuantity'
  Int ->
  ActiveTrustedSigners
activeTrustedSigners pEnabled_ pQuantity_ =
  ActiveTrustedSigners'
    { _atsItems = Nothing,
      _atsEnabled = pEnabled_,
      _atsQuantity = pQuantity_
    }

-- | A list of AWS accounts and the identifiers of active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs and signed cookies.
atsItems :: Lens' ActiveTrustedSigners [Signer]
atsItems = lens _atsItems (\s a -> s {_atsItems = a}) . _Default . _Coerce

-- | This field is @true@ if any of the AWS accounts in the list have active CloudFront key pairs that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
atsEnabled :: Lens' ActiveTrustedSigners Bool
atsEnabled = lens _atsEnabled (\s a -> s {_atsEnabled = a})

-- | The number of AWS accounts in the list.
atsQuantity :: Lens' ActiveTrustedSigners Int
atsQuantity = lens _atsQuantity (\s a -> s {_atsQuantity = a})

instance FromXML ActiveTrustedSigners where
  parseXML x =
    ActiveTrustedSigners'
      <$> (x .@? "Items" .!@ mempty >>= may (parseXMLList "Signer"))
      <*> (x .@ "Enabled")
      <*> (x .@ "Quantity")

instance Hashable ActiveTrustedSigners

instance NFData ActiveTrustedSigners
