{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.TrustedSigners
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.TrustedSigners where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of AWS accounts whose public keys CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
--
--
-- /See:/ 'trustedSigners' smart constructor.
data TrustedSigners = TrustedSigners'
  { _tsItems :: !(Maybe [Text]),
    _tsEnabled :: !Bool,
    _tsQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrustedSigners' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsItems' - A list of AWS account identifiers.
--
-- * 'tsEnabled' - This field is @true@ if any of the AWS accounts have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
--
-- * 'tsQuantity' - The number of AWS accounts in the list.
trustedSigners ::
  -- | 'tsEnabled'
  Bool ->
  -- | 'tsQuantity'
  Int ->
  TrustedSigners
trustedSigners pEnabled_ pQuantity_ =
  TrustedSigners'
    { _tsItems = Nothing,
      _tsEnabled = pEnabled_,
      _tsQuantity = pQuantity_
    }

-- | A list of AWS account identifiers.
tsItems :: Lens' TrustedSigners [Text]
tsItems = lens _tsItems (\s a -> s {_tsItems = a}) . _Default . _Coerce

-- | This field is @true@ if any of the AWS accounts have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
tsEnabled :: Lens' TrustedSigners Bool
tsEnabled = lens _tsEnabled (\s a -> s {_tsEnabled = a})

-- | The number of AWS accounts in the list.
tsQuantity :: Lens' TrustedSigners Int
tsQuantity = lens _tsQuantity (\s a -> s {_tsQuantity = a})

instance FromXML TrustedSigners where
  parseXML x =
    TrustedSigners'
      <$> ( x .@? "Items" .!@ mempty
              >>= may (parseXMLList "AwsAccountNumber")
          )
      <*> (x .@ "Enabled")
      <*> (x .@ "Quantity")

instance Hashable TrustedSigners

instance NFData TrustedSigners

instance ToXML TrustedSigners where
  toXML TrustedSigners' {..} =
    mconcat
      [ "Items" @= toXML (toXMLList "AwsAccountNumber" <$> _tsItems),
        "Enabled" @= _tsEnabled,
        "Quantity" @= _tsQuantity
      ]
