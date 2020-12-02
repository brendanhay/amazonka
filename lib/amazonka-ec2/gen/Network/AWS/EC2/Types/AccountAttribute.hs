{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AccountAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AccountAttribute where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AccountAttributeValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an account attribute.
--
--
--
-- /See:/ 'accountAttribute' smart constructor.
data AccountAttribute = AccountAttribute'
  { _aaAttributeValues ::
      !(Maybe [AccountAttributeValue]),
    _aaAttributeName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccountAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaAttributeValues' - The values for the account attribute.
--
-- * 'aaAttributeName' - The name of the account attribute.
accountAttribute ::
  AccountAttribute
accountAttribute =
  AccountAttribute'
    { _aaAttributeValues = Nothing,
      _aaAttributeName = Nothing
    }

-- | The values for the account attribute.
aaAttributeValues :: Lens' AccountAttribute [AccountAttributeValue]
aaAttributeValues = lens _aaAttributeValues (\s a -> s {_aaAttributeValues = a}) . _Default . _Coerce

-- | The name of the account attribute.
aaAttributeName :: Lens' AccountAttribute (Maybe Text)
aaAttributeName = lens _aaAttributeName (\s a -> s {_aaAttributeName = a})

instance FromXML AccountAttribute where
  parseXML x =
    AccountAttribute'
      <$> ( x .@? "attributeValueSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "attributeName")

instance Hashable AccountAttribute

instance NFData AccountAttribute
