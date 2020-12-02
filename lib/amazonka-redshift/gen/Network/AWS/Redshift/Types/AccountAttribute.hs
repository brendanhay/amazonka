{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.AccountAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.AccountAttribute where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.AttributeValueTarget

-- | A name value pair that describes an aspect of an account.
--
--
--
-- /See:/ 'accountAttribute' smart constructor.
data AccountAttribute = AccountAttribute'
  { _aaAttributeValues ::
      !(Maybe [AttributeValueTarget]),
    _aaAttributeName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccountAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaAttributeValues' - A list of attribute values.
--
-- * 'aaAttributeName' - The name of the attribute.
accountAttribute ::
  AccountAttribute
accountAttribute =
  AccountAttribute'
    { _aaAttributeValues = Nothing,
      _aaAttributeName = Nothing
    }

-- | A list of attribute values.
aaAttributeValues :: Lens' AccountAttribute [AttributeValueTarget]
aaAttributeValues = lens _aaAttributeValues (\s a -> s {_aaAttributeValues = a}) . _Default . _Coerce

-- | The name of the attribute.
aaAttributeName :: Lens' AccountAttribute (Maybe Text)
aaAttributeName = lens _aaAttributeName (\s a -> s {_aaAttributeName = a})

instance FromXML AccountAttribute where
  parseXML x =
    AccountAttribute'
      <$> ( x .@? "AttributeValues" .!@ mempty
              >>= may (parseXMLList "AttributeValueTarget")
          )
      <*> (x .@? "AttributeName")

instance Hashable AccountAttribute

instance NFData AccountAttribute
