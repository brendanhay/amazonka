{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AccountAttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AccountAttributeValue where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a value of an account attribute.
--
--
--
-- /See:/ 'accountAttributeValue' smart constructor.
newtype AccountAttributeValue = AccountAttributeValue'
  { _aavAttributeValue ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccountAttributeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aavAttributeValue' - The value of the attribute.
accountAttributeValue ::
  AccountAttributeValue
accountAttributeValue =
  AccountAttributeValue' {_aavAttributeValue = Nothing}

-- | The value of the attribute.
aavAttributeValue :: Lens' AccountAttributeValue (Maybe Text)
aavAttributeValue = lens _aavAttributeValue (\s a -> s {_aavAttributeValue = a})

instance FromXML AccountAttributeValue where
  parseXML x = AccountAttributeValue' <$> (x .@? "attributeValue")

instance Hashable AccountAttributeValue

instance NFData AccountAttributeValue
