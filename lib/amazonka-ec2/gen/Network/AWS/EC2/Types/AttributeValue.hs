{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AttributeValue where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a value for a resource attribute that is a String.
--
--
--
-- /See:/ 'attributeValue' smart constructor.
newtype AttributeValue = AttributeValue' {_avValue :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttributeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avValue' - The attribute value. The value is case-sensitive.
attributeValue ::
  AttributeValue
attributeValue = AttributeValue' {_avValue = Nothing}

-- | The attribute value. The value is case-sensitive.
avValue :: Lens' AttributeValue (Maybe Text)
avValue = lens _avValue (\s a -> s {_avValue = a})

instance FromXML AttributeValue where
  parseXML x = AttributeValue' <$> (x .@? "value")

instance Hashable AttributeValue

instance NFData AttributeValue

instance ToQuery AttributeValue where
  toQuery AttributeValue' {..} = mconcat ["Value" =: _avValue]
