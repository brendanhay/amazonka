{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AttributeBooleanValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AttributeBooleanValue where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a value for a resource attribute that is a Boolean value.
--
--
--
-- /See:/ 'attributeBooleanValue' smart constructor.
newtype AttributeBooleanValue = AttributeBooleanValue'
  { _abvValue ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttributeBooleanValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'abvValue' - The attribute value. The valid values are @true@ or @false@ .
attributeBooleanValue ::
  AttributeBooleanValue
attributeBooleanValue = AttributeBooleanValue' {_abvValue = Nothing}

-- | The attribute value. The valid values are @true@ or @false@ .
abvValue :: Lens' AttributeBooleanValue (Maybe Bool)
abvValue = lens _abvValue (\s a -> s {_abvValue = a})

instance FromXML AttributeBooleanValue where
  parseXML x = AttributeBooleanValue' <$> (x .@? "value")

instance Hashable AttributeBooleanValue

instance NFData AttributeBooleanValue

instance ToQuery AttributeBooleanValue where
  toQuery AttributeBooleanValue' {..} = mconcat ["Value" =: _abvValue]
