{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.AttributeValueTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.AttributeValueTarget where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- | Describes an attribute value.
--
--
--
-- /See:/ 'attributeValueTarget' smart constructor.
newtype AttributeValueTarget = AttributeValueTarget'
  { _avtAttributeValue ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttributeValueTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avtAttributeValue' - The value of the attribute.
attributeValueTarget ::
  AttributeValueTarget
attributeValueTarget =
  AttributeValueTarget' {_avtAttributeValue = Nothing}

-- | The value of the attribute.
avtAttributeValue :: Lens' AttributeValueTarget (Maybe Text)
avtAttributeValue = lens _avtAttributeValue (\s a -> s {_avtAttributeValue = a})

instance FromXML AttributeValueTarget where
  parseXML x = AttributeValueTarget' <$> (x .@? "AttributeValue")

instance Hashable AttributeValueTarget

instance NFData AttributeValueTarget
