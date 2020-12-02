{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pricing.Types.AttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pricing.Types.AttributeValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The values of a given attribute, such as @Throughput Optimized HDD@ or @Provisioned IOPS@ for the @Amazon EC2@ @volumeType@ attribute.
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
-- * 'avValue' - The specific value of an @attributeName@ .
attributeValue ::
  AttributeValue
attributeValue = AttributeValue' {_avValue = Nothing}

-- | The specific value of an @attributeName@ .
avValue :: Lens' AttributeValue (Maybe Text)
avValue = lens _avValue (\s a -> s {_avValue = a})

instance FromJSON AttributeValue where
  parseJSON =
    withObject
      "AttributeValue"
      (\x -> AttributeValue' <$> (x .:? "Value"))

instance Hashable AttributeValue

instance NFData AttributeValue
