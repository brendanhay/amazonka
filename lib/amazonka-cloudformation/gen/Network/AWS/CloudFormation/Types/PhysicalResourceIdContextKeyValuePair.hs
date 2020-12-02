{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.PhysicalResourceIdContextKeyValuePair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.PhysicalResourceIdContextKeyValuePair where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Context information that enables AWS CloudFormation to uniquely identify a resource. AWS CloudFormation uses context key-value pairs in cases where a resource's logical and physical IDs are not enough to uniquely identify that resource. Each context key-value pair specifies a resource that contains the targeted resource.
--
--
--
-- /See:/ 'physicalResourceIdContextKeyValuePair' smart constructor.
data PhysicalResourceIdContextKeyValuePair = PhysicalResourceIdContextKeyValuePair'
  { _prickvpKey ::
      !Text,
    _prickvpValue ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PhysicalResourceIdContextKeyValuePair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prickvpKey' - The resource context key.
--
-- * 'prickvpValue' - The resource context value.
physicalResourceIdContextKeyValuePair ::
  -- | 'prickvpKey'
  Text ->
  -- | 'prickvpValue'
  Text ->
  PhysicalResourceIdContextKeyValuePair
physicalResourceIdContextKeyValuePair pKey_ pValue_ =
  PhysicalResourceIdContextKeyValuePair'
    { _prickvpKey = pKey_,
      _prickvpValue = pValue_
    }

-- | The resource context key.
prickvpKey :: Lens' PhysicalResourceIdContextKeyValuePair Text
prickvpKey = lens _prickvpKey (\s a -> s {_prickvpKey = a})

-- | The resource context value.
prickvpValue :: Lens' PhysicalResourceIdContextKeyValuePair Text
prickvpValue = lens _prickvpValue (\s a -> s {_prickvpValue = a})

instance FromXML PhysicalResourceIdContextKeyValuePair where
  parseXML x =
    PhysicalResourceIdContextKeyValuePair'
      <$> (x .@ "Key") <*> (x .@ "Value")

instance Hashable PhysicalResourceIdContextKeyValuePair

instance NFData PhysicalResourceIdContextKeyValuePair
