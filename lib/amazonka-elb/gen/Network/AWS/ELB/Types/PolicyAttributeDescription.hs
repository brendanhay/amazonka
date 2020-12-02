{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.PolicyAttributeDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.PolicyAttributeDescription where

import Network.AWS.ELB.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a policy attribute.
--
--
--
-- /See:/ 'policyAttributeDescription' smart constructor.
data PolicyAttributeDescription = PolicyAttributeDescription'
  { _padAttributeValue ::
      !(Maybe Text),
    _padAttributeName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PolicyAttributeDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'padAttributeValue' - The value of the attribute.
--
-- * 'padAttributeName' - The name of the attribute.
policyAttributeDescription ::
  PolicyAttributeDescription
policyAttributeDescription =
  PolicyAttributeDescription'
    { _padAttributeValue = Nothing,
      _padAttributeName = Nothing
    }

-- | The value of the attribute.
padAttributeValue :: Lens' PolicyAttributeDescription (Maybe Text)
padAttributeValue = lens _padAttributeValue (\s a -> s {_padAttributeValue = a})

-- | The name of the attribute.
padAttributeName :: Lens' PolicyAttributeDescription (Maybe Text)
padAttributeName = lens _padAttributeName (\s a -> s {_padAttributeName = a})

instance FromXML PolicyAttributeDescription where
  parseXML x =
    PolicyAttributeDescription'
      <$> (x .@? "AttributeValue") <*> (x .@? "AttributeName")

instance Hashable PolicyAttributeDescription

instance NFData PolicyAttributeDescription
