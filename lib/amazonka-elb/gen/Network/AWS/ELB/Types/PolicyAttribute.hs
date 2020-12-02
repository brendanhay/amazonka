{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.PolicyAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.PolicyAttribute where

import Network.AWS.ELB.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a policy attribute.
--
--
--
-- /See:/ 'policyAttribute' smart constructor.
data PolicyAttribute = PolicyAttribute'
  { _paAttributeValue ::
      !(Maybe Text),
    _paAttributeName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PolicyAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paAttributeValue' - The value of the attribute.
--
-- * 'paAttributeName' - The name of the attribute.
policyAttribute ::
  PolicyAttribute
policyAttribute =
  PolicyAttribute'
    { _paAttributeValue = Nothing,
      _paAttributeName = Nothing
    }

-- | The value of the attribute.
paAttributeValue :: Lens' PolicyAttribute (Maybe Text)
paAttributeValue = lens _paAttributeValue (\s a -> s {_paAttributeValue = a})

-- | The name of the attribute.
paAttributeName :: Lens' PolicyAttribute (Maybe Text)
paAttributeName = lens _paAttributeName (\s a -> s {_paAttributeName = a})

instance Hashable PolicyAttribute

instance NFData PolicyAttribute

instance ToQuery PolicyAttribute where
  toQuery PolicyAttribute' {..} =
    mconcat
      [ "AttributeValue" =: _paAttributeValue,
        "AttributeName" =: _paAttributeName
      ]
