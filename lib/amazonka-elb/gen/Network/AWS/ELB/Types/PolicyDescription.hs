{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.PolicyDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.PolicyDescription where

import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.PolicyAttributeDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a policy.
--
--
--
-- /See:/ 'policyDescription' smart constructor.
data PolicyDescription = PolicyDescription'
  { _pdPolicyName ::
      !(Maybe Text),
    _pdPolicyAttributeDescriptions ::
      !(Maybe [PolicyAttributeDescription]),
    _pdPolicyTypeName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PolicyDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdPolicyName' - The name of the policy.
--
-- * 'pdPolicyAttributeDescriptions' - The policy attributes.
--
-- * 'pdPolicyTypeName' - The name of the policy type.
policyDescription ::
  PolicyDescription
policyDescription =
  PolicyDescription'
    { _pdPolicyName = Nothing,
      _pdPolicyAttributeDescriptions = Nothing,
      _pdPolicyTypeName = Nothing
    }

-- | The name of the policy.
pdPolicyName :: Lens' PolicyDescription (Maybe Text)
pdPolicyName = lens _pdPolicyName (\s a -> s {_pdPolicyName = a})

-- | The policy attributes.
pdPolicyAttributeDescriptions :: Lens' PolicyDescription [PolicyAttributeDescription]
pdPolicyAttributeDescriptions = lens _pdPolicyAttributeDescriptions (\s a -> s {_pdPolicyAttributeDescriptions = a}) . _Default . _Coerce

-- | The name of the policy type.
pdPolicyTypeName :: Lens' PolicyDescription (Maybe Text)
pdPolicyTypeName = lens _pdPolicyTypeName (\s a -> s {_pdPolicyTypeName = a})

instance FromXML PolicyDescription where
  parseXML x =
    PolicyDescription'
      <$> (x .@? "PolicyName")
      <*> ( x .@? "PolicyAttributeDescriptions" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "PolicyTypeName")

instance Hashable PolicyDescription

instance NFData PolicyDescription
