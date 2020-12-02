{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.PolicyVersionIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PolicyVersionIdentifier where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the version of the policy associated with the resource.
--
--
--
-- /See:/ 'policyVersionIdentifier' smart constructor.
data PolicyVersionIdentifier = PolicyVersionIdentifier'
  { _pviPolicyName ::
      !(Maybe Text),
    _pviPolicyVersionId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PolicyVersionIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pviPolicyName' - The name of the policy.
--
-- * 'pviPolicyVersionId' - The ID of the version of the policy associated with the resource.
policyVersionIdentifier ::
  PolicyVersionIdentifier
policyVersionIdentifier =
  PolicyVersionIdentifier'
    { _pviPolicyName = Nothing,
      _pviPolicyVersionId = Nothing
    }

-- | The name of the policy.
pviPolicyName :: Lens' PolicyVersionIdentifier (Maybe Text)
pviPolicyName = lens _pviPolicyName (\s a -> s {_pviPolicyName = a})

-- | The ID of the version of the policy associated with the resource.
pviPolicyVersionId :: Lens' PolicyVersionIdentifier (Maybe Text)
pviPolicyVersionId = lens _pviPolicyVersionId (\s a -> s {_pviPolicyVersionId = a})

instance FromJSON PolicyVersionIdentifier where
  parseJSON =
    withObject
      "PolicyVersionIdentifier"
      ( \x ->
          PolicyVersionIdentifier'
            <$> (x .:? "policyName") <*> (x .:? "policyVersionId")
      )

instance Hashable PolicyVersionIdentifier

instance NFData PolicyVersionIdentifier

instance ToJSON PolicyVersionIdentifier where
  toJSON PolicyVersionIdentifier' {..} =
    object
      ( catMaybes
          [ ("policyName" .=) <$> _pviPolicyName,
            ("policyVersionId" .=) <$> _pviPolicyVersionId
          ]
      )
