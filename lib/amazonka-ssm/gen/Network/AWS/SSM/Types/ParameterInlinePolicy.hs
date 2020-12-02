{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParameterInlinePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ParameterInlinePolicy where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | One or more policies assigned to a parameter.
--
--
--
-- /See:/ 'parameterInlinePolicy' smart constructor.
data ParameterInlinePolicy = ParameterInlinePolicy'
  { _pipPolicyType ::
      !(Maybe Text),
    _pipPolicyStatus :: !(Maybe Text),
    _pipPolicyText :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParameterInlinePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pipPolicyType' - The type of policy. Parameter Store supports the following policy types: Expiration, ExpirationNotification, and NoChangeNotification.
--
-- * 'pipPolicyStatus' - The status of the policy. Policies report the following statuses: Pending (the policy has not been enforced or applied yet), Finished (the policy was applied), Failed (the policy was not applied), or InProgress (the policy is being applied now).
--
-- * 'pipPolicyText' - The JSON text of the policy.
parameterInlinePolicy ::
  ParameterInlinePolicy
parameterInlinePolicy =
  ParameterInlinePolicy'
    { _pipPolicyType = Nothing,
      _pipPolicyStatus = Nothing,
      _pipPolicyText = Nothing
    }

-- | The type of policy. Parameter Store supports the following policy types: Expiration, ExpirationNotification, and NoChangeNotification.
pipPolicyType :: Lens' ParameterInlinePolicy (Maybe Text)
pipPolicyType = lens _pipPolicyType (\s a -> s {_pipPolicyType = a})

-- | The status of the policy. Policies report the following statuses: Pending (the policy has not been enforced or applied yet), Finished (the policy was applied), Failed (the policy was not applied), or InProgress (the policy is being applied now).
pipPolicyStatus :: Lens' ParameterInlinePolicy (Maybe Text)
pipPolicyStatus = lens _pipPolicyStatus (\s a -> s {_pipPolicyStatus = a})

-- | The JSON text of the policy.
pipPolicyText :: Lens' ParameterInlinePolicy (Maybe Text)
pipPolicyText = lens _pipPolicyText (\s a -> s {_pipPolicyText = a})

instance FromJSON ParameterInlinePolicy where
  parseJSON =
    withObject
      "ParameterInlinePolicy"
      ( \x ->
          ParameterInlinePolicy'
            <$> (x .:? "PolicyType")
            <*> (x .:? "PolicyStatus")
            <*> (x .:? "PolicyText")
      )

instance Hashable ParameterInlinePolicy

instance NFData ParameterInlinePolicy
