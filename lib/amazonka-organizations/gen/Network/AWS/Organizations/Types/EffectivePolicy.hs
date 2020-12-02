{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.EffectivePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.EffectivePolicy where

import Network.AWS.Lens
import Network.AWS.Organizations.Types.EffectivePolicyType
import Network.AWS.Prelude

-- | Contains rules to be applied to the affected accounts. The effective policy is the aggregation of any policies the account inherits, plus any policy directly attached to the account.
--
--
--
-- /See:/ 'effectivePolicy' smart constructor.
data EffectivePolicy = EffectivePolicy'
  { _epTargetId ::
      !(Maybe Text),
    _epPolicyType :: !(Maybe EffectivePolicyType),
    _epLastUpdatedTimestamp :: !(Maybe POSIX),
    _epPolicyContent :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EffectivePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'epTargetId' - The account ID of the policy target.
--
-- * 'epPolicyType' - The policy type.
--
-- * 'epLastUpdatedTimestamp' - The time of the last update to this policy.
--
-- * 'epPolicyContent' - The text content of the policy.
effectivePolicy ::
  EffectivePolicy
effectivePolicy =
  EffectivePolicy'
    { _epTargetId = Nothing,
      _epPolicyType = Nothing,
      _epLastUpdatedTimestamp = Nothing,
      _epPolicyContent = Nothing
    }

-- | The account ID of the policy target.
epTargetId :: Lens' EffectivePolicy (Maybe Text)
epTargetId = lens _epTargetId (\s a -> s {_epTargetId = a})

-- | The policy type.
epPolicyType :: Lens' EffectivePolicy (Maybe EffectivePolicyType)
epPolicyType = lens _epPolicyType (\s a -> s {_epPolicyType = a})

-- | The time of the last update to this policy.
epLastUpdatedTimestamp :: Lens' EffectivePolicy (Maybe UTCTime)
epLastUpdatedTimestamp = lens _epLastUpdatedTimestamp (\s a -> s {_epLastUpdatedTimestamp = a}) . mapping _Time

-- | The text content of the policy.
epPolicyContent :: Lens' EffectivePolicy (Maybe Text)
epPolicyContent = lens _epPolicyContent (\s a -> s {_epPolicyContent = a})

instance FromJSON EffectivePolicy where
  parseJSON =
    withObject
      "EffectivePolicy"
      ( \x ->
          EffectivePolicy'
            <$> (x .:? "TargetId")
            <*> (x .:? "PolicyType")
            <*> (x .:? "LastUpdatedTimestamp")
            <*> (x .:? "PolicyContent")
      )

instance Hashable EffectivePolicy

instance NFData EffectivePolicy
