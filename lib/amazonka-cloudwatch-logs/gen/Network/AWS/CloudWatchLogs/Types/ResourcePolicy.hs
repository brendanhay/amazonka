{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.ResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.ResourcePolicy where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A policy enabling one or more entities to put logs to a log group in this account.
--
--
--
-- /See:/ 'resourcePolicy' smart constructor.
data ResourcePolicy = ResourcePolicy'
  { _rpPolicyName ::
      !(Maybe Text),
    _rpPolicyDocument :: !(Maybe Text),
    _rpLastUpdatedTime :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourcePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpPolicyName' - The name of the resource policy.
--
-- * 'rpPolicyDocument' - The details of the policy.
--
-- * 'rpLastUpdatedTime' - Timestamp showing when this policy was last updated, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
resourcePolicy ::
  ResourcePolicy
resourcePolicy =
  ResourcePolicy'
    { _rpPolicyName = Nothing,
      _rpPolicyDocument = Nothing,
      _rpLastUpdatedTime = Nothing
    }

-- | The name of the resource policy.
rpPolicyName :: Lens' ResourcePolicy (Maybe Text)
rpPolicyName = lens _rpPolicyName (\s a -> s {_rpPolicyName = a})

-- | The details of the policy.
rpPolicyDocument :: Lens' ResourcePolicy (Maybe Text)
rpPolicyDocument = lens _rpPolicyDocument (\s a -> s {_rpPolicyDocument = a})

-- | Timestamp showing when this policy was last updated, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
rpLastUpdatedTime :: Lens' ResourcePolicy (Maybe Natural)
rpLastUpdatedTime = lens _rpLastUpdatedTime (\s a -> s {_rpLastUpdatedTime = a}) . mapping _Nat

instance FromJSON ResourcePolicy where
  parseJSON =
    withObject
      "ResourcePolicy"
      ( \x ->
          ResourcePolicy'
            <$> (x .:? "policyName")
            <*> (x .:? "policyDocument")
            <*> (x .:? "lastUpdatedTime")
      )

instance Hashable ResourcePolicy

instance NFData ResourcePolicy
