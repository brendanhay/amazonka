{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.Policy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.Policy where

import Network.AWS.Lens
import Network.AWS.Organizations.Types.PolicySummary
import Network.AWS.Prelude

-- | Contains rules to be applied to the affected accounts. Policies can be attached directly to accounts, or to roots and OUs to affect all accounts in those hierarchies.
--
--
--
-- /See:/ 'policy' smart constructor.
data Policy = Policy'
  { _pContent :: !(Maybe Text),
    _pPolicySummary :: !(Maybe PolicySummary)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Policy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pContent' - The text content of the policy.
--
-- * 'pPolicySummary' - A structure that contains additional details about the policy.
policy ::
  Policy
policy = Policy' {_pContent = Nothing, _pPolicySummary = Nothing}

-- | The text content of the policy.
pContent :: Lens' Policy (Maybe Text)
pContent = lens _pContent (\s a -> s {_pContent = a})

-- | A structure that contains additional details about the policy.
pPolicySummary :: Lens' Policy (Maybe PolicySummary)
pPolicySummary = lens _pPolicySummary (\s a -> s {_pPolicySummary = a})

instance FromJSON Policy where
  parseJSON =
    withObject
      "Policy"
      (\x -> Policy' <$> (x .:? "Content") <*> (x .:? "PolicySummary"))

instance Hashable Policy

instance NFData Policy
