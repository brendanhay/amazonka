{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.PolicyTypeSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.PolicyTypeSummary where

import Network.AWS.Lens
import Network.AWS.Organizations.Types.PolicyType
import Network.AWS.Organizations.Types.PolicyTypeStatus
import Network.AWS.Prelude

-- | Contains information about a policy type and its status in the associated root.
--
--
--
-- /See:/ 'policyTypeSummary' smart constructor.
data PolicyTypeSummary = PolicyTypeSummary'
  { _ptsStatus ::
      !(Maybe PolicyTypeStatus),
    _ptsType :: !(Maybe PolicyType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PolicyTypeSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptsStatus' - The status of the policy type as it relates to the associated root. To attach a policy of the specified type to a root or to an OU or account in that root, it must be available in the organization and enabled for that root.
--
-- * 'ptsType' - The name of the policy type.
policyTypeSummary ::
  PolicyTypeSummary
policyTypeSummary =
  PolicyTypeSummary' {_ptsStatus = Nothing, _ptsType = Nothing}

-- | The status of the policy type as it relates to the associated root. To attach a policy of the specified type to a root or to an OU or account in that root, it must be available in the organization and enabled for that root.
ptsStatus :: Lens' PolicyTypeSummary (Maybe PolicyTypeStatus)
ptsStatus = lens _ptsStatus (\s a -> s {_ptsStatus = a})

-- | The name of the policy type.
ptsType :: Lens' PolicyTypeSummary (Maybe PolicyType)
ptsType = lens _ptsType (\s a -> s {_ptsType = a})

instance FromJSON PolicyTypeSummary where
  parseJSON =
    withObject
      "PolicyTypeSummary"
      (\x -> PolicyTypeSummary' <$> (x .:? "Status") <*> (x .:? "Type"))

instance Hashable PolicyTypeSummary

instance NFData PolicyTypeSummary
