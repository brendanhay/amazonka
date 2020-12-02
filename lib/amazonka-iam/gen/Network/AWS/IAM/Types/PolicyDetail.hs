{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an IAM policy, including the policy document.
--
--
-- This data type is used as a response element in the 'GetAccountAuthorizationDetails' operation.
--
--
-- /See:/ 'policyDetail' smart constructor.
data PolicyDetail = PolicyDetail'
  { _pdPolicyDocument ::
      !(Maybe Text),
    _pdPolicyName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PolicyDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdPolicyDocument' - The policy document.
--
-- * 'pdPolicyName' - The name of the policy.
policyDetail ::
  PolicyDetail
policyDetail =
  PolicyDetail'
    { _pdPolicyDocument = Nothing,
      _pdPolicyName = Nothing
    }

-- | The policy document.
pdPolicyDocument :: Lens' PolicyDetail (Maybe Text)
pdPolicyDocument = lens _pdPolicyDocument (\s a -> s {_pdPolicyDocument = a})

-- | The name of the policy.
pdPolicyName :: Lens' PolicyDetail (Maybe Text)
pdPolicyName = lens _pdPolicyName (\s a -> s {_pdPolicyName = a})

instance FromXML PolicyDetail where
  parseXML x =
    PolicyDetail'
      <$> (x .@? "PolicyDocument") <*> (x .@? "PolicyName")

instance Hashable PolicyDetail

instance NFData PolicyDetail
