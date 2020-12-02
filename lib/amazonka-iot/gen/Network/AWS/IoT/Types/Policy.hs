{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Policy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Policy where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an AWS IoT policy.
--
--
--
-- /See:/ 'policy' smart constructor.
data Policy = Policy'
  { _pPolicyName :: !(Maybe Text),
    _pPolicyARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Policy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pPolicyName' - The policy name.
--
-- * 'pPolicyARN' - The policy ARN.
policy ::
  Policy
policy = Policy' {_pPolicyName = Nothing, _pPolicyARN = Nothing}

-- | The policy name.
pPolicyName :: Lens' Policy (Maybe Text)
pPolicyName = lens _pPolicyName (\s a -> s {_pPolicyName = a})

-- | The policy ARN.
pPolicyARN :: Lens' Policy (Maybe Text)
pPolicyARN = lens _pPolicyARN (\s a -> s {_pPolicyARN = a})

instance FromJSON Policy where
  parseJSON =
    withObject
      "Policy"
      (\x -> Policy' <$> (x .:? "policyName") <*> (x .:? "policyArn"))

instance Hashable Policy

instance NFData Policy
