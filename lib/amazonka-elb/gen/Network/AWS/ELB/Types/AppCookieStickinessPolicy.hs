{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.AppCookieStickinessPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.AppCookieStickinessPolicy where

import Network.AWS.ELB.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a policy for application-controlled session stickiness.
--
--
--
-- /See:/ 'appCookieStickinessPolicy' smart constructor.
data AppCookieStickinessPolicy = AppCookieStickinessPolicy'
  { _acspPolicyName ::
      !(Maybe Text),
    _acspCookieName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AppCookieStickinessPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acspPolicyName' - The mnemonic name for the policy being created. The name must be unique within a set of policies for this load balancer.
--
-- * 'acspCookieName' - The name of the application cookie used for stickiness.
appCookieStickinessPolicy ::
  AppCookieStickinessPolicy
appCookieStickinessPolicy =
  AppCookieStickinessPolicy'
    { _acspPolicyName = Nothing,
      _acspCookieName = Nothing
    }

-- | The mnemonic name for the policy being created. The name must be unique within a set of policies for this load balancer.
acspPolicyName :: Lens' AppCookieStickinessPolicy (Maybe Text)
acspPolicyName = lens _acspPolicyName (\s a -> s {_acspPolicyName = a})

-- | The name of the application cookie used for stickiness.
acspCookieName :: Lens' AppCookieStickinessPolicy (Maybe Text)
acspCookieName = lens _acspCookieName (\s a -> s {_acspCookieName = a})

instance FromXML AppCookieStickinessPolicy where
  parseXML x =
    AppCookieStickinessPolicy'
      <$> (x .@? "PolicyName") <*> (x .@? "CookieName")

instance Hashable AppCookieStickinessPolicy

instance NFData AppCookieStickinessPolicy
