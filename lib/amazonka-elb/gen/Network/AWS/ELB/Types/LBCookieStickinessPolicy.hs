{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.LBCookieStickinessPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.LBCookieStickinessPolicy where

import Network.AWS.ELB.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a policy for duration-based session stickiness.
--
--
--
-- /See:/ 'lBCookieStickinessPolicy' smart constructor.
data LBCookieStickinessPolicy = LBCookieStickinessPolicy'
  { _lbcspPolicyName ::
      !(Maybe Text),
    _lbcspCookieExpirationPeriod ::
      !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LBCookieStickinessPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbcspPolicyName' - The name of the policy. This name must be unique within the set of policies for this load balancer.
--
-- * 'lbcspCookieExpirationPeriod' - The time period, in seconds, after which the cookie should be considered stale. If this parameter is not specified, the stickiness session lasts for the duration of the browser session.
lBCookieStickinessPolicy ::
  LBCookieStickinessPolicy
lBCookieStickinessPolicy =
  LBCookieStickinessPolicy'
    { _lbcspPolicyName = Nothing,
      _lbcspCookieExpirationPeriod = Nothing
    }

-- | The name of the policy. This name must be unique within the set of policies for this load balancer.
lbcspPolicyName :: Lens' LBCookieStickinessPolicy (Maybe Text)
lbcspPolicyName = lens _lbcspPolicyName (\s a -> s {_lbcspPolicyName = a})

-- | The time period, in seconds, after which the cookie should be considered stale. If this parameter is not specified, the stickiness session lasts for the duration of the browser session.
lbcspCookieExpirationPeriod :: Lens' LBCookieStickinessPolicy (Maybe Integer)
lbcspCookieExpirationPeriod = lens _lbcspCookieExpirationPeriod (\s a -> s {_lbcspCookieExpirationPeriod = a})

instance FromXML LBCookieStickinessPolicy where
  parseXML x =
    LBCookieStickinessPolicy'
      <$> (x .@? "PolicyName") <*> (x .@? "CookieExpirationPeriod")

instance Hashable LBCookieStickinessPolicy

instance NFData LBCookieStickinessPolicy
