{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.Policies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.Policies where

import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.AppCookieStickinessPolicy
import Network.AWS.ELB.Types.LBCookieStickinessPolicy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The policies for a load balancer.
--
--
--
-- /See:/ 'policies' smart constructor.
data Policies = Policies'
  { _pOtherPolicies :: !(Maybe [Text]),
    _pLBCookieStickinessPolicies ::
      !(Maybe [LBCookieStickinessPolicy]),
    _pAppCookieStickinessPolicies ::
      !(Maybe [AppCookieStickinessPolicy])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Policies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pOtherPolicies' - The policies other than the stickiness policies.
--
-- * 'pLBCookieStickinessPolicies' - The stickiness policies created using 'CreateLBCookieStickinessPolicy' .
--
-- * 'pAppCookieStickinessPolicies' - The stickiness policies created using 'CreateAppCookieStickinessPolicy' .
policies ::
  Policies
policies =
  Policies'
    { _pOtherPolicies = Nothing,
      _pLBCookieStickinessPolicies = Nothing,
      _pAppCookieStickinessPolicies = Nothing
    }

-- | The policies other than the stickiness policies.
pOtherPolicies :: Lens' Policies [Text]
pOtherPolicies = lens _pOtherPolicies (\s a -> s {_pOtherPolicies = a}) . _Default . _Coerce

-- | The stickiness policies created using 'CreateLBCookieStickinessPolicy' .
pLBCookieStickinessPolicies :: Lens' Policies [LBCookieStickinessPolicy]
pLBCookieStickinessPolicies = lens _pLBCookieStickinessPolicies (\s a -> s {_pLBCookieStickinessPolicies = a}) . _Default . _Coerce

-- | The stickiness policies created using 'CreateAppCookieStickinessPolicy' .
pAppCookieStickinessPolicies :: Lens' Policies [AppCookieStickinessPolicy]
pAppCookieStickinessPolicies = lens _pAppCookieStickinessPolicies (\s a -> s {_pAppCookieStickinessPolicies = a}) . _Default . _Coerce

instance FromXML Policies where
  parseXML x =
    Policies'
      <$> (x .@? "OtherPolicies" .!@ mempty >>= may (parseXMLList "member"))
      <*> ( x .@? "LBCookieStickinessPolicies" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> ( x .@? "AppCookieStickinessPolicies" .!@ mempty
              >>= may (parseXMLList "member")
          )

instance Hashable Policies

instance NFData Policies
