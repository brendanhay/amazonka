{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.LBCookieStickinessPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.LBCookieStickinessPolicy where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens

-- | Information about a policy for duration-based session stickiness.
--
-- /See:/ 'newLBCookieStickinessPolicy' smart constructor.
data LBCookieStickinessPolicy = LBCookieStickinessPolicy'
  { -- | The name of the policy. This name must be unique within the set of
    -- policies for this load balancer.
    policyName :: Core.Maybe Core.Text,
    -- | The time period, in seconds, after which the cookie should be considered
    -- stale. If this parameter is not specified, the stickiness session lasts
    -- for the duration of the browser session.
    cookieExpirationPeriod :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LBCookieStickinessPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'lBCookieStickinessPolicy_policyName' - The name of the policy. This name must be unique within the set of
-- policies for this load balancer.
--
-- 'cookieExpirationPeriod', 'lBCookieStickinessPolicy_cookieExpirationPeriod' - The time period, in seconds, after which the cookie should be considered
-- stale. If this parameter is not specified, the stickiness session lasts
-- for the duration of the browser session.
newLBCookieStickinessPolicy ::
  LBCookieStickinessPolicy
newLBCookieStickinessPolicy =
  LBCookieStickinessPolicy'
    { policyName =
        Core.Nothing,
      cookieExpirationPeriod = Core.Nothing
    }

-- | The name of the policy. This name must be unique within the set of
-- policies for this load balancer.
lBCookieStickinessPolicy_policyName :: Lens.Lens' LBCookieStickinessPolicy (Core.Maybe Core.Text)
lBCookieStickinessPolicy_policyName = Lens.lens (\LBCookieStickinessPolicy' {policyName} -> policyName) (\s@LBCookieStickinessPolicy' {} a -> s {policyName = a} :: LBCookieStickinessPolicy)

-- | The time period, in seconds, after which the cookie should be considered
-- stale. If this parameter is not specified, the stickiness session lasts
-- for the duration of the browser session.
lBCookieStickinessPolicy_cookieExpirationPeriod :: Lens.Lens' LBCookieStickinessPolicy (Core.Maybe Core.Integer)
lBCookieStickinessPolicy_cookieExpirationPeriod = Lens.lens (\LBCookieStickinessPolicy' {cookieExpirationPeriod} -> cookieExpirationPeriod) (\s@LBCookieStickinessPolicy' {} a -> s {cookieExpirationPeriod = a} :: LBCookieStickinessPolicy)

instance Core.FromXML LBCookieStickinessPolicy where
  parseXML x =
    LBCookieStickinessPolicy'
      Core.<$> (x Core..@? "PolicyName")
      Core.<*> (x Core..@? "CookieExpirationPeriod")

instance Core.Hashable LBCookieStickinessPolicy

instance Core.NFData LBCookieStickinessPolicy
