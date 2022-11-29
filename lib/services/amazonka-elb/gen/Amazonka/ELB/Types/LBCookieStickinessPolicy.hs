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
-- Module      : Amazonka.ELB.Types.LBCookieStickinessPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELB.Types.LBCookieStickinessPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ELB.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about a policy for duration-based session stickiness.
--
-- /See:/ 'newLBCookieStickinessPolicy' smart constructor.
data LBCookieStickinessPolicy = LBCookieStickinessPolicy'
  { -- | The name of the policy. This name must be unique within the set of
    -- policies for this load balancer.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | The time period, in seconds, after which the cookie should be considered
    -- stale. If this parameter is not specified, the stickiness session lasts
    -- for the duration of the browser session.
    cookieExpirationPeriod :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      cookieExpirationPeriod = Prelude.Nothing
    }

-- | The name of the policy. This name must be unique within the set of
-- policies for this load balancer.
lBCookieStickinessPolicy_policyName :: Lens.Lens' LBCookieStickinessPolicy (Prelude.Maybe Prelude.Text)
lBCookieStickinessPolicy_policyName = Lens.lens (\LBCookieStickinessPolicy' {policyName} -> policyName) (\s@LBCookieStickinessPolicy' {} a -> s {policyName = a} :: LBCookieStickinessPolicy)

-- | The time period, in seconds, after which the cookie should be considered
-- stale. If this parameter is not specified, the stickiness session lasts
-- for the duration of the browser session.
lBCookieStickinessPolicy_cookieExpirationPeriod :: Lens.Lens' LBCookieStickinessPolicy (Prelude.Maybe Prelude.Integer)
lBCookieStickinessPolicy_cookieExpirationPeriod = Lens.lens (\LBCookieStickinessPolicy' {cookieExpirationPeriod} -> cookieExpirationPeriod) (\s@LBCookieStickinessPolicy' {} a -> s {cookieExpirationPeriod = a} :: LBCookieStickinessPolicy)

instance Core.FromXML LBCookieStickinessPolicy where
  parseXML x =
    LBCookieStickinessPolicy'
      Prelude.<$> (x Core..@? "PolicyName")
      Prelude.<*> (x Core..@? "CookieExpirationPeriod")

instance Prelude.Hashable LBCookieStickinessPolicy where
  hashWithSalt _salt LBCookieStickinessPolicy' {..} =
    _salt `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` cookieExpirationPeriod

instance Prelude.NFData LBCookieStickinessPolicy where
  rnf LBCookieStickinessPolicy' {..} =
    Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf cookieExpirationPeriod
