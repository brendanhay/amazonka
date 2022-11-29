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
-- Module      : Amazonka.SecurityHub.Types.AwsElbLoadBalancerPolicies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElbLoadBalancerPolicies where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsElbAppCookieStickinessPolicy
import Amazonka.SecurityHub.Types.AwsElbLbCookieStickinessPolicy

-- | Contains information about the policies for a load balancer.
--
-- /See:/ 'newAwsElbLoadBalancerPolicies' smart constructor.
data AwsElbLoadBalancerPolicies = AwsElbLoadBalancerPolicies'
  { -- | The policies other than the stickiness policies.
    otherPolicies :: Prelude.Maybe [Prelude.Text],
    -- | The stickiness policies that are created using
    -- @CreateLBCookieStickinessPolicy@.
    lbCookieStickinessPolicies :: Prelude.Maybe [AwsElbLbCookieStickinessPolicy],
    -- | The stickiness policies that are created using
    -- @CreateAppCookieStickinessPolicy@.
    appCookieStickinessPolicies :: Prelude.Maybe [AwsElbAppCookieStickinessPolicy]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElbLoadBalancerPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'otherPolicies', 'awsElbLoadBalancerPolicies_otherPolicies' - The policies other than the stickiness policies.
--
-- 'lbCookieStickinessPolicies', 'awsElbLoadBalancerPolicies_lbCookieStickinessPolicies' - The stickiness policies that are created using
-- @CreateLBCookieStickinessPolicy@.
--
-- 'appCookieStickinessPolicies', 'awsElbLoadBalancerPolicies_appCookieStickinessPolicies' - The stickiness policies that are created using
-- @CreateAppCookieStickinessPolicy@.
newAwsElbLoadBalancerPolicies ::
  AwsElbLoadBalancerPolicies
newAwsElbLoadBalancerPolicies =
  AwsElbLoadBalancerPolicies'
    { otherPolicies =
        Prelude.Nothing,
      lbCookieStickinessPolicies = Prelude.Nothing,
      appCookieStickinessPolicies = Prelude.Nothing
    }

-- | The policies other than the stickiness policies.
awsElbLoadBalancerPolicies_otherPolicies :: Lens.Lens' AwsElbLoadBalancerPolicies (Prelude.Maybe [Prelude.Text])
awsElbLoadBalancerPolicies_otherPolicies = Lens.lens (\AwsElbLoadBalancerPolicies' {otherPolicies} -> otherPolicies) (\s@AwsElbLoadBalancerPolicies' {} a -> s {otherPolicies = a} :: AwsElbLoadBalancerPolicies) Prelude.. Lens.mapping Lens.coerced

-- | The stickiness policies that are created using
-- @CreateLBCookieStickinessPolicy@.
awsElbLoadBalancerPolicies_lbCookieStickinessPolicies :: Lens.Lens' AwsElbLoadBalancerPolicies (Prelude.Maybe [AwsElbLbCookieStickinessPolicy])
awsElbLoadBalancerPolicies_lbCookieStickinessPolicies = Lens.lens (\AwsElbLoadBalancerPolicies' {lbCookieStickinessPolicies} -> lbCookieStickinessPolicies) (\s@AwsElbLoadBalancerPolicies' {} a -> s {lbCookieStickinessPolicies = a} :: AwsElbLoadBalancerPolicies) Prelude.. Lens.mapping Lens.coerced

-- | The stickiness policies that are created using
-- @CreateAppCookieStickinessPolicy@.
awsElbLoadBalancerPolicies_appCookieStickinessPolicies :: Lens.Lens' AwsElbLoadBalancerPolicies (Prelude.Maybe [AwsElbAppCookieStickinessPolicy])
awsElbLoadBalancerPolicies_appCookieStickinessPolicies = Lens.lens (\AwsElbLoadBalancerPolicies' {appCookieStickinessPolicies} -> appCookieStickinessPolicies) (\s@AwsElbLoadBalancerPolicies' {} a -> s {appCookieStickinessPolicies = a} :: AwsElbLoadBalancerPolicies) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AwsElbLoadBalancerPolicies where
  parseJSON =
    Core.withObject
      "AwsElbLoadBalancerPolicies"
      ( \x ->
          AwsElbLoadBalancerPolicies'
            Prelude.<$> (x Core..:? "OtherPolicies" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "LbCookieStickinessPolicies"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "AppCookieStickinessPolicies"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AwsElbLoadBalancerPolicies where
  hashWithSalt _salt AwsElbLoadBalancerPolicies' {..} =
    _salt `Prelude.hashWithSalt` otherPolicies
      `Prelude.hashWithSalt` lbCookieStickinessPolicies
      `Prelude.hashWithSalt` appCookieStickinessPolicies

instance Prelude.NFData AwsElbLoadBalancerPolicies where
  rnf AwsElbLoadBalancerPolicies' {..} =
    Prelude.rnf otherPolicies
      `Prelude.seq` Prelude.rnf lbCookieStickinessPolicies
      `Prelude.seq` Prelude.rnf appCookieStickinessPolicies

instance Core.ToJSON AwsElbLoadBalancerPolicies where
  toJSON AwsElbLoadBalancerPolicies' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OtherPolicies" Core..=) Prelude.<$> otherPolicies,
            ("LbCookieStickinessPolicies" Core..=)
              Prelude.<$> lbCookieStickinessPolicies,
            ("AppCookieStickinessPolicies" Core..=)
              Prelude.<$> appCookieStickinessPolicies
          ]
      )
