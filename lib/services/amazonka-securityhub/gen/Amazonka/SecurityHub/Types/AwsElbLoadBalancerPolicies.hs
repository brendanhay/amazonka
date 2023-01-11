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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElbLoadBalancerPolicies where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsElbAppCookieStickinessPolicy
import Amazonka.SecurityHub.Types.AwsElbLbCookieStickinessPolicy

-- | Contains information about the policies for a load balancer.
--
-- /See:/ 'newAwsElbLoadBalancerPolicies' smart constructor.
data AwsElbLoadBalancerPolicies = AwsElbLoadBalancerPolicies'
  { -- | The stickiness policies that are created using
    -- @CreateAppCookieStickinessPolicy@.
    appCookieStickinessPolicies :: Prelude.Maybe [AwsElbAppCookieStickinessPolicy],
    -- | The stickiness policies that are created using
    -- @CreateLBCookieStickinessPolicy@.
    lbCookieStickinessPolicies :: Prelude.Maybe [AwsElbLbCookieStickinessPolicy],
    -- | The policies other than the stickiness policies.
    otherPolicies :: Prelude.Maybe [Prelude.Text]
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
-- 'appCookieStickinessPolicies', 'awsElbLoadBalancerPolicies_appCookieStickinessPolicies' - The stickiness policies that are created using
-- @CreateAppCookieStickinessPolicy@.
--
-- 'lbCookieStickinessPolicies', 'awsElbLoadBalancerPolicies_lbCookieStickinessPolicies' - The stickiness policies that are created using
-- @CreateLBCookieStickinessPolicy@.
--
-- 'otherPolicies', 'awsElbLoadBalancerPolicies_otherPolicies' - The policies other than the stickiness policies.
newAwsElbLoadBalancerPolicies ::
  AwsElbLoadBalancerPolicies
newAwsElbLoadBalancerPolicies =
  AwsElbLoadBalancerPolicies'
    { appCookieStickinessPolicies =
        Prelude.Nothing,
      lbCookieStickinessPolicies = Prelude.Nothing,
      otherPolicies = Prelude.Nothing
    }

-- | The stickiness policies that are created using
-- @CreateAppCookieStickinessPolicy@.
awsElbLoadBalancerPolicies_appCookieStickinessPolicies :: Lens.Lens' AwsElbLoadBalancerPolicies (Prelude.Maybe [AwsElbAppCookieStickinessPolicy])
awsElbLoadBalancerPolicies_appCookieStickinessPolicies = Lens.lens (\AwsElbLoadBalancerPolicies' {appCookieStickinessPolicies} -> appCookieStickinessPolicies) (\s@AwsElbLoadBalancerPolicies' {} a -> s {appCookieStickinessPolicies = a} :: AwsElbLoadBalancerPolicies) Prelude.. Lens.mapping Lens.coerced

-- | The stickiness policies that are created using
-- @CreateLBCookieStickinessPolicy@.
awsElbLoadBalancerPolicies_lbCookieStickinessPolicies :: Lens.Lens' AwsElbLoadBalancerPolicies (Prelude.Maybe [AwsElbLbCookieStickinessPolicy])
awsElbLoadBalancerPolicies_lbCookieStickinessPolicies = Lens.lens (\AwsElbLoadBalancerPolicies' {lbCookieStickinessPolicies} -> lbCookieStickinessPolicies) (\s@AwsElbLoadBalancerPolicies' {} a -> s {lbCookieStickinessPolicies = a} :: AwsElbLoadBalancerPolicies) Prelude.. Lens.mapping Lens.coerced

-- | The policies other than the stickiness policies.
awsElbLoadBalancerPolicies_otherPolicies :: Lens.Lens' AwsElbLoadBalancerPolicies (Prelude.Maybe [Prelude.Text])
awsElbLoadBalancerPolicies_otherPolicies = Lens.lens (\AwsElbLoadBalancerPolicies' {otherPolicies} -> otherPolicies) (\s@AwsElbLoadBalancerPolicies' {} a -> s {otherPolicies = a} :: AwsElbLoadBalancerPolicies) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AwsElbLoadBalancerPolicies where
  parseJSON =
    Data.withObject
      "AwsElbLoadBalancerPolicies"
      ( \x ->
          AwsElbLoadBalancerPolicies'
            Prelude.<$> ( x Data..:? "AppCookieStickinessPolicies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "LbCookieStickinessPolicies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "OtherPolicies" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AwsElbLoadBalancerPolicies where
  hashWithSalt _salt AwsElbLoadBalancerPolicies' {..} =
    _salt
      `Prelude.hashWithSalt` appCookieStickinessPolicies
      `Prelude.hashWithSalt` lbCookieStickinessPolicies
      `Prelude.hashWithSalt` otherPolicies

instance Prelude.NFData AwsElbLoadBalancerPolicies where
  rnf AwsElbLoadBalancerPolicies' {..} =
    Prelude.rnf appCookieStickinessPolicies
      `Prelude.seq` Prelude.rnf lbCookieStickinessPolicies
      `Prelude.seq` Prelude.rnf otherPolicies

instance Data.ToJSON AwsElbLoadBalancerPolicies where
  toJSON AwsElbLoadBalancerPolicies' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AppCookieStickinessPolicies" Data..=)
              Prelude.<$> appCookieStickinessPolicies,
            ("LbCookieStickinessPolicies" Data..=)
              Prelude.<$> lbCookieStickinessPolicies,
            ("OtherPolicies" Data..=) Prelude.<$> otherPolicies
          ]
      )
