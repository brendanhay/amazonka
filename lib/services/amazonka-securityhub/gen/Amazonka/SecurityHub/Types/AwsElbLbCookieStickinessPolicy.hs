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
-- Module      : Amazonka.SecurityHub.Types.AwsElbLbCookieStickinessPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElbLbCookieStickinessPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a stickiness policy that was created using
-- @CreateLBCookieStickinessPolicy@.
--
-- /See:/ 'newAwsElbLbCookieStickinessPolicy' smart constructor.
data AwsElbLbCookieStickinessPolicy = AwsElbLbCookieStickinessPolicy'
  { -- | The amount of time, in seconds, after which the cookie is considered
    -- stale. If an expiration period is not specified, the stickiness session
    -- lasts for the duration of the browser session.
    cookieExpirationPeriod :: Prelude.Maybe Prelude.Integer,
    -- | The name of the policy. The name must be unique within the set of
    -- policies for the load balancer.
    policyName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElbLbCookieStickinessPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cookieExpirationPeriod', 'awsElbLbCookieStickinessPolicy_cookieExpirationPeriod' - The amount of time, in seconds, after which the cookie is considered
-- stale. If an expiration period is not specified, the stickiness session
-- lasts for the duration of the browser session.
--
-- 'policyName', 'awsElbLbCookieStickinessPolicy_policyName' - The name of the policy. The name must be unique within the set of
-- policies for the load balancer.
newAwsElbLbCookieStickinessPolicy ::
  AwsElbLbCookieStickinessPolicy
newAwsElbLbCookieStickinessPolicy =
  AwsElbLbCookieStickinessPolicy'
    { cookieExpirationPeriod =
        Prelude.Nothing,
      policyName = Prelude.Nothing
    }

-- | The amount of time, in seconds, after which the cookie is considered
-- stale. If an expiration period is not specified, the stickiness session
-- lasts for the duration of the browser session.
awsElbLbCookieStickinessPolicy_cookieExpirationPeriod :: Lens.Lens' AwsElbLbCookieStickinessPolicy (Prelude.Maybe Prelude.Integer)
awsElbLbCookieStickinessPolicy_cookieExpirationPeriod = Lens.lens (\AwsElbLbCookieStickinessPolicy' {cookieExpirationPeriod} -> cookieExpirationPeriod) (\s@AwsElbLbCookieStickinessPolicy' {} a -> s {cookieExpirationPeriod = a} :: AwsElbLbCookieStickinessPolicy)

-- | The name of the policy. The name must be unique within the set of
-- policies for the load balancer.
awsElbLbCookieStickinessPolicy_policyName :: Lens.Lens' AwsElbLbCookieStickinessPolicy (Prelude.Maybe Prelude.Text)
awsElbLbCookieStickinessPolicy_policyName = Lens.lens (\AwsElbLbCookieStickinessPolicy' {policyName} -> policyName) (\s@AwsElbLbCookieStickinessPolicy' {} a -> s {policyName = a} :: AwsElbLbCookieStickinessPolicy)

instance Data.FromJSON AwsElbLbCookieStickinessPolicy where
  parseJSON =
    Data.withObject
      "AwsElbLbCookieStickinessPolicy"
      ( \x ->
          AwsElbLbCookieStickinessPolicy'
            Prelude.<$> (x Data..:? "CookieExpirationPeriod")
            Prelude.<*> (x Data..:? "PolicyName")
      )

instance
  Prelude.Hashable
    AwsElbLbCookieStickinessPolicy
  where
  hashWithSalt
    _salt
    AwsElbLbCookieStickinessPolicy' {..} =
      _salt `Prelude.hashWithSalt` cookieExpirationPeriod
        `Prelude.hashWithSalt` policyName

instance
  Prelude.NFData
    AwsElbLbCookieStickinessPolicy
  where
  rnf AwsElbLbCookieStickinessPolicy' {..} =
    Prelude.rnf cookieExpirationPeriod
      `Prelude.seq` Prelude.rnf policyName

instance Data.ToJSON AwsElbLbCookieStickinessPolicy where
  toJSON AwsElbLbCookieStickinessPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CookieExpirationPeriod" Data..=)
              Prelude.<$> cookieExpirationPeriod,
            ("PolicyName" Data..=) Prelude.<$> policyName
          ]
      )
