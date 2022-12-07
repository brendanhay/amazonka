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
-- Module      : Amazonka.SecurityHub.Types.AwsElbAppCookieStickinessPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElbAppCookieStickinessPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a stickiness policy that was created using
-- @CreateAppCookieStickinessPolicy@.
--
-- /See:/ 'newAwsElbAppCookieStickinessPolicy' smart constructor.
data AwsElbAppCookieStickinessPolicy = AwsElbAppCookieStickinessPolicy'
  { -- | The mnemonic name for the policy being created. The name must be unique
    -- within the set of policies for the load balancer.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | The name of the application cookie used for stickiness.
    cookieName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElbAppCookieStickinessPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'awsElbAppCookieStickinessPolicy_policyName' - The mnemonic name for the policy being created. The name must be unique
-- within the set of policies for the load balancer.
--
-- 'cookieName', 'awsElbAppCookieStickinessPolicy_cookieName' - The name of the application cookie used for stickiness.
newAwsElbAppCookieStickinessPolicy ::
  AwsElbAppCookieStickinessPolicy
newAwsElbAppCookieStickinessPolicy =
  AwsElbAppCookieStickinessPolicy'
    { policyName =
        Prelude.Nothing,
      cookieName = Prelude.Nothing
    }

-- | The mnemonic name for the policy being created. The name must be unique
-- within the set of policies for the load balancer.
awsElbAppCookieStickinessPolicy_policyName :: Lens.Lens' AwsElbAppCookieStickinessPolicy (Prelude.Maybe Prelude.Text)
awsElbAppCookieStickinessPolicy_policyName = Lens.lens (\AwsElbAppCookieStickinessPolicy' {policyName} -> policyName) (\s@AwsElbAppCookieStickinessPolicy' {} a -> s {policyName = a} :: AwsElbAppCookieStickinessPolicy)

-- | The name of the application cookie used for stickiness.
awsElbAppCookieStickinessPolicy_cookieName :: Lens.Lens' AwsElbAppCookieStickinessPolicy (Prelude.Maybe Prelude.Text)
awsElbAppCookieStickinessPolicy_cookieName = Lens.lens (\AwsElbAppCookieStickinessPolicy' {cookieName} -> cookieName) (\s@AwsElbAppCookieStickinessPolicy' {} a -> s {cookieName = a} :: AwsElbAppCookieStickinessPolicy)

instance
  Data.FromJSON
    AwsElbAppCookieStickinessPolicy
  where
  parseJSON =
    Data.withObject
      "AwsElbAppCookieStickinessPolicy"
      ( \x ->
          AwsElbAppCookieStickinessPolicy'
            Prelude.<$> (x Data..:? "PolicyName")
            Prelude.<*> (x Data..:? "CookieName")
      )

instance
  Prelude.Hashable
    AwsElbAppCookieStickinessPolicy
  where
  hashWithSalt
    _salt
    AwsElbAppCookieStickinessPolicy' {..} =
      _salt `Prelude.hashWithSalt` policyName
        `Prelude.hashWithSalt` cookieName

instance
  Prelude.NFData
    AwsElbAppCookieStickinessPolicy
  where
  rnf AwsElbAppCookieStickinessPolicy' {..} =
    Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf cookieName

instance Data.ToJSON AwsElbAppCookieStickinessPolicy where
  toJSON AwsElbAppCookieStickinessPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PolicyName" Data..=) Prelude.<$> policyName,
            ("CookieName" Data..=) Prelude.<$> cookieName
          ]
      )
