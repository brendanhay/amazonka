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
-- Module      : Amazonka.ELB.Types.AppCookieStickinessPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELB.Types.AppCookieStickinessPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELB.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about a policy for application-controlled session
-- stickiness.
--
-- /See:/ 'newAppCookieStickinessPolicy' smart constructor.
data AppCookieStickinessPolicy = AppCookieStickinessPolicy'
  { -- | The mnemonic name for the policy being created. The name must be unique
    -- within a set of policies for this load balancer.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | The name of the application cookie used for stickiness.
    cookieName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppCookieStickinessPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'appCookieStickinessPolicy_policyName' - The mnemonic name for the policy being created. The name must be unique
-- within a set of policies for this load balancer.
--
-- 'cookieName', 'appCookieStickinessPolicy_cookieName' - The name of the application cookie used for stickiness.
newAppCookieStickinessPolicy ::
  AppCookieStickinessPolicy
newAppCookieStickinessPolicy =
  AppCookieStickinessPolicy'
    { policyName =
        Prelude.Nothing,
      cookieName = Prelude.Nothing
    }

-- | The mnemonic name for the policy being created. The name must be unique
-- within a set of policies for this load balancer.
appCookieStickinessPolicy_policyName :: Lens.Lens' AppCookieStickinessPolicy (Prelude.Maybe Prelude.Text)
appCookieStickinessPolicy_policyName = Lens.lens (\AppCookieStickinessPolicy' {policyName} -> policyName) (\s@AppCookieStickinessPolicy' {} a -> s {policyName = a} :: AppCookieStickinessPolicy)

-- | The name of the application cookie used for stickiness.
appCookieStickinessPolicy_cookieName :: Lens.Lens' AppCookieStickinessPolicy (Prelude.Maybe Prelude.Text)
appCookieStickinessPolicy_cookieName = Lens.lens (\AppCookieStickinessPolicy' {cookieName} -> cookieName) (\s@AppCookieStickinessPolicy' {} a -> s {cookieName = a} :: AppCookieStickinessPolicy)

instance Data.FromXML AppCookieStickinessPolicy where
  parseXML x =
    AppCookieStickinessPolicy'
      Prelude.<$> (x Data..@? "PolicyName")
      Prelude.<*> (x Data..@? "CookieName")

instance Prelude.Hashable AppCookieStickinessPolicy where
  hashWithSalt _salt AppCookieStickinessPolicy' {..} =
    _salt `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` cookieName

instance Prelude.NFData AppCookieStickinessPolicy where
  rnf AppCookieStickinessPolicy' {..} =
    Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf cookieName
