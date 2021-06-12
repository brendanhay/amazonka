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
-- Module      : Network.AWS.ELB.Types.AppCookieStickinessPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.AppCookieStickinessPolicy where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens

-- | Information about a policy for application-controlled session
-- stickiness.
--
-- /See:/ 'newAppCookieStickinessPolicy' smart constructor.
data AppCookieStickinessPolicy = AppCookieStickinessPolicy'
  { -- | The mnemonic name for the policy being created. The name must be unique
    -- within a set of policies for this load balancer.
    policyName :: Core.Maybe Core.Text,
    -- | The name of the application cookie used for stickiness.
    cookieName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      cookieName = Core.Nothing
    }

-- | The mnemonic name for the policy being created. The name must be unique
-- within a set of policies for this load balancer.
appCookieStickinessPolicy_policyName :: Lens.Lens' AppCookieStickinessPolicy (Core.Maybe Core.Text)
appCookieStickinessPolicy_policyName = Lens.lens (\AppCookieStickinessPolicy' {policyName} -> policyName) (\s@AppCookieStickinessPolicy' {} a -> s {policyName = a} :: AppCookieStickinessPolicy)

-- | The name of the application cookie used for stickiness.
appCookieStickinessPolicy_cookieName :: Lens.Lens' AppCookieStickinessPolicy (Core.Maybe Core.Text)
appCookieStickinessPolicy_cookieName = Lens.lens (\AppCookieStickinessPolicy' {cookieName} -> cookieName) (\s@AppCookieStickinessPolicy' {} a -> s {cookieName = a} :: AppCookieStickinessPolicy)

instance Core.FromXML AppCookieStickinessPolicy where
  parseXML x =
    AppCookieStickinessPolicy'
      Core.<$> (x Core..@? "PolicyName")
      Core.<*> (x Core..@? "CookieName")

instance Core.Hashable AppCookieStickinessPolicy

instance Core.NFData AppCookieStickinessPolicy
