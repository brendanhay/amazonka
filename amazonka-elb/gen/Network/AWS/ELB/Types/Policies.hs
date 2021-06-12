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
-- Module      : Network.AWS.ELB.Types.Policies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.Policies where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.AppCookieStickinessPolicy
import Network.AWS.ELB.Types.LBCookieStickinessPolicy
import qualified Network.AWS.Lens as Lens

-- | The policies for a load balancer.
--
-- /See:/ 'newPolicies' smart constructor.
data Policies = Policies'
  { -- | The stickiness policies created using CreateAppCookieStickinessPolicy.
    appCookieStickinessPolicies :: Core.Maybe [AppCookieStickinessPolicy],
    -- | The stickiness policies created using CreateLBCookieStickinessPolicy.
    lBCookieStickinessPolicies :: Core.Maybe [LBCookieStickinessPolicy],
    -- | The policies other than the stickiness policies.
    otherPolicies :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Policies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appCookieStickinessPolicies', 'policies_appCookieStickinessPolicies' - The stickiness policies created using CreateAppCookieStickinessPolicy.
--
-- 'lBCookieStickinessPolicies', 'policies_lBCookieStickinessPolicies' - The stickiness policies created using CreateLBCookieStickinessPolicy.
--
-- 'otherPolicies', 'policies_otherPolicies' - The policies other than the stickiness policies.
newPolicies ::
  Policies
newPolicies =
  Policies'
    { appCookieStickinessPolicies =
        Core.Nothing,
      lBCookieStickinessPolicies = Core.Nothing,
      otherPolicies = Core.Nothing
    }

-- | The stickiness policies created using CreateAppCookieStickinessPolicy.
policies_appCookieStickinessPolicies :: Lens.Lens' Policies (Core.Maybe [AppCookieStickinessPolicy])
policies_appCookieStickinessPolicies = Lens.lens (\Policies' {appCookieStickinessPolicies} -> appCookieStickinessPolicies) (\s@Policies' {} a -> s {appCookieStickinessPolicies = a} :: Policies) Core.. Lens.mapping Lens._Coerce

-- | The stickiness policies created using CreateLBCookieStickinessPolicy.
policies_lBCookieStickinessPolicies :: Lens.Lens' Policies (Core.Maybe [LBCookieStickinessPolicy])
policies_lBCookieStickinessPolicies = Lens.lens (\Policies' {lBCookieStickinessPolicies} -> lBCookieStickinessPolicies) (\s@Policies' {} a -> s {lBCookieStickinessPolicies = a} :: Policies) Core.. Lens.mapping Lens._Coerce

-- | The policies other than the stickiness policies.
policies_otherPolicies :: Lens.Lens' Policies (Core.Maybe [Core.Text])
policies_otherPolicies = Lens.lens (\Policies' {otherPolicies} -> otherPolicies) (\s@Policies' {} a -> s {otherPolicies = a} :: Policies) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML Policies where
  parseXML x =
    Policies'
      Core.<$> ( x Core..@? "AppCookieStickinessPolicies"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> ( x Core..@? "LBCookieStickinessPolicies"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> ( x Core..@? "OtherPolicies" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )

instance Core.Hashable Policies

instance Core.NFData Policies
