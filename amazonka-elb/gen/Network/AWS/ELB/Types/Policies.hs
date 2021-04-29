{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.AppCookieStickinessPolicy
import Network.AWS.ELB.Types.LBCookieStickinessPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The policies for a load balancer.
--
-- /See:/ 'newPolicies' smart constructor.
data Policies = Policies'
  { -- | The stickiness policies created using CreateAppCookieStickinessPolicy.
    appCookieStickinessPolicies :: Prelude.Maybe [AppCookieStickinessPolicy],
    -- | The stickiness policies created using CreateLBCookieStickinessPolicy.
    lBCookieStickinessPolicies :: Prelude.Maybe [LBCookieStickinessPolicy],
    -- | The policies other than the stickiness policies.
    otherPolicies :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      lBCookieStickinessPolicies = Prelude.Nothing,
      otherPolicies = Prelude.Nothing
    }

-- | The stickiness policies created using CreateAppCookieStickinessPolicy.
policies_appCookieStickinessPolicies :: Lens.Lens' Policies (Prelude.Maybe [AppCookieStickinessPolicy])
policies_appCookieStickinessPolicies = Lens.lens (\Policies' {appCookieStickinessPolicies} -> appCookieStickinessPolicies) (\s@Policies' {} a -> s {appCookieStickinessPolicies = a} :: Policies) Prelude.. Lens.mapping Prelude._Coerce

-- | The stickiness policies created using CreateLBCookieStickinessPolicy.
policies_lBCookieStickinessPolicies :: Lens.Lens' Policies (Prelude.Maybe [LBCookieStickinessPolicy])
policies_lBCookieStickinessPolicies = Lens.lens (\Policies' {lBCookieStickinessPolicies} -> lBCookieStickinessPolicies) (\s@Policies' {} a -> s {lBCookieStickinessPolicies = a} :: Policies) Prelude.. Lens.mapping Prelude._Coerce

-- | The policies other than the stickiness policies.
policies_otherPolicies :: Lens.Lens' Policies (Prelude.Maybe [Prelude.Text])
policies_otherPolicies = Lens.lens (\Policies' {otherPolicies} -> otherPolicies) (\s@Policies' {} a -> s {otherPolicies = a} :: Policies) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML Policies where
  parseXML x =
    Policies'
      Prelude.<$> ( x Prelude..@? "AppCookieStickinessPolicies"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> ( x Prelude..@? "LBCookieStickinessPolicies"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> ( x Prelude..@? "OtherPolicies"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )

instance Prelude.Hashable Policies

instance Prelude.NFData Policies
