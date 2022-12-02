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
-- Module      : Amazonka.NetworkFirewall.Types.RulesSourceList
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.RulesSourceList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.GeneratedRulesType
import Amazonka.NetworkFirewall.Types.TargetType
import qualified Amazonka.Prelude as Prelude

-- | Stateful inspection criteria for a domain list rule group.
--
-- For HTTPS traffic, domain filtering is SNI-based. It uses the server
-- name indicator extension of the TLS handshake.
--
-- By default, Network Firewall domain list inspection only includes
-- traffic coming from the VPC where you deploy the firewall. To inspect
-- traffic from IP addresses outside of the deployment VPC, you set the
-- @HOME_NET@ rule variable to include the CIDR range of the deployment VPC
-- plus the other CIDR ranges. For more information, see RuleVariables in
-- this guide and
-- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/stateful-rule-groups-domain-names.html Stateful domain list rule groups in Network Firewall>
-- in the /Network Firewall Developer Guide/.
--
-- /See:/ 'newRulesSourceList' smart constructor.
data RulesSourceList = RulesSourceList'
  { -- | The domains that you want to inspect for in your traffic flows. Valid
    -- domain specifications are the following:
    --
    -- -   Explicit names. For example, @abc.example.com@ matches only the
    --     domain @abc.example.com@.
    --
    -- -   Names that use a domain wildcard, which you indicate with an initial
    --     \'@.@\'. For example,@.example.com@ matches @example.com@ and
    --     matches all subdomains of @example.com@, such as @abc.example.com@
    --     and @www.example.com@.
    targets :: [Prelude.Text],
    -- | The protocols you want to inspect. Specify @TLS_SNI@ for @HTTPS@.
    -- Specify @HTTP_HOST@ for @HTTP@. You can specify either or both.
    targetTypes :: [TargetType],
    -- | Whether you want to allow or deny access to the domains in your target
    -- list.
    generatedRulesType :: GeneratedRulesType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RulesSourceList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targets', 'rulesSourceList_targets' - The domains that you want to inspect for in your traffic flows. Valid
-- domain specifications are the following:
--
-- -   Explicit names. For example, @abc.example.com@ matches only the
--     domain @abc.example.com@.
--
-- -   Names that use a domain wildcard, which you indicate with an initial
--     \'@.@\'. For example,@.example.com@ matches @example.com@ and
--     matches all subdomains of @example.com@, such as @abc.example.com@
--     and @www.example.com@.
--
-- 'targetTypes', 'rulesSourceList_targetTypes' - The protocols you want to inspect. Specify @TLS_SNI@ for @HTTPS@.
-- Specify @HTTP_HOST@ for @HTTP@. You can specify either or both.
--
-- 'generatedRulesType', 'rulesSourceList_generatedRulesType' - Whether you want to allow or deny access to the domains in your target
-- list.
newRulesSourceList ::
  -- | 'generatedRulesType'
  GeneratedRulesType ->
  RulesSourceList
newRulesSourceList pGeneratedRulesType_ =
  RulesSourceList'
    { targets = Prelude.mempty,
      targetTypes = Prelude.mempty,
      generatedRulesType = pGeneratedRulesType_
    }

-- | The domains that you want to inspect for in your traffic flows. Valid
-- domain specifications are the following:
--
-- -   Explicit names. For example, @abc.example.com@ matches only the
--     domain @abc.example.com@.
--
-- -   Names that use a domain wildcard, which you indicate with an initial
--     \'@.@\'. For example,@.example.com@ matches @example.com@ and
--     matches all subdomains of @example.com@, such as @abc.example.com@
--     and @www.example.com@.
rulesSourceList_targets :: Lens.Lens' RulesSourceList [Prelude.Text]
rulesSourceList_targets = Lens.lens (\RulesSourceList' {targets} -> targets) (\s@RulesSourceList' {} a -> s {targets = a} :: RulesSourceList) Prelude.. Lens.coerced

-- | The protocols you want to inspect. Specify @TLS_SNI@ for @HTTPS@.
-- Specify @HTTP_HOST@ for @HTTP@. You can specify either or both.
rulesSourceList_targetTypes :: Lens.Lens' RulesSourceList [TargetType]
rulesSourceList_targetTypes = Lens.lens (\RulesSourceList' {targetTypes} -> targetTypes) (\s@RulesSourceList' {} a -> s {targetTypes = a} :: RulesSourceList) Prelude.. Lens.coerced

-- | Whether you want to allow or deny access to the domains in your target
-- list.
rulesSourceList_generatedRulesType :: Lens.Lens' RulesSourceList GeneratedRulesType
rulesSourceList_generatedRulesType = Lens.lens (\RulesSourceList' {generatedRulesType} -> generatedRulesType) (\s@RulesSourceList' {} a -> s {generatedRulesType = a} :: RulesSourceList)

instance Data.FromJSON RulesSourceList where
  parseJSON =
    Data.withObject
      "RulesSourceList"
      ( \x ->
          RulesSourceList'
            Prelude.<$> (x Data..:? "Targets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TargetTypes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "GeneratedRulesType")
      )

instance Prelude.Hashable RulesSourceList where
  hashWithSalt _salt RulesSourceList' {..} =
    _salt `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` targetTypes
      `Prelude.hashWithSalt` generatedRulesType

instance Prelude.NFData RulesSourceList where
  rnf RulesSourceList' {..} =
    Prelude.rnf targets
      `Prelude.seq` Prelude.rnf targetTypes
      `Prelude.seq` Prelude.rnf generatedRulesType

instance Data.ToJSON RulesSourceList where
  toJSON RulesSourceList' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Targets" Data..= targets),
            Prelude.Just ("TargetTypes" Data..= targetTypes),
            Prelude.Just
              ("GeneratedRulesType" Data..= generatedRulesType)
          ]
      )
