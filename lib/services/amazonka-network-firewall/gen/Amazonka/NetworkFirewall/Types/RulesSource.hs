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
-- Module      : Amazonka.NetworkFirewall.Types.RulesSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.RulesSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.RulesSourceList
import Amazonka.NetworkFirewall.Types.StatefulRule
import Amazonka.NetworkFirewall.Types.StatelessRulesAndCustomActions
import qualified Amazonka.Prelude as Prelude

-- | The stateless or stateful rules definitions for use in a single rule
-- group. Each rule group requires a single @RulesSource@. You can use an
-- instance of this for either stateless rules or stateful rules.
--
-- /See:/ 'newRulesSource' smart constructor.
data RulesSource = RulesSource'
  { -- | Stateful inspection criteria for a domain list rule group.
    rulesSourceList :: Prelude.Maybe RulesSourceList,
    -- | Stateful inspection criteria, provided in Suricata compatible intrusion
    -- prevention system (IPS) rules. Suricata is an open-source network IPS
    -- that includes a standard rule-based language for network traffic
    -- inspection.
    --
    -- These rules contain the inspection criteria and the action to take for
    -- traffic that matches the criteria, so this type of rule group doesn\'t
    -- have a separate action setting.
    rulesString :: Prelude.Maybe Prelude.Text,
    -- | An array of individual stateful rules inspection criteria to be used
    -- together in a stateful rule group. Use this option to specify simple
    -- Suricata rules with protocol, source and destination, ports, direction,
    -- and rule options. For information about the Suricata @Rules@ format, see
    -- <https://suricata.readthedocs.iorules/intro.html# Rules Format>.
    statefulRules :: Prelude.Maybe [StatefulRule],
    -- | Stateless inspection criteria to be used in a stateless rule group.
    statelessRulesAndCustomActions :: Prelude.Maybe StatelessRulesAndCustomActions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RulesSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rulesSourceList', 'rulesSource_rulesSourceList' - Stateful inspection criteria for a domain list rule group.
--
-- 'rulesString', 'rulesSource_rulesString' - Stateful inspection criteria, provided in Suricata compatible intrusion
-- prevention system (IPS) rules. Suricata is an open-source network IPS
-- that includes a standard rule-based language for network traffic
-- inspection.
--
-- These rules contain the inspection criteria and the action to take for
-- traffic that matches the criteria, so this type of rule group doesn\'t
-- have a separate action setting.
--
-- 'statefulRules', 'rulesSource_statefulRules' - An array of individual stateful rules inspection criteria to be used
-- together in a stateful rule group. Use this option to specify simple
-- Suricata rules with protocol, source and destination, ports, direction,
-- and rule options. For information about the Suricata @Rules@ format, see
-- <https://suricata.readthedocs.iorules/intro.html# Rules Format>.
--
-- 'statelessRulesAndCustomActions', 'rulesSource_statelessRulesAndCustomActions' - Stateless inspection criteria to be used in a stateless rule group.
newRulesSource ::
  RulesSource
newRulesSource =
  RulesSource'
    { rulesSourceList = Prelude.Nothing,
      rulesString = Prelude.Nothing,
      statefulRules = Prelude.Nothing,
      statelessRulesAndCustomActions = Prelude.Nothing
    }

-- | Stateful inspection criteria for a domain list rule group.
rulesSource_rulesSourceList :: Lens.Lens' RulesSource (Prelude.Maybe RulesSourceList)
rulesSource_rulesSourceList = Lens.lens (\RulesSource' {rulesSourceList} -> rulesSourceList) (\s@RulesSource' {} a -> s {rulesSourceList = a} :: RulesSource)

-- | Stateful inspection criteria, provided in Suricata compatible intrusion
-- prevention system (IPS) rules. Suricata is an open-source network IPS
-- that includes a standard rule-based language for network traffic
-- inspection.
--
-- These rules contain the inspection criteria and the action to take for
-- traffic that matches the criteria, so this type of rule group doesn\'t
-- have a separate action setting.
rulesSource_rulesString :: Lens.Lens' RulesSource (Prelude.Maybe Prelude.Text)
rulesSource_rulesString = Lens.lens (\RulesSource' {rulesString} -> rulesString) (\s@RulesSource' {} a -> s {rulesString = a} :: RulesSource)

-- | An array of individual stateful rules inspection criteria to be used
-- together in a stateful rule group. Use this option to specify simple
-- Suricata rules with protocol, source and destination, ports, direction,
-- and rule options. For information about the Suricata @Rules@ format, see
-- <https://suricata.readthedocs.iorules/intro.html# Rules Format>.
rulesSource_statefulRules :: Lens.Lens' RulesSource (Prelude.Maybe [StatefulRule])
rulesSource_statefulRules = Lens.lens (\RulesSource' {statefulRules} -> statefulRules) (\s@RulesSource' {} a -> s {statefulRules = a} :: RulesSource) Prelude.. Lens.mapping Lens.coerced

-- | Stateless inspection criteria to be used in a stateless rule group.
rulesSource_statelessRulesAndCustomActions :: Lens.Lens' RulesSource (Prelude.Maybe StatelessRulesAndCustomActions)
rulesSource_statelessRulesAndCustomActions = Lens.lens (\RulesSource' {statelessRulesAndCustomActions} -> statelessRulesAndCustomActions) (\s@RulesSource' {} a -> s {statelessRulesAndCustomActions = a} :: RulesSource)

instance Data.FromJSON RulesSource where
  parseJSON =
    Data.withObject
      "RulesSource"
      ( \x ->
          RulesSource'
            Prelude.<$> (x Data..:? "RulesSourceList")
            Prelude.<*> (x Data..:? "RulesString")
            Prelude.<*> (x Data..:? "StatefulRules" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "StatelessRulesAndCustomActions")
      )

instance Prelude.Hashable RulesSource where
  hashWithSalt _salt RulesSource' {..} =
    _salt
      `Prelude.hashWithSalt` rulesSourceList
      `Prelude.hashWithSalt` rulesString
      `Prelude.hashWithSalt` statefulRules
      `Prelude.hashWithSalt` statelessRulesAndCustomActions

instance Prelude.NFData RulesSource where
  rnf RulesSource' {..} =
    Prelude.rnf rulesSourceList
      `Prelude.seq` Prelude.rnf rulesString
      `Prelude.seq` Prelude.rnf statefulRules
      `Prelude.seq` Prelude.rnf statelessRulesAndCustomActions

instance Data.ToJSON RulesSource where
  toJSON RulesSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RulesSourceList" Data..=)
              Prelude.<$> rulesSourceList,
            ("RulesString" Data..=) Prelude.<$> rulesString,
            ("StatefulRules" Data..=) Prelude.<$> statefulRules,
            ("StatelessRulesAndCustomActions" Data..=)
              Prelude.<$> statelessRulesAndCustomActions
          ]
      )
