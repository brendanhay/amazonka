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
-- Module      : Amazonka.SecurityHub.Types.RuleGroupSourceStatefulRulesHeaderDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.RuleGroupSourceStatefulRulesHeaderDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The inspection criteria for a stateful rule.
--
-- /See:/ 'newRuleGroupSourceStatefulRulesHeaderDetails' smart constructor.
data RuleGroupSourceStatefulRulesHeaderDetails = RuleGroupSourceStatefulRulesHeaderDetails'
  { -- | The destination IP address or address range to inspect for, in CIDR
    -- notation. To match with any address, specify @ANY@.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The destination port to inspect for. You can specify an individual port,
    -- such as @1994@. You also can specify a port range, such as @1990:1994@.
    -- To match with any port, specify @ANY@.
    destinationPort :: Prelude.Maybe Prelude.Text,
    -- | The source port to inspect for. You can specify an individual port, such
    -- as @1994@. You also can specify a port range, such as @1990:1994@. To
    -- match with any port, specify @ANY@.
    sourcePort :: Prelude.Maybe Prelude.Text,
    -- | The source IP address or address range to inspect for, in CIDR notation.
    -- To match with any address, specify @ANY@.
    source :: Prelude.Maybe Prelude.Text,
    -- | The protocol to inspect for. To inspector for all protocols, use @IP@.
    protocol :: Prelude.Maybe Prelude.Text,
    -- | The direction of traffic flow to inspect. If set to @ANY@, the
    -- inspection matches bidirectional traffic, both from the source to the
    -- destination and from the destination to the source. If set to @FORWARD@,
    -- the inspection only matches traffic going from the source to the
    -- destination.
    direction :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupSourceStatefulRulesHeaderDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'ruleGroupSourceStatefulRulesHeaderDetails_destination' - The destination IP address or address range to inspect for, in CIDR
-- notation. To match with any address, specify @ANY@.
--
-- 'destinationPort', 'ruleGroupSourceStatefulRulesHeaderDetails_destinationPort' - The destination port to inspect for. You can specify an individual port,
-- such as @1994@. You also can specify a port range, such as @1990:1994@.
-- To match with any port, specify @ANY@.
--
-- 'sourcePort', 'ruleGroupSourceStatefulRulesHeaderDetails_sourcePort' - The source port to inspect for. You can specify an individual port, such
-- as @1994@. You also can specify a port range, such as @1990:1994@. To
-- match with any port, specify @ANY@.
--
-- 'source', 'ruleGroupSourceStatefulRulesHeaderDetails_source' - The source IP address or address range to inspect for, in CIDR notation.
-- To match with any address, specify @ANY@.
--
-- 'protocol', 'ruleGroupSourceStatefulRulesHeaderDetails_protocol' - The protocol to inspect for. To inspector for all protocols, use @IP@.
--
-- 'direction', 'ruleGroupSourceStatefulRulesHeaderDetails_direction' - The direction of traffic flow to inspect. If set to @ANY@, the
-- inspection matches bidirectional traffic, both from the source to the
-- destination and from the destination to the source. If set to @FORWARD@,
-- the inspection only matches traffic going from the source to the
-- destination.
newRuleGroupSourceStatefulRulesHeaderDetails ::
  RuleGroupSourceStatefulRulesHeaderDetails
newRuleGroupSourceStatefulRulesHeaderDetails =
  RuleGroupSourceStatefulRulesHeaderDetails'
    { destination =
        Prelude.Nothing,
      destinationPort =
        Prelude.Nothing,
      sourcePort = Prelude.Nothing,
      source = Prelude.Nothing,
      protocol = Prelude.Nothing,
      direction = Prelude.Nothing
    }

-- | The destination IP address or address range to inspect for, in CIDR
-- notation. To match with any address, specify @ANY@.
ruleGroupSourceStatefulRulesHeaderDetails_destination :: Lens.Lens' RuleGroupSourceStatefulRulesHeaderDetails (Prelude.Maybe Prelude.Text)
ruleGroupSourceStatefulRulesHeaderDetails_destination = Lens.lens (\RuleGroupSourceStatefulRulesHeaderDetails' {destination} -> destination) (\s@RuleGroupSourceStatefulRulesHeaderDetails' {} a -> s {destination = a} :: RuleGroupSourceStatefulRulesHeaderDetails)

-- | The destination port to inspect for. You can specify an individual port,
-- such as @1994@. You also can specify a port range, such as @1990:1994@.
-- To match with any port, specify @ANY@.
ruleGroupSourceStatefulRulesHeaderDetails_destinationPort :: Lens.Lens' RuleGroupSourceStatefulRulesHeaderDetails (Prelude.Maybe Prelude.Text)
ruleGroupSourceStatefulRulesHeaderDetails_destinationPort = Lens.lens (\RuleGroupSourceStatefulRulesHeaderDetails' {destinationPort} -> destinationPort) (\s@RuleGroupSourceStatefulRulesHeaderDetails' {} a -> s {destinationPort = a} :: RuleGroupSourceStatefulRulesHeaderDetails)

-- | The source port to inspect for. You can specify an individual port, such
-- as @1994@. You also can specify a port range, such as @1990:1994@. To
-- match with any port, specify @ANY@.
ruleGroupSourceStatefulRulesHeaderDetails_sourcePort :: Lens.Lens' RuleGroupSourceStatefulRulesHeaderDetails (Prelude.Maybe Prelude.Text)
ruleGroupSourceStatefulRulesHeaderDetails_sourcePort = Lens.lens (\RuleGroupSourceStatefulRulesHeaderDetails' {sourcePort} -> sourcePort) (\s@RuleGroupSourceStatefulRulesHeaderDetails' {} a -> s {sourcePort = a} :: RuleGroupSourceStatefulRulesHeaderDetails)

-- | The source IP address or address range to inspect for, in CIDR notation.
-- To match with any address, specify @ANY@.
ruleGroupSourceStatefulRulesHeaderDetails_source :: Lens.Lens' RuleGroupSourceStatefulRulesHeaderDetails (Prelude.Maybe Prelude.Text)
ruleGroupSourceStatefulRulesHeaderDetails_source = Lens.lens (\RuleGroupSourceStatefulRulesHeaderDetails' {source} -> source) (\s@RuleGroupSourceStatefulRulesHeaderDetails' {} a -> s {source = a} :: RuleGroupSourceStatefulRulesHeaderDetails)

-- | The protocol to inspect for. To inspector for all protocols, use @IP@.
ruleGroupSourceStatefulRulesHeaderDetails_protocol :: Lens.Lens' RuleGroupSourceStatefulRulesHeaderDetails (Prelude.Maybe Prelude.Text)
ruleGroupSourceStatefulRulesHeaderDetails_protocol = Lens.lens (\RuleGroupSourceStatefulRulesHeaderDetails' {protocol} -> protocol) (\s@RuleGroupSourceStatefulRulesHeaderDetails' {} a -> s {protocol = a} :: RuleGroupSourceStatefulRulesHeaderDetails)

-- | The direction of traffic flow to inspect. If set to @ANY@, the
-- inspection matches bidirectional traffic, both from the source to the
-- destination and from the destination to the source. If set to @FORWARD@,
-- the inspection only matches traffic going from the source to the
-- destination.
ruleGroupSourceStatefulRulesHeaderDetails_direction :: Lens.Lens' RuleGroupSourceStatefulRulesHeaderDetails (Prelude.Maybe Prelude.Text)
ruleGroupSourceStatefulRulesHeaderDetails_direction = Lens.lens (\RuleGroupSourceStatefulRulesHeaderDetails' {direction} -> direction) (\s@RuleGroupSourceStatefulRulesHeaderDetails' {} a -> s {direction = a} :: RuleGroupSourceStatefulRulesHeaderDetails)

instance
  Data.FromJSON
    RuleGroupSourceStatefulRulesHeaderDetails
  where
  parseJSON =
    Data.withObject
      "RuleGroupSourceStatefulRulesHeaderDetails"
      ( \x ->
          RuleGroupSourceStatefulRulesHeaderDetails'
            Prelude.<$> (x Data..:? "Destination")
              Prelude.<*> (x Data..:? "DestinationPort")
              Prelude.<*> (x Data..:? "SourcePort")
              Prelude.<*> (x Data..:? "Source")
              Prelude.<*> (x Data..:? "Protocol")
              Prelude.<*> (x Data..:? "Direction")
      )

instance
  Prelude.Hashable
    RuleGroupSourceStatefulRulesHeaderDetails
  where
  hashWithSalt
    _salt
    RuleGroupSourceStatefulRulesHeaderDetails' {..} =
      _salt `Prelude.hashWithSalt` destination
        `Prelude.hashWithSalt` destinationPort
        `Prelude.hashWithSalt` sourcePort
        `Prelude.hashWithSalt` source
        `Prelude.hashWithSalt` protocol
        `Prelude.hashWithSalt` direction

instance
  Prelude.NFData
    RuleGroupSourceStatefulRulesHeaderDetails
  where
  rnf RuleGroupSourceStatefulRulesHeaderDetails' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf destinationPort
      `Prelude.seq` Prelude.rnf sourcePort
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf direction

instance
  Data.ToJSON
    RuleGroupSourceStatefulRulesHeaderDetails
  where
  toJSON RuleGroupSourceStatefulRulesHeaderDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Destination" Data..=) Prelude.<$> destination,
            ("DestinationPort" Data..=)
              Prelude.<$> destinationPort,
            ("SourcePort" Data..=) Prelude.<$> sourcePort,
            ("Source" Data..=) Prelude.<$> source,
            ("Protocol" Data..=) Prelude.<$> protocol,
            ("Direction" Data..=) Prelude.<$> direction
          ]
      )
