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
-- Module      : Amazonka.NetworkFirewall.Types.StatefulEngineOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.StatefulEngineOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.RuleOrder
import Amazonka.NetworkFirewall.Types.StreamExceptionPolicy
import qualified Amazonka.Prelude as Prelude

-- | Configuration settings for the handling of the stateful rule groups in a
-- firewall policy.
--
-- /See:/ 'newStatefulEngineOptions' smart constructor.
data StatefulEngineOptions = StatefulEngineOptions'
  { -- | Indicates how to manage the order of stateful rule evaluation for the
    -- policy. @DEFAULT_ACTION_ORDER@ is the default behavior. Stateful rules
    -- are provided to the rule engine as Suricata compatible strings, and
    -- Suricata evaluates them based on certain settings. For more information,
    -- see
    -- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/suricata-rule-evaluation-order.html Evaluation order for stateful rules>
    -- in the /Network Firewall Developer Guide/.
    ruleOrder :: Prelude.Maybe RuleOrder,
    -- | Configures how Network Firewall processes traffic when a network
    -- connection breaks midstream. Network connections can break due to
    -- disruptions in external networks or within the firewall itself.
    --
    -- -   @DROP@ - Network Firewall fails closed and drops all subsequent
    --     traffic going to the firewall. This is the default behavior.
    --
    -- -   @CONTINUE@ - Network Firewall continues to apply rules to the
    --     subsequent traffic without context from traffic before the break.
    --     This impacts the behavior of rules that depend on this context. For
    --     example, if you have a stateful rule to @drop http@ traffic, Network
    --     Firewall won\'t match the traffic for this rule because the service
    --     won\'t have the context from session initialization defining the
    --     application layer protocol as HTTP. However, this behavior is rule
    --     dependent—a TCP-layer rule using a @flow:stateless@ rule would still
    --     match, as would the @aws:drop_strict@ default action.
    --
    -- -   @REJECT@ - Network Firewall fails closed and drops all subsequent
    --     traffic going to the firewall. Network Firewall also sends a TCP
    --     reject packet back to your client so that the client can immediately
    --     establish a new session. Network Firewall will have context about
    --     the new session and will apply rules to the subsequent traffic.
    streamExceptionPolicy :: Prelude.Maybe StreamExceptionPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatefulEngineOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleOrder', 'statefulEngineOptions_ruleOrder' - Indicates how to manage the order of stateful rule evaluation for the
-- policy. @DEFAULT_ACTION_ORDER@ is the default behavior. Stateful rules
-- are provided to the rule engine as Suricata compatible strings, and
-- Suricata evaluates them based on certain settings. For more information,
-- see
-- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/suricata-rule-evaluation-order.html Evaluation order for stateful rules>
-- in the /Network Firewall Developer Guide/.
--
-- 'streamExceptionPolicy', 'statefulEngineOptions_streamExceptionPolicy' - Configures how Network Firewall processes traffic when a network
-- connection breaks midstream. Network connections can break due to
-- disruptions in external networks or within the firewall itself.
--
-- -   @DROP@ - Network Firewall fails closed and drops all subsequent
--     traffic going to the firewall. This is the default behavior.
--
-- -   @CONTINUE@ - Network Firewall continues to apply rules to the
--     subsequent traffic without context from traffic before the break.
--     This impacts the behavior of rules that depend on this context. For
--     example, if you have a stateful rule to @drop http@ traffic, Network
--     Firewall won\'t match the traffic for this rule because the service
--     won\'t have the context from session initialization defining the
--     application layer protocol as HTTP. However, this behavior is rule
--     dependent—a TCP-layer rule using a @flow:stateless@ rule would still
--     match, as would the @aws:drop_strict@ default action.
--
-- -   @REJECT@ - Network Firewall fails closed and drops all subsequent
--     traffic going to the firewall. Network Firewall also sends a TCP
--     reject packet back to your client so that the client can immediately
--     establish a new session. Network Firewall will have context about
--     the new session and will apply rules to the subsequent traffic.
newStatefulEngineOptions ::
  StatefulEngineOptions
newStatefulEngineOptions =
  StatefulEngineOptions'
    { ruleOrder = Prelude.Nothing,
      streamExceptionPolicy = Prelude.Nothing
    }

-- | Indicates how to manage the order of stateful rule evaluation for the
-- policy. @DEFAULT_ACTION_ORDER@ is the default behavior. Stateful rules
-- are provided to the rule engine as Suricata compatible strings, and
-- Suricata evaluates them based on certain settings. For more information,
-- see
-- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/suricata-rule-evaluation-order.html Evaluation order for stateful rules>
-- in the /Network Firewall Developer Guide/.
statefulEngineOptions_ruleOrder :: Lens.Lens' StatefulEngineOptions (Prelude.Maybe RuleOrder)
statefulEngineOptions_ruleOrder = Lens.lens (\StatefulEngineOptions' {ruleOrder} -> ruleOrder) (\s@StatefulEngineOptions' {} a -> s {ruleOrder = a} :: StatefulEngineOptions)

-- | Configures how Network Firewall processes traffic when a network
-- connection breaks midstream. Network connections can break due to
-- disruptions in external networks or within the firewall itself.
--
-- -   @DROP@ - Network Firewall fails closed and drops all subsequent
--     traffic going to the firewall. This is the default behavior.
--
-- -   @CONTINUE@ - Network Firewall continues to apply rules to the
--     subsequent traffic without context from traffic before the break.
--     This impacts the behavior of rules that depend on this context. For
--     example, if you have a stateful rule to @drop http@ traffic, Network
--     Firewall won\'t match the traffic for this rule because the service
--     won\'t have the context from session initialization defining the
--     application layer protocol as HTTP. However, this behavior is rule
--     dependent—a TCP-layer rule using a @flow:stateless@ rule would still
--     match, as would the @aws:drop_strict@ default action.
--
-- -   @REJECT@ - Network Firewall fails closed and drops all subsequent
--     traffic going to the firewall. Network Firewall also sends a TCP
--     reject packet back to your client so that the client can immediately
--     establish a new session. Network Firewall will have context about
--     the new session and will apply rules to the subsequent traffic.
statefulEngineOptions_streamExceptionPolicy :: Lens.Lens' StatefulEngineOptions (Prelude.Maybe StreamExceptionPolicy)
statefulEngineOptions_streamExceptionPolicy = Lens.lens (\StatefulEngineOptions' {streamExceptionPolicy} -> streamExceptionPolicy) (\s@StatefulEngineOptions' {} a -> s {streamExceptionPolicy = a} :: StatefulEngineOptions)

instance Data.FromJSON StatefulEngineOptions where
  parseJSON =
    Data.withObject
      "StatefulEngineOptions"
      ( \x ->
          StatefulEngineOptions'
            Prelude.<$> (x Data..:? "RuleOrder")
            Prelude.<*> (x Data..:? "StreamExceptionPolicy")
      )

instance Prelude.Hashable StatefulEngineOptions where
  hashWithSalt _salt StatefulEngineOptions' {..} =
    _salt
      `Prelude.hashWithSalt` ruleOrder
      `Prelude.hashWithSalt` streamExceptionPolicy

instance Prelude.NFData StatefulEngineOptions where
  rnf StatefulEngineOptions' {..} =
    Prelude.rnf ruleOrder
      `Prelude.seq` Prelude.rnf streamExceptionPolicy

instance Data.ToJSON StatefulEngineOptions where
  toJSON StatefulEngineOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RuleOrder" Data..=) Prelude.<$> ruleOrder,
            ("StreamExceptionPolicy" Data..=)
              Prelude.<$> streamExceptionPolicy
          ]
      )
