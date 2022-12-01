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
-- Module      : Amazonka.Route53RecoveryReadiness.Types.RuleResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types.RuleResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53RecoveryReadiness.Types.Message
import Amazonka.Route53RecoveryReadiness.Types.Readiness

-- | The result of a successful Rule request, with status for an individual
-- rule.
--
-- /See:/ 'newRuleResult' smart constructor.
data RuleResult = RuleResult'
  { -- | Details about the resource\'s readiness.
    messages :: [Message],
    -- | The readiness at rule level.
    readiness :: Readiness,
    -- | The identifier of the rule.
    ruleId :: Prelude.Text,
    -- | The time the resource was last checked for readiness, in ISO-8601
    -- format, UTC.
    lastCheckedTimestamp :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messages', 'ruleResult_messages' - Details about the resource\'s readiness.
--
-- 'readiness', 'ruleResult_readiness' - The readiness at rule level.
--
-- 'ruleId', 'ruleResult_ruleId' - The identifier of the rule.
--
-- 'lastCheckedTimestamp', 'ruleResult_lastCheckedTimestamp' - The time the resource was last checked for readiness, in ISO-8601
-- format, UTC.
newRuleResult ::
  -- | 'readiness'
  Readiness ->
  -- | 'ruleId'
  Prelude.Text ->
  -- | 'lastCheckedTimestamp'
  Prelude.UTCTime ->
  RuleResult
newRuleResult
  pReadiness_
  pRuleId_
  pLastCheckedTimestamp_ =
    RuleResult'
      { messages = Prelude.mempty,
        readiness = pReadiness_,
        ruleId = pRuleId_,
        lastCheckedTimestamp =
          Core._Time Lens.# pLastCheckedTimestamp_
      }

-- | Details about the resource\'s readiness.
ruleResult_messages :: Lens.Lens' RuleResult [Message]
ruleResult_messages = Lens.lens (\RuleResult' {messages} -> messages) (\s@RuleResult' {} a -> s {messages = a} :: RuleResult) Prelude.. Lens.coerced

-- | The readiness at rule level.
ruleResult_readiness :: Lens.Lens' RuleResult Readiness
ruleResult_readiness = Lens.lens (\RuleResult' {readiness} -> readiness) (\s@RuleResult' {} a -> s {readiness = a} :: RuleResult)

-- | The identifier of the rule.
ruleResult_ruleId :: Lens.Lens' RuleResult Prelude.Text
ruleResult_ruleId = Lens.lens (\RuleResult' {ruleId} -> ruleId) (\s@RuleResult' {} a -> s {ruleId = a} :: RuleResult)

-- | The time the resource was last checked for readiness, in ISO-8601
-- format, UTC.
ruleResult_lastCheckedTimestamp :: Lens.Lens' RuleResult Prelude.UTCTime
ruleResult_lastCheckedTimestamp = Lens.lens (\RuleResult' {lastCheckedTimestamp} -> lastCheckedTimestamp) (\s@RuleResult' {} a -> s {lastCheckedTimestamp = a} :: RuleResult) Prelude.. Core._Time

instance Core.FromJSON RuleResult where
  parseJSON =
    Core.withObject
      "RuleResult"
      ( \x ->
          RuleResult'
            Prelude.<$> (x Core..:? "messages" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "readiness")
            Prelude.<*> (x Core..: "ruleId")
            Prelude.<*> (x Core..: "lastCheckedTimestamp")
      )

instance Prelude.Hashable RuleResult where
  hashWithSalt _salt RuleResult' {..} =
    _salt `Prelude.hashWithSalt` messages
      `Prelude.hashWithSalt` readiness
      `Prelude.hashWithSalt` ruleId
      `Prelude.hashWithSalt` lastCheckedTimestamp

instance Prelude.NFData RuleResult where
  rnf RuleResult' {..} =
    Prelude.rnf messages
      `Prelude.seq` Prelude.rnf readiness
      `Prelude.seq` Prelude.rnf ruleId
      `Prelude.seq` Prelude.rnf lastCheckedTimestamp
