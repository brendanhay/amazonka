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
-- Module      : Amazonka.Connect.Types.RuleSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.RuleSummary where

import Amazonka.Connect.Types.ActionSummary
import Amazonka.Connect.Types.EventSourceName
import Amazonka.Connect.Types.RulePublishStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of @ActionTypes@ associated with a rule.
--
-- /See:/ 'newRuleSummary' smart constructor.
data RuleSummary = RuleSummary'
  { -- | The name of the rule.
    name :: Prelude.Text,
    -- | A unique identifier for the rule.
    ruleId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the rule.
    ruleArn :: Prelude.Text,
    -- | The name of the event source.
    eventSourceName :: EventSourceName,
    -- | The publish status of the rule.
    publishStatus :: RulePublishStatus,
    -- | A list of ActionTypes associated with a rule.
    actionSummaries :: [ActionSummary],
    -- | The timestamp for when the rule was created.
    createdTime :: Data.POSIX,
    -- | The timestamp for when the rule was last updated.
    lastUpdatedTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'ruleSummary_name' - The name of the rule.
--
-- 'ruleId', 'ruleSummary_ruleId' - A unique identifier for the rule.
--
-- 'ruleArn', 'ruleSummary_ruleArn' - The Amazon Resource Name (ARN) of the rule.
--
-- 'eventSourceName', 'ruleSummary_eventSourceName' - The name of the event source.
--
-- 'publishStatus', 'ruleSummary_publishStatus' - The publish status of the rule.
--
-- 'actionSummaries', 'ruleSummary_actionSummaries' - A list of ActionTypes associated with a rule.
--
-- 'createdTime', 'ruleSummary_createdTime' - The timestamp for when the rule was created.
--
-- 'lastUpdatedTime', 'ruleSummary_lastUpdatedTime' - The timestamp for when the rule was last updated.
newRuleSummary ::
  -- | 'name'
  Prelude.Text ->
  -- | 'ruleId'
  Prelude.Text ->
  -- | 'ruleArn'
  Prelude.Text ->
  -- | 'eventSourceName'
  EventSourceName ->
  -- | 'publishStatus'
  RulePublishStatus ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  -- | 'lastUpdatedTime'
  Prelude.UTCTime ->
  RuleSummary
newRuleSummary
  pName_
  pRuleId_
  pRuleArn_
  pEventSourceName_
  pPublishStatus_
  pCreatedTime_
  pLastUpdatedTime_ =
    RuleSummary'
      { name = pName_,
        ruleId = pRuleId_,
        ruleArn = pRuleArn_,
        eventSourceName = pEventSourceName_,
        publishStatus = pPublishStatus_,
        actionSummaries = Prelude.mempty,
        createdTime = Data._Time Lens.# pCreatedTime_,
        lastUpdatedTime =
          Data._Time Lens.# pLastUpdatedTime_
      }

-- | The name of the rule.
ruleSummary_name :: Lens.Lens' RuleSummary Prelude.Text
ruleSummary_name = Lens.lens (\RuleSummary' {name} -> name) (\s@RuleSummary' {} a -> s {name = a} :: RuleSummary)

-- | A unique identifier for the rule.
ruleSummary_ruleId :: Lens.Lens' RuleSummary Prelude.Text
ruleSummary_ruleId = Lens.lens (\RuleSummary' {ruleId} -> ruleId) (\s@RuleSummary' {} a -> s {ruleId = a} :: RuleSummary)

-- | The Amazon Resource Name (ARN) of the rule.
ruleSummary_ruleArn :: Lens.Lens' RuleSummary Prelude.Text
ruleSummary_ruleArn = Lens.lens (\RuleSummary' {ruleArn} -> ruleArn) (\s@RuleSummary' {} a -> s {ruleArn = a} :: RuleSummary)

-- | The name of the event source.
ruleSummary_eventSourceName :: Lens.Lens' RuleSummary EventSourceName
ruleSummary_eventSourceName = Lens.lens (\RuleSummary' {eventSourceName} -> eventSourceName) (\s@RuleSummary' {} a -> s {eventSourceName = a} :: RuleSummary)

-- | The publish status of the rule.
ruleSummary_publishStatus :: Lens.Lens' RuleSummary RulePublishStatus
ruleSummary_publishStatus = Lens.lens (\RuleSummary' {publishStatus} -> publishStatus) (\s@RuleSummary' {} a -> s {publishStatus = a} :: RuleSummary)

-- | A list of ActionTypes associated with a rule.
ruleSummary_actionSummaries :: Lens.Lens' RuleSummary [ActionSummary]
ruleSummary_actionSummaries = Lens.lens (\RuleSummary' {actionSummaries} -> actionSummaries) (\s@RuleSummary' {} a -> s {actionSummaries = a} :: RuleSummary) Prelude.. Lens.coerced

-- | The timestamp for when the rule was created.
ruleSummary_createdTime :: Lens.Lens' RuleSummary Prelude.UTCTime
ruleSummary_createdTime = Lens.lens (\RuleSummary' {createdTime} -> createdTime) (\s@RuleSummary' {} a -> s {createdTime = a} :: RuleSummary) Prelude.. Data._Time

-- | The timestamp for when the rule was last updated.
ruleSummary_lastUpdatedTime :: Lens.Lens' RuleSummary Prelude.UTCTime
ruleSummary_lastUpdatedTime = Lens.lens (\RuleSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@RuleSummary' {} a -> s {lastUpdatedTime = a} :: RuleSummary) Prelude.. Data._Time

instance Data.FromJSON RuleSummary where
  parseJSON =
    Data.withObject
      "RuleSummary"
      ( \x ->
          RuleSummary'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "RuleId")
            Prelude.<*> (x Data..: "RuleArn")
            Prelude.<*> (x Data..: "EventSourceName")
            Prelude.<*> (x Data..: "PublishStatus")
            Prelude.<*> ( x
                            Data..:? "ActionSummaries"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "CreatedTime")
            Prelude.<*> (x Data..: "LastUpdatedTime")
      )

instance Prelude.Hashable RuleSummary where
  hashWithSalt _salt RuleSummary' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ruleId
      `Prelude.hashWithSalt` ruleArn
      `Prelude.hashWithSalt` eventSourceName
      `Prelude.hashWithSalt` publishStatus
      `Prelude.hashWithSalt` actionSummaries
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` lastUpdatedTime

instance Prelude.NFData RuleSummary where
  rnf RuleSummary' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf ruleId `Prelude.seq`
        Prelude.rnf ruleArn `Prelude.seq`
          Prelude.rnf eventSourceName `Prelude.seq`
            Prelude.rnf publishStatus `Prelude.seq`
              Prelude.rnf actionSummaries `Prelude.seq`
                Prelude.rnf createdTime `Prelude.seq`
                  Prelude.rnf lastUpdatedTime
