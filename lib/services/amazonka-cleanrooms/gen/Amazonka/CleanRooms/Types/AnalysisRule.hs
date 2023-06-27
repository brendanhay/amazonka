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
-- Module      : Amazonka.CleanRooms.Types.AnalysisRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.AnalysisRule where

import Amazonka.CleanRooms.Types.AnalysisRulePolicy
import Amazonka.CleanRooms.Types.AnalysisRuleType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A specification about how data from the configured table can be used in
-- a query.
--
-- /See:/ 'newAnalysisRule' smart constructor.
data AnalysisRule = AnalysisRule'
  { -- | The unique ID for the associated collaboration.
    collaborationId :: Prelude.Text,
    -- | The type of analysis rule. Valid values are \`AGGREGATION\` and
    -- \`LIST\`.
    type' :: AnalysisRuleType,
    -- | The name for the analysis rule.
    name :: Prelude.Text,
    -- | The time the analysis rule was created.
    createTime :: Data.POSIX,
    -- | The time the analysis rule was last updated.
    updateTime :: Data.POSIX,
    -- | A policy that describes the associated data usage limitations.
    policy :: AnalysisRulePolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalysisRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collaborationId', 'analysisRule_collaborationId' - The unique ID for the associated collaboration.
--
-- 'type'', 'analysisRule_type' - The type of analysis rule. Valid values are \`AGGREGATION\` and
-- \`LIST\`.
--
-- 'name', 'analysisRule_name' - The name for the analysis rule.
--
-- 'createTime', 'analysisRule_createTime' - The time the analysis rule was created.
--
-- 'updateTime', 'analysisRule_updateTime' - The time the analysis rule was last updated.
--
-- 'policy', 'analysisRule_policy' - A policy that describes the associated data usage limitations.
newAnalysisRule ::
  -- | 'collaborationId'
  Prelude.Text ->
  -- | 'type''
  AnalysisRuleType ->
  -- | 'name'
  Prelude.Text ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  -- | 'policy'
  AnalysisRulePolicy ->
  AnalysisRule
newAnalysisRule
  pCollaborationId_
  pType_
  pName_
  pCreateTime_
  pUpdateTime_
  pPolicy_ =
    AnalysisRule'
      { collaborationId = pCollaborationId_,
        type' = pType_,
        name = pName_,
        createTime = Data._Time Lens.# pCreateTime_,
        updateTime = Data._Time Lens.# pUpdateTime_,
        policy = pPolicy_
      }

-- | The unique ID for the associated collaboration.
analysisRule_collaborationId :: Lens.Lens' AnalysisRule Prelude.Text
analysisRule_collaborationId = Lens.lens (\AnalysisRule' {collaborationId} -> collaborationId) (\s@AnalysisRule' {} a -> s {collaborationId = a} :: AnalysisRule)

-- | The type of analysis rule. Valid values are \`AGGREGATION\` and
-- \`LIST\`.
analysisRule_type :: Lens.Lens' AnalysisRule AnalysisRuleType
analysisRule_type = Lens.lens (\AnalysisRule' {type'} -> type') (\s@AnalysisRule' {} a -> s {type' = a} :: AnalysisRule)

-- | The name for the analysis rule.
analysisRule_name :: Lens.Lens' AnalysisRule Prelude.Text
analysisRule_name = Lens.lens (\AnalysisRule' {name} -> name) (\s@AnalysisRule' {} a -> s {name = a} :: AnalysisRule)

-- | The time the analysis rule was created.
analysisRule_createTime :: Lens.Lens' AnalysisRule Prelude.UTCTime
analysisRule_createTime = Lens.lens (\AnalysisRule' {createTime} -> createTime) (\s@AnalysisRule' {} a -> s {createTime = a} :: AnalysisRule) Prelude.. Data._Time

-- | The time the analysis rule was last updated.
analysisRule_updateTime :: Lens.Lens' AnalysisRule Prelude.UTCTime
analysisRule_updateTime = Lens.lens (\AnalysisRule' {updateTime} -> updateTime) (\s@AnalysisRule' {} a -> s {updateTime = a} :: AnalysisRule) Prelude.. Data._Time

-- | A policy that describes the associated data usage limitations.
analysisRule_policy :: Lens.Lens' AnalysisRule AnalysisRulePolicy
analysisRule_policy = Lens.lens (\AnalysisRule' {policy} -> policy) (\s@AnalysisRule' {} a -> s {policy = a} :: AnalysisRule)

instance Data.FromJSON AnalysisRule where
  parseJSON =
    Data.withObject
      "AnalysisRule"
      ( \x ->
          AnalysisRule'
            Prelude.<$> (x Data..: "collaborationId")
            Prelude.<*> (x Data..: "type")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "createTime")
            Prelude.<*> (x Data..: "updateTime")
            Prelude.<*> (x Data..: "policy")
      )

instance Prelude.Hashable AnalysisRule where
  hashWithSalt _salt AnalysisRule' {..} =
    _salt
      `Prelude.hashWithSalt` collaborationId
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` updateTime
      `Prelude.hashWithSalt` policy

instance Prelude.NFData AnalysisRule where
  rnf AnalysisRule' {..} =
    Prelude.rnf collaborationId
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf policy
