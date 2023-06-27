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
-- Module      : Amazonka.CleanRooms.Types.ConfiguredTableAnalysisRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.ConfiguredTableAnalysisRule where

import Amazonka.CleanRooms.Types.ConfiguredTableAnalysisRulePolicy
import Amazonka.CleanRooms.Types.ConfiguredTableAnalysisRuleType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A configured table analysis rule, which limits how data for this table
-- can be used.
--
-- /See:/ 'newConfiguredTableAnalysisRule' smart constructor.
data ConfiguredTableAnalysisRule = ConfiguredTableAnalysisRule'
  { -- | The unique ID for the configured table.
    configuredTableId :: Prelude.Text,
    -- | The unique ARN for the configured table.
    configuredTableArn :: Prelude.Text,
    -- | The policy that controls SQL query rules.
    policy :: ConfiguredTableAnalysisRulePolicy,
    -- | The type of configured table analysis rule. Valid values are
    -- \`AGGREGATION\` and \`LIST\`.
    type' :: ConfiguredTableAnalysisRuleType,
    -- | The time the configured table analysis rule was created.
    createTime :: Data.POSIX,
    -- | The time the configured table analysis rule was last updated.
    updateTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfiguredTableAnalysisRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuredTableId', 'configuredTableAnalysisRule_configuredTableId' - The unique ID for the configured table.
--
-- 'configuredTableArn', 'configuredTableAnalysisRule_configuredTableArn' - The unique ARN for the configured table.
--
-- 'policy', 'configuredTableAnalysisRule_policy' - The policy that controls SQL query rules.
--
-- 'type'', 'configuredTableAnalysisRule_type' - The type of configured table analysis rule. Valid values are
-- \`AGGREGATION\` and \`LIST\`.
--
-- 'createTime', 'configuredTableAnalysisRule_createTime' - The time the configured table analysis rule was created.
--
-- 'updateTime', 'configuredTableAnalysisRule_updateTime' - The time the configured table analysis rule was last updated.
newConfiguredTableAnalysisRule ::
  -- | 'configuredTableId'
  Prelude.Text ->
  -- | 'configuredTableArn'
  Prelude.Text ->
  -- | 'policy'
  ConfiguredTableAnalysisRulePolicy ->
  -- | 'type''
  ConfiguredTableAnalysisRuleType ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  ConfiguredTableAnalysisRule
newConfiguredTableAnalysisRule
  pConfiguredTableId_
  pConfiguredTableArn_
  pPolicy_
  pType_
  pCreateTime_
  pUpdateTime_ =
    ConfiguredTableAnalysisRule'
      { configuredTableId =
          pConfiguredTableId_,
        configuredTableArn = pConfiguredTableArn_,
        policy = pPolicy_,
        type' = pType_,
        createTime = Data._Time Lens.# pCreateTime_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | The unique ID for the configured table.
configuredTableAnalysisRule_configuredTableId :: Lens.Lens' ConfiguredTableAnalysisRule Prelude.Text
configuredTableAnalysisRule_configuredTableId = Lens.lens (\ConfiguredTableAnalysisRule' {configuredTableId} -> configuredTableId) (\s@ConfiguredTableAnalysisRule' {} a -> s {configuredTableId = a} :: ConfiguredTableAnalysisRule)

-- | The unique ARN for the configured table.
configuredTableAnalysisRule_configuredTableArn :: Lens.Lens' ConfiguredTableAnalysisRule Prelude.Text
configuredTableAnalysisRule_configuredTableArn = Lens.lens (\ConfiguredTableAnalysisRule' {configuredTableArn} -> configuredTableArn) (\s@ConfiguredTableAnalysisRule' {} a -> s {configuredTableArn = a} :: ConfiguredTableAnalysisRule)

-- | The policy that controls SQL query rules.
configuredTableAnalysisRule_policy :: Lens.Lens' ConfiguredTableAnalysisRule ConfiguredTableAnalysisRulePolicy
configuredTableAnalysisRule_policy = Lens.lens (\ConfiguredTableAnalysisRule' {policy} -> policy) (\s@ConfiguredTableAnalysisRule' {} a -> s {policy = a} :: ConfiguredTableAnalysisRule)

-- | The type of configured table analysis rule. Valid values are
-- \`AGGREGATION\` and \`LIST\`.
configuredTableAnalysisRule_type :: Lens.Lens' ConfiguredTableAnalysisRule ConfiguredTableAnalysisRuleType
configuredTableAnalysisRule_type = Lens.lens (\ConfiguredTableAnalysisRule' {type'} -> type') (\s@ConfiguredTableAnalysisRule' {} a -> s {type' = a} :: ConfiguredTableAnalysisRule)

-- | The time the configured table analysis rule was created.
configuredTableAnalysisRule_createTime :: Lens.Lens' ConfiguredTableAnalysisRule Prelude.UTCTime
configuredTableAnalysisRule_createTime = Lens.lens (\ConfiguredTableAnalysisRule' {createTime} -> createTime) (\s@ConfiguredTableAnalysisRule' {} a -> s {createTime = a} :: ConfiguredTableAnalysisRule) Prelude.. Data._Time

-- | The time the configured table analysis rule was last updated.
configuredTableAnalysisRule_updateTime :: Lens.Lens' ConfiguredTableAnalysisRule Prelude.UTCTime
configuredTableAnalysisRule_updateTime = Lens.lens (\ConfiguredTableAnalysisRule' {updateTime} -> updateTime) (\s@ConfiguredTableAnalysisRule' {} a -> s {updateTime = a} :: ConfiguredTableAnalysisRule) Prelude.. Data._Time

instance Data.FromJSON ConfiguredTableAnalysisRule where
  parseJSON =
    Data.withObject
      "ConfiguredTableAnalysisRule"
      ( \x ->
          ConfiguredTableAnalysisRule'
            Prelude.<$> (x Data..: "configuredTableId")
            Prelude.<*> (x Data..: "configuredTableArn")
            Prelude.<*> (x Data..: "policy")
            Prelude.<*> (x Data..: "type")
            Prelude.<*> (x Data..: "createTime")
            Prelude.<*> (x Data..: "updateTime")
      )

instance Prelude.Hashable ConfiguredTableAnalysisRule where
  hashWithSalt _salt ConfiguredTableAnalysisRule' {..} =
    _salt
      `Prelude.hashWithSalt` configuredTableId
      `Prelude.hashWithSalt` configuredTableArn
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` updateTime

instance Prelude.NFData ConfiguredTableAnalysisRule where
  rnf ConfiguredTableAnalysisRule' {..} =
    Prelude.rnf configuredTableId
      `Prelude.seq` Prelude.rnf configuredTableArn
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf updateTime
